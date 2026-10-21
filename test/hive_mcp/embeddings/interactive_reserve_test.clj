(ns hive-mcp.embeddings.interactive-reserve-test
  "A bulk embedding flood cannot starve an interactive embed.

   The stub embedder serializes every call behind one monitor and charges a
   fixed cost per text, the way an ollama with NUM_PARALLEL=1 does. Bulk work
   enters through `bulk/embed-batch!` (the addon :embedding/embed-batch port),
   which chunks it and runs it in the :batch lane; the interactive call keeps a
   reserved permit and only ever queues behind one short chunk.

   Kanban 20261020223606-16e6e1ef."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.bulk :as bulk]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.resilient :as resilient]
            [hive-mcp.embeddings.shared-gate :as shared]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- serial-embedder
  "One call at a time, `per-text-ms` per text. Records [lane n-texts] per call."
  [per-text-ms calls]
  (let [lock (Object.)
        work (fn [n]
               (locking lock
                 (swap! calls conj [(shared/current-lane) n])
                 (Thread/sleep (long (* per-text-ms n)))))]
    (reify proto/EmbeddingProvider
      (embed-text [_ _] (work 1) [1.0 0.0])
      (embed-batch [_ texts] (work (count texts)) (mapv (constantly [0.0 1.0]) texts))
      (embedding-dimension [_] 2))))

(defn- chain-over
  "A resilient embedder over `raw`, admitted by `gate`, with its own health."
  [raw gate total-budget-ms]
  (resilient/resilient-embedder
   [{:provider (shared/gated-provider raw gate) :provider-key :stub}]
   {:budget-ms total-budget-ms :cold-budget-ms total-budget-ms
    :total-budget-ms total-budget-ms
    :warmth (atom #{}) :health (atom {}) :cooldown-ms 30000}))

(deftest reserved-permit-admits-interactive-past-a-held-batch
  (testing "with a reserve, an interactive caller is admitted while batch holds the permit"
    (let [gate (shared/new-gate 1 1000 {:interactive-reserve 1})]
      (is (true? (shared/try-acquire! gate :batch 50)))
      (is (true? (shared/try-acquire! gate :interactive 50)))
      (is (false? (shared/try-acquire! gate :batch 50))
          "batch never takes the reserved permit")
      (shared/release! gate)
      (shared/release! gate)))
  (testing "without a reserve the old one-permit behaviour is unchanged"
    (let [gate (shared/new-gate 1 1000)]
      (is (true? (shared/try-acquire! gate :batch 50)))
      (is (false? (shared/try-acquire! gate :interactive 50)))
      (shared/release! gate))))

(deftest process-gate-reserves-an-interactive-permit
  (is (pos? (:interactive-reserve shared/process-gate 0))
      "the process gate ships with an interactive reserve by default"))

(deftest reserve-is-read-from-config
  (is (= 1 (shared/reserve-from-config {})))
  (is (= 0 (shared/reserve-from-config {:embeddings {:gate {:interactive-reserve 0}}})))
  (is (= 2 (shared/reserve-from-config {:embeddings {:gate {:interactive-reserve 2}}})))
  (is (= 1 (shared/reserve-from-config {:embeddings {:gate {:interactive-reserve -3}}}))))

(deftest bulk-embed-chunks-and-runs-in-the-batch-lane
  (let [calls (atom [])
        raw (serial-embedder 0 calls)
        texts (mapv str (range 19))
        out (shared/with-lane :interactive
              (bulk/embed-batch! raw texts {:max-texts 8}))]
    (is (= 19 (count out)) "one vector per text, positionally")
    (is (= [8 8 3] (mapv second @calls)) "no call exceeds the cap")
    (is (every? #{:batch} (map first @calls))
        "bulk work never competes in the interactive lane, whatever the caller's lane")))

(deftest bulk-cap-is-read-from-config
  (is (= bulk/default-max-texts (bulk/max-texts-from-config {})))
  (is (= 4 (bulk/max-texts-from-config {:embeddings {:batch {:max-texts 4}}})))
  (is (= bulk/default-max-texts
         (bulk/max-texts-from-config {:embeddings {:batch {:max-texts 0}}}))))

(deftest bulk-flood-leaves-interactive-within-budget
  ;; 64 texts x 10 ms = 640 ms if sent as one call; the interactive budget is
  ;; 400 ms. Chunked at 8 texts a call is 80 ms, so an interactive call waits
  ;; at most one chunk at the stub and is admitted through the reserve.
  (let [calls (atom [])
        raw (serial-embedder 10 calls)
        gate (shared/new-gate 1 5000 {:interactive-reserve 1})
        bulk-port (chain-over raw gate 5000)
        interactive-port (chain-over raw gate 400)
        texts (mapv str (range 64))
        flood (doall (for [_ (range 3)]
                       (future (bulk/embed-batch! bulk-port texts {:max-texts 8}))))]
    (try
      (is (shared/await-waiters gate :batch 1 2000) "the flood is queued at the gate")
      (dotimes [_ 3]
        (let [t0 (System/nanoTime)
              v (shared/with-lane :interactive (proto/embed-text interactive-port "q"))
              ms (/ (- (System/nanoTime) t0) 1e6)]
          (is (= [1.0 0.0] v))
          (is (< ms 400.0) (str "interactive embed took " ms " ms under a bulk flood"))))
      (is (every? #(= 64 (count (deref % 20000 nil))) flood) "the flood still completes")
      (is (every? #(<= % 8) (map second (filter (comp #{:batch} first) @calls))))
      (finally (run! future-cancel flood)))))
