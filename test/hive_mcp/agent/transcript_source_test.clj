(ns hive-mcp.agent.transcript-source-test
  "Facets for the transcript sources: a trifecta on the pure `entry-turn`,
   and goldens over `composite` with injected TranscriptSource stubs and
   over the JSONL / Datalevin layouts in a temp directory, the Datalevin
   reader injected as its `read-dir` fn."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-dsl.result :as r]
            [hive-mcp.agent.transcript-source :as ts]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema entry-turn-trifecta
  hive-mcp.agent.transcript-source/entry-turn
  {:in          [:maybe [:map
                         [:turn {:optional true} [:maybe [:int {:min 0 :max 50}]]]
                         [:transcript/turn {:optional true} [:maybe [:int {:min 0 :max 50}]]]
                         [:role {:optional true} [:enum "user" "assistant"]]]]
   :out         :int
   :rel         (fn [e out] (= out (or (:turn e) (:transcript/turn e) 0)))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/agent/transcript-source-entry-turn.edn"})

;; read-dir-with: never open a store a writer holds (the `transcript list`
;; SIGSEGV). The store fns are injected and the opener counted.

(defn- counting-ops
  "Store fns with a counted opener, plus the probes of `store-decision`."
  [probes]
  (let [opens (atom 0)]
    {:opens opens
     :ops   (merge {:make  (fn [_] (swap! opens inc) (r/ok ::store))
                    :query (fn [_ _] (r/ok [{:turn 1}]))
                    :close (fn [_] nil)}
                   probes)}))

(def ^:private tmp-store (str (System/getProperty "java.io.tmpdir") "/tsrc-x/global/ag"))

(deftest unknown-or-held-stores-are-never-opened-test
  (doseq [[why probes err]
          [["no counts, no swarm registry" {} :transcript/store-unknown]
           ["no counts, swarm registry unreadable" {:live-writers (fn [_] nil)} :transcript/store-unknown]
           ["no counts, a running writer on the dir" {:live-writers (fn [_] [{:slave/id "ag"}])} :transcript/store-busy]
           ["legacy counts hold the dir" {:legacy-holds (fn [_] 1) :live-writers (fn [_] [])} :transcript/store-busy]
           ["hot reload: empty registry beside a live writer"
            {:registry-holds (fn [_] 0) :live-writers (fn [_] [{:slave/id "ag"}])} :transcript/store-busy]
           ["hot reload: old writer counted beside a registry entry"
            {:registry-holds (fn [_] 1) :legacy-holds (fn [_] 1)} :transcript/store-busy]]]
    (let [{:keys [opens ops]} (counting-ops probes)
          res (ts/read-dir-with ops tmp-store "ag")]
      (is (= err (:error res)) why)
      (is (zero? @opens) why))))

(deftest free-or-shared-stores-open-once-test
  (doseq [[why probes want]
          [["no running writer" {:live-writers (fn [aid] (filterv #(= aid (:slave/id %)) [{:slave/id "other"}]))} :free]
           ["registry holds the dir" {:registry-holds (fn [_] 1) :live-writers (fn [_] [{:slave/id "ag"}])} :shared]]]
    (let [{:keys [opens ops]} (counting-ops probes)]
      (is (= want (ts/store-decision ops tmp-store)) why)
      (is (= [{:turn 1}] (:ok (ts/read-dir-with ops tmp-store "ag"))) why)
      (is (= 1 @opens) why))))

(deftest a-writer-starting-before-the-open-is-caught-test
  (let [calls (atom 0)
        {:keys [opens ops]} (counting-ops {:live-writers (fn [_] (if (> (swap! calls inc) 1) [{:slave/id "ag"}] []))})]
    (is (= :transcript/store-busy (:error (ts/read-dir-with ops tmp-store "ag"))))
    (is (zero? @opens) "the re-check right before the open refuses")))

(defrecord StubSource [label listing entries]
  ts/TranscriptSource
  (list-transcripts [_] listing)
  (read-entries [_ agent-id]
    (if-let [es (get entries agent-id)]
      es
      (r/err :transcript/not-found {:agent-id agent-id :source label}))))

(deftest-golden-fn composite-golden
  "test/golden/agent/transcript-source-composite.edn"
  (fn []
    (let [a   (->StubSource :a (r/ok [{:agent-id "x" :source :a}]) {"x" (r/ok [{:turn 1}])})
          b   (->StubSource :b (r/err :boom {}) {"y" (r/ok [{:turn 2}]) "x" (r/ok [{:turn 9}])})
          bad (->StubSource :bad (r/ok [{:agent-id "z" :source :bad}]) {"z" (r/err :io/failed {:why "disk"})})
          c   (ts/composite [a b bad])]
      {:listing      (ts/list-transcripts c)
       :first-wins   (ts/read-entries c "x")
       :second       (ts/read-entries c "y")
       :error-passes (ts/read-entries c "z")
       :not-found    (ts/read-entries c "nobody")
       :empty        (ts/read-entries (ts/composite []) "x")})))

(defn- temp-dir []
  (doto (io/file (System/getProperty "java.io.tmpdir") (str "f02-transcripts-" (random-uuid))) .mkdirs))

(deftest-golden-fn on-disk-sources-golden
  "test/golden/agent/transcript-source-on-disk.edn"
  (fn []
    (let [jdir (temp-dir)
          droot (temp-dir)
          _ (spit (io/file jdir "ling-a.jsonl") "{\"turn\":1,\"role\":\"user\"}\n\n{\"turn\":2,\"role\":\"assistant\"}\n")
          _ (doseq [[proj agent] [["hive" "ling-b"] ["other" "ling-b"] ["tmp" "ling-skip"]]]
              (let [d (doto (io/file droot proj agent) .mkdirs)]
                (spit (io/file d "data.mdb") "")))
          read-dir (fn [dir agent-id]
                     (r/ok [{:transcript/turn (if (.contains (str dir) "/other/") 3 1) :agent agent-id}]))
          jsonl (ts/->JsonlSource (str jdir))
          dl    (ts/->DatalevinSource (str droot) read-dir)
          strip (fn [res] (update res :ok (fn [rows] (sort-by (juxt :agent-id :project-id)
                                                              (map #(dissoc % :modified :size-kb) rows)))))]
      {:jsonl-list     (strip (ts/list-transcripts jsonl))
       :jsonl-read     (ts/read-entries jsonl "ling-a")
       :jsonl-missing  (:error (ts/read-entries jsonl "nope"))
       :dl-list        (strip (ts/list-transcripts dl))
       :dl-read        (ts/read-entries dl "ling-b")
       :dl-skipped     (:error (ts/read-entries dl "ling-skip"))
       :dl-read-fails  (:error (ts/read-entries (ts/->DatalevinSource (str droot) (fn [_ _] (r/err :x {})))
                                                "ling-b"))})))

(deftest default-wiring-test
  (is (string? (ts/hive-agent-datalevin-root)))
  (is (satisfies? ts/TranscriptSource (ts/default-source)))
  (is (satisfies? ts/ParentIndex (ts/default-parent-index))))

(deftest row-parent-reads-both-registry-spellings
  (is (= "coordinator:1" (ts/row-parent {:slave/parent "coordinator:1"})))
  (is (= "coordinator:1" (ts/row-parent {:slave/parent {:slave/id "coordinator:1"}})))
  (is (= "coordinator:1" (ts/row-parent {:slave/parent nil :slave/parent-id "coordinator:1"}))
      "a restored row whose parent has no row keeps the parent in :slave/parent-id only")
  (is (= "coordinator:1" (ts/row-parent {:slave/parent "coordinator:1" :slave/parent-id "coordinator:2"})))
  (is (nil? (ts/row-parent {:slave/id "orphan"})))
  (is (nil? (ts/row-parent nil))))
