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
