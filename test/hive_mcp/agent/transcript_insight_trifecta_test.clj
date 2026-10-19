(ns hive-mcp.agent.transcript-insight-trifecta-test
  "Facets for the pure transcript folds, derived from their malli contracts.
   Entries are generated in the two real shapes (JSONL and hive-agent
   Datalevin) so the properties reach the digest and matcher branches."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.transcript-insight :as ti]
            [hive-mcp.agent.transcript-source :as ts]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Text [:string {:max 20}])

(def Call
  [:map
   [:tool-call/name [:enum "bash" "edit" "read_file" "Write"]]
   [:tool-call/arguments [:enum "{\"command\":\"git status\"}" "{\"file_path\":\"/a/b\"}" "{}" "not json"]]
   [:tool-call/result [:enum "" "ok" "[main abc1234] fix it" "Ran 3 tests containing 5 assertions. 0 failures, 1 errors." "Execution error"]]])

(def DlEntry
  [:map
   [:transcript/role [:enum :assistant :tool :user :system]]
   [:transcript/content Text]
   [:transcript/turn [:int {:min 0 :max 20}]]
   [:transcript/timestamp {:optional true} [:int {:min 1000000000000 :max 1000000100000}]]
   [:transcript/tool-calls {:optional true} [:vector {:max 3} Call]]])

(def JsonlEntry
  [:map
   [:role [:enum "assistant" "tool" "user" "system"]]
   [:content Text]
   [:turn [:int {:min 0 :max 20}]]])

(def Entry [:or DlEntry JsonlEntry])
(def Entries [:vector {:max 8} Entry])

(deftrifecta-from-schema entry-content-trifecta
  hive-mcp.agent.transcript-insight/entry-content
  {:in Entry
   :out :string
   :rel (fn [e out] (= out (or (:content e) (:transcript/content e))))
   :golden-path "test/golden/agent/insight-entry-content.edn"})

(deftrifecta-from-schema ending-trifecta
  hive-mcp.agent.transcript-insight/ending
  {:in Entries
   :out [:enum "error" "text" "tool-calls" "unknown"]
   :rel (fn [es out]
          (and (or (seq es) (= "unknown" out))
               (or (not= out "error")
                   (ti/llm-error? (peek es)))))
   :golden-path "test/golden/agent/insight-ending.edn"
   :num-tests 200})

(deftrifecta-from-schema max-turn-trifecta
  hive-mcp.agent.transcript-insight/max-turn
  {:in Entries
   :out [:int {:min 0}]
   :rel (fn [es out] (and (>= out 0) (every? #(<= (or (ti/entry-turn %) 0) out) es)))
   :golden-path "test/golden/agent/insight-max-turn.edn"})

(deftrifecta-from-schema digest-trifecta
  hive-mcp.agent.transcript-insight/digest
  {:in [:cat [:string {:min 1 :max 4}] Entries]
   :out [:map [:agent-id :string] [:turns :int] [:entries :int] [:tools [:map-of :any :int]]
         [:errors [:map [:count :int]]]]
   :rel (fn [[id es] d]
          (and (= id (:agent-id d))
               (= (count es) (:entries d))
               (= (:turns d) (ti/max-turn es))
               (= (:ending d) (ti/ending es))
               (= (reduce + (vals (:commands d))) (count (:shell d)))))
   :golden-path "test/golden/agent/insight-digest.edn"
   :num-tests 200})

(deftrifecta-from-schema classify-command-trifecta
  hive-mcp.agent.transcript-insight/classify-command
  {:in [:enum "clojure -M:test" "git commit -m x" "make build" "ls" "bb test" "npm run build" nil]
   :out [:enum "test" "git" "build" "other"]
   :rel (fn [c out] (or (not= out "test") (some? c)))
   :golden-path "test/golden/agent/insight-classify-command.edn"})

(deftrifecta-from-schema clip-trifecta
  hive-mcp.agent.transcript-insight/clip
  {:in [:cat [:string {:max 30}] [:int {:min 0 :max 40}]]
   :out :string
   :rel (fn [[s n] out] (and (str/starts-with? s out) (= (count out) (min n (count s)))))
   :golden-path "test/golden/agent/insight-clip.edn"})

(deftest parse-since-arms
  (let [now 1000000000000]
    (testing "relative spans"
      (is (= (- now 1800000) (ti/parse-since "30m" now)))
      (is (= (- now 604800000) (ti/parse-since "1w" now))))
    (testing "epoch ms and ISO"
      (is (= 1790000000000 (ti/parse-since "1790000000000" now)))
      (is (= 1788048000000 (ti/parse-since "2026-08-30" now)))
      (is (= 0 (ti/parse-since "1970-01-01T00:00:00Z" now))))
    (testing "blank is nil, junk is an error map"
      (is (nil? (ti/parse-since nil now)))
      (is (nil? (ti/parse-since "  " now)))
      (is (string? (:error (ti/parse-since "soon" now)))))
    (is (= "1970-01-01T00:00:00Z" (ti/iso 0)))
    (is (nil? (ti/iso nil)))))

(deftest needle-arms-validate-their-own-schema
  (let [sub (ti/compile-needle "Foo.bar") rx (ti/compile-needle "/fo+/i") bad (ti/compile-needle "/(/") blank (ti/compile-needle "")]
    (is (= :substring (:kind sub)))
    (is (= :regex (:kind rx)))
    (is (m/validate ti/Needle sub))
    (is (m/validate ti/Needle rx))
    (is (string? (:error bad)))
    (is (string? (:error blank)))
    (is (m/validate ti/Needle bad))
    (is (some? (re-find (:pattern sub) "xFOO.BARx")) "substring is quoted and case-insensitive")
    (is (nil? (re-find (:pattern sub) "fooXbar")))))

(deftest find-hits-and-segments
  (let [es [{:role "assistant" :content "ran the tests" :turn 1
             :tool_calls [{:function {:name "bash" :arguments "{\"command\":\"make test\"}"}}]}
            {:role "tool" :content "all green" :turn 2 :name "bash"}]
        needle (ti/compile-needle "green")]
    (is (= [{:agent "a" :run "r" :turn 2 :role "tool" :snippet "all green" :tool "bash"}]
           (vec (ti/find-hits {:agent "a" :run "r"} es needle {}))))
    (is (empty? (ti/find-hits {} es needle {:role "assistant"})))
    (is (= 2 (count (ti/entry-segments (first es)))))
    (is (= "…cd…" (ti/snippet "abcdef" 2 4 0)))))

(golden/deftest-golden-fn insight-fold-golden
  "test/golden/agent/insight-fold.edn"
  (fn []
    (let [es [{:transcript/role :assistant :transcript/content "start" :transcript/turn 1
               :transcript/timestamp 1000
               :transcript/tool-calls [{:tool-call/name "bash"
                                        :tool-call/arguments "{\"command\":\"git commit -m x\"}"
                                        :tool-call/result "[main abc1234] fix it"}
                                       {:tool-call/name "bash"
                                        :tool-call/arguments "{\"command\":\"clojure -M:test\"}"
                                        :tool-call/result "Ran 3 tests containing 5 assertions. 0 failures, 1 errors."}
                                       {:tool-call/name "Write"
                                        :tool-call/arguments "{\"file_path\":\"/a/b\"}"
                                        :tool-call/result ""}]}
              {:transcript/role :system :transcript/content "LLM error: boom" :transcript/turn 2
               :transcript/timestamp 61000}
              {:transcript/role :assistant :transcript/content "done" :transcript/turn 3
               :transcript/timestamp 62000}]
          d (ti/digest "ling-1" es)]
      {:digest d
       :row (ti/digest-row d)
       :report (ti/final-report "ling-1" es)
       :last-tool (ti/last-tool-result es)
       :idle (ti/idle-gap es)
       :listing (ti/select-listing
                 (ti/collapse-listing [{:agent-id "ling-a" :modified 5 :source :jsonl}
                                       {:agent-id "ling-a" :modified 9 :source :datalevin}
                                       {:agent-id "other" :modified 7 :source :jsonl}])
                 {:agent "ling-*" :limit 5})})))

(deftest source-entry-turn
  (is (= 0 (ts/entry-turn {})))
  (is (= 4 (ts/entry-turn {:turn 4})))
  (is (= 5 (ts/entry-turn {:transcript/turn 5}))))

(deftest-contract-coverage transcript-contract-coverage
  {:paths ["src/hive_mcp/agent/transcript_insight.clj"
           "src/hive_mcp/agent/transcript_source.clj"]
   :exempt {}})
