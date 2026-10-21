(ns hive-mcp.tools.consolidated.batch-migration-trifecta-test
  "[MKG-P2.2] memory batch-* and agent batch-spawn run through the Batchable
   runners (`memory.batch/run-batch`, `swarm.batch/run-batch`) behind
   `bca/cli-batch-handler`. The legacy `tools.cli/make-batch-handler` envelope
   is the oracle: the same handlers, driven both ways, must answer the same
   `{:results :summary}` shape. The runner is injected through the handler's
   `run-fn` port, so nothing is redefined."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.batch.cli-adapter :as bca]
            [hive-mcp.batch.protocol :as proto]
            [hive-mcp.tools.consolidated.agent :as cagent]
            [hive-mcp.tools.consolidated.memory :as cmem]
            [hive-mcp.tools.cli :as cli]
            [hive-mcp.tools.memory.batch :as memory-batch]
            [hive-mcp.tools.swarm.batch :as swarm-batch]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- parse [out]
  (json/read-str (:text out) :key-fn keyword))

(defn- ok-handler [params] {:type "text" :text (str "done " (:content params))})

(defn- boom-handler [params]
  (if (= "bad" (:content params))
    (throw (ex-info "kaboom" {}))
    (ok-handler params)))

(defn- runner-fn
  "A run-fn over the REAL default runner with a stub handler tree (the port)."
  [make-runner handlers]
  (let [runner (make-runner {:handlers handlers})]
    (fn [ops opts] (proto/batch-execute runner ops opts))))

(defn- digest
  "The caller-visible shape of a batch envelope: per-op success flags and the summary."
  [out]
  (let [body (parse out)]
    {:successes (mapv :success (:results body))
     :summary   (:summary body)}))

(defn memory-batch-tags-digest
  "Unary subject: contents -> digest of `batch-tags` over the memory Batchable runner."
  [contents]
  (let [h (#'cmem/make-single-command-batch
           :tags (runner-fn memory-batch/make-memory-runner {:tags #'boom-handler}))]
    (digest (h {:operations (mapv (fn [c] {:content c}) contents)}))))

(defn legacy-tags-digest
  "Oracle: the same ops through the deprecated `make-batch-handler`."
  [contents]
  (let [h (cli/make-batch-handler {:tags #'boom-handler})]
    (digest (h {:operations (mapv (fn [c] {:command "tags" :content c}) contents)}))))

(deftrifecta batch-tags-matches-the-legacy-envelope
  hive-mcp.tools.consolidated.batch-migration-trifecta-test/memory-batch-tags-digest
  {:golden-path "test/golden/batch/memory-batch-tags-digest.edn"
   :cases {:single ["a"]
           :mixed ["a" "bad" "c"]
           :all-bad ["bad" "bad"]}
   :gen (gen/vector (gen/elements ["a" "b" "bad"]) 1 6)
   :pred (fn [{:keys [successes summary]}]
           (and (= (count successes) (:total summary))
                (= (count (filter true? successes)) (:success summary))
                (= (:total summary) (+ (:success summary) (:failed summary)))))
   :num-tests 60
   :mutations [["drops every failure" (fn [_] {:successes [true] :summary {:total 2 :success 2 :failed 0}})]
               ["miscounts the total" (fn [cs] {:successes (mapv (constantly true) cs)
                                                :summary {:total 0 :success (count cs) :failed 0}})]]
   :assert (fn []
             (doseq [cs [["a"] ["a" "bad" "c"] ["bad" "bad"]]]
               (is (= (legacy-tags-digest cs)
                      (memory-batch-tags-digest cs))
                   (pr-str cs))))})

(defn- spawn-handler [run-fn]
  (bca/cli-batch-handler
   {:run-fn run-fn :cmd-kw :spawn :tool-name "agent spawn" :shared-params? true}))

(deftest batch-spawn-matches-the-legacy-envelope
  (testing "the same spawn handler, driven through swarm.batch and through make-batch-handler"
    (doseq [cs [["a"] ["a" "bad" "c"] ["bad"]]]
      (let [legacy (digest ((cli/make-batch-handler {:spawn #'boom-handler})
                            {:operations (mapv (fn [c] {:command "spawn" :content c}) cs)}))
            now    (digest ((spawn-handler (runner-fn swarm-batch/make-swarm-runner
                                                      {:agent {:spawn #'boom-handler}}))
                            {:operations (mapv (fn [c] {:content c}) cs)}))]
        (is (= legacy now) (pr-str cs)))))
  (testing "the registered batch-spawn handler refuses an empty batch before any runner"
    (let [handler @(resolve 'hive-mcp.tools.consolidated.agent/batch-spawn-handler)]
      (doseq [params [{} {:operations []} {:operations nil}]]
        (is (true? (:isError (handler params))) (pr-str params))))))

(deftest single-command-batch-runs-through-the-batchable-runner
  (testing "accepted ops reach the runner as :tool/:command ops with shared params merged under them"
    (let [seen     (atom nil)
          run-fn   (fn [ops _opts]
                     (reset! seen ops)
                     {:success true
                      :waves   {1 {:results (mapv (fn [_] {:success true :result {:ok true}}) ops)}}
                      :summary {:total (count ops) :success (count ops) :failed 0 :waves 1}})
          batch-fn (#'cmem/make-single-command-batch :tags run-fn)
          parsed   (parse
                    (batch-fn {:parallel true
                               :tags ["shared"]
                               :operations [{:id "e1"} {:command "tags" :id "e2" :tags ["own"]}]}))]
      (is (= ["tags" "tags"] (mapv :tool @seen)))
      (is (= ["tags" "tags"] (mapv :command @seen)))
      (is (= [["shared"] ["own"]] (mapv :tags @seen)) "per-op params win over shared ones")
      (is (every? #(not (contains? % :parallel)) @seen) "batch plumbing does not bleed into ops")
      (is (= {:total 2 :success 2 :failed 0} (:summary parsed)))))

  (testing "a batch the runner refuses before executing reports every accepted op failed with its errors"
    (let [run-fn   (fn [ops _opts]
                     {:success false
                      :errors  ["Duplicate operation ID: 'a' (appears 2 times)"]
                      :summary {:total (count ops) :success 0 :failed 0 :waves 0}})
          batch-fn (#'cmem/make-single-command-batch :promote run-fn)
          parsed   (parse (batch-fn {:operations [{:id "a"} {:id "a"}]}))]
      (is (= {:total 2 :success 0 :failed 2} (:summary parsed)))
      (is (every? false? (map :success (:results parsed))))
      (is (every? #(str/includes? (:error %) "Duplicate operation ID") (:results parsed)))))

  (testing "a mixed batch fails only the foreign op and zips results back to caller positions"
    (let [run-fn   (runner-fn memory-batch/make-memory-runner {:feedback #'ok-handler})
          batch-fn (#'cmem/make-single-command-batch :feedback run-fn)
          parsed   (parse (batch-fn {:operations [{:content "one"}
                                                  {:command "kg edge"}
                                                  {:command "feedback" :content "three"}]}))]
      (is (= [true false true] (mapv :success (:results parsed))))
      (is (= {:total 3 :success 2 :failed 1} (:summary parsed))))))

(deftest all-seven-commands-are-registered-on-the-batchable-path
  (testing "memory's six batch verbs and agent's batch-spawn stay dispatchable"
    (doseq [cmd [:batch-add :batch-feedback :batch-tags :batch-duration :batch-promote :batch-demote]]
      (is (ifn? (get cmem/canonical-handlers cmd)) (str cmd)))
    (is (some? (get cagent/canonical-handlers :batch-spawn)))))
