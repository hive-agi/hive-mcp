(ns hive-mcp.agent.dispatch-backend-test
  "make-backend for DISPATCH-routed providers (:anthropic :chatgpt :codex
   :kimi): they wrap the hive-agent LLMRouter through the `:complete` port
   instead of throwing \"not an OpenAI-compat provider\". The router is a
   stub passed through the port; nothing is redefined."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.agent.cider :as cider]
            [hive-mcp.agent.dispatch-backend :as dispatch]
            [hive-mcp.agent.protocol :as proto]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private usage {:prompt_tokens 3 :completion_tokens 5 :total_tokens 8})

(deftrifecta router-message-folds-to-backend-result
  hive-mcp.agent.dispatch-backend/message->result
  {:golden-path "test/golden/agent/dispatch-message-result.edn"
   :cases {:text      {:role "assistant" :content "hello"}
           :usage     {:role "assistant" :content "hello" :usage usage}
           :tool-call {:role "assistant"
                       :tool_calls [{:id "c1" :type "function"
                                     :function {:name "t" :arguments "{\"a\":1}"}}]}
           :empty     {:role "assistant" :content ""}
           :nil       nil}
   :gen (gen/let [text (gen/not-empty gen/string-alphanumeric)
                  with-usage? gen/boolean]
          (cond-> {:role "assistant" :content text}
            with-usage? (assoc :usage usage)))
   :pred #(and (map? %) (contains? #{:text :tool_calls :error} (:type %)))
   :num-tests 50
   :mutations [["a router message is reported as an error"
                (fn [_] {:type :error :error "mutant"})]
               ["usage is dropped on the way out"
                (fn [m] {:type :text :content (:content m)})]]
   :assert (fn []
             (is (= {:type :text :content "hello"}
                    (dispatch/message->result {:content "hello"})))
             (is (= {:input 3 :output 5 :total 8}
                    (:usage (dispatch/message->result {:content "hello" :usage usage}))))
             (is (= :tool_calls
                    (:type (dispatch/message->result
                            {:tool_calls [{:id "c1" :type "function"
                                           :function {:name "t" :arguments "{}"}}]}))))
             (is (= :error (:type (dispatch/message->result {:content ""})))))})

(defn- stub-router
  "A `complete` port that records every params map it is handed and answers
   with the canned assistant message."
  [calls message]
  (fn [params] (swap! calls conj params) message))

(deftest dispatch-providers-build-through-the-router-port-test
  (testing "each dispatch-routed provider builds a backend instead of the OpenAI-compat refusal"
    (doseq [[provider router-provider] [[:anthropic :anthropic]
                                        [:chatgpt   :chatgpt]
                                        [:codex     :chatgpt]
                                        [:kimi      :kimi]]]
      (let [calls   (atom [])
            backend (cider/make-backend provider
                                        {:model    "m-1"
                                         :complete (stub-router calls {:content "ok"})})
            result  (proto/chat backend [{:role "user" :content "hi"}] nil)]
        (is (satisfies? proto/LLMBackend backend) (str provider))
        (is (= "m-1" (proto/model-name backend)))
        (is (= {:type :text :content "ok"} result) (str provider))
        (is (= [{:messages [{:role "user" :content "hi"}]
                 :model    "m-1"
                 :provider router-provider}]
               @calls)
            (str provider " addresses the router's own provider key"))))))

(deftest dispatch-backend-sends-tools-in-openai-shape-test
  (let [calls   (atom [])
        backend (cider/make-backend :kimi {:model "m" :complete (stub-router calls {:content "ok"})})]
    (proto/chat backend [{:role "user" :content "x"}]
                [{:name "t" :description "d" :inputSchema {:type "object"}}])
    (is (= [{:type "function"
             :function {:name "t" :description "d" :parameters {:type "object"}}}]
           (:tools (first @calls))))))

(deftest dispatch-backend-build-failures-are-ex-info-with-a-cause-test
  (let [cause (fn [f] (try (f) nil
                           (catch clojure.lang.ExceptionInfo e (:error (ex-data e)))))]
    (testing "no model given and none configured: a fallback chain can skip the provider"
      (is (= :model-not-configured
             (cause #(cider/make-backend :kimi {:complete (fn [_] {})})))))
    (testing "a provider the router has no client for"
      (is (= :dispatch-provider-unroutable
             (cause #(dispatch/dispatch-backend {:provider :nope :model "m"
                                                 :complete (fn [_] {})})))))
    (testing "no router reachable: the resolver port answers nil"
      (is (= :dispatch-router-unavailable
             (cause #(dispatch/dispatch-backend {:provider :kimi :model "m"
                                                 :resolve-complete (constantly nil)})))))
    (testing "the resolver port supplies the router when no :complete is given"
      (let [calls (atom [])
            b     (dispatch/dispatch-backend
                   {:provider :kimi :model "m"
                    :resolve-complete (constantly (stub-router calls {:content "ok"}))})]
        (is (= {:type :text :content "ok"}
               (proto/chat b [{:role "user" :content "x"}] nil)))))))

(deftest openai-compat-providers-still-take-the-compat-branch-test
  (testing "a keyed chat-completions provider is unchanged"
    (let [b (cider/make-backend :venice {:api-key "sk-test" :model "test-model"})]
      (is (= "venice" (:provider-name b)))))
  (testing "an unknown provider still throws Unknown backend type"
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"Unknown backend type"
                          (cider/make-backend :zz-no-such-provider {:model "m"})))))
