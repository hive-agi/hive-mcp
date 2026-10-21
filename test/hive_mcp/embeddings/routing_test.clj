(ns hive-mcp.embeddings.routing-test
  "Trifecta tests for apply-route-flip! — pins the user-intent guard."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.embeddings.routing :as routing]
            [hive-mcp.config.core :as global-config]
            [hive-test.golden :refer [deftest-golden]]
            [hive-mcp.config.test-support :as cfg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; -----------------------------------------------------------------------------
;; Helpers — isolate global-config atom for each scenario.
;; -----------------------------------------------------------------------------

(def ^:private mutations (atom []))

(defn- run-flip
  "Helper: call apply-route-flip! with given config + opts, return
   {:action <:flipped|:pinned|:no-secret> :mutations <vec>}.
   Mutations captured into the test-local atom by the with-redefs mock."
  [{:keys [secrets routes opts]}]
  (reset! mutations [])
  (let [action (with-redefs [global-config/get-secret        (fn [k] (get secrets k))
                             global-config/get-global-config (fn [] {:embedder {:routes routes}})
                             global-config/update-in-config! (fn [path _f route to & _]
                                                               (swap! mutations conj
                                                                      {:path path :route route :to to}))]
                 (routing/apply-route-flip! opts))]
    {:action action
     :mutations @mutations}))

;; -----------------------------------------------------------------------------
;; Golden cases (deftest — keeping classic shape; deftrifecta needs a single
;; var-sym, here we want to assert side-effects)
;; -----------------------------------------------------------------------------

(def base-opts
  {:route   :type/plan
   :default :openrouter-qwen3
   :to      :venice-qwen3
   :secret  :venice-api-key
   :reason  "test"})

(deftest apply-route-flip!-golden
  (testing "no secret → :no-secret, no mutation"
    (let [r (run-flip {:secrets {}
                       :routes  {:type/plan :openrouter-qwen3}
                       :opts    base-opts})]
      (is (= :no-secret (:action r)))
      (is (empty? (:mutations r)))))

  (testing "secret + default route → :flipped, single mutation"
    (let [r (run-flip {:secrets {:venice-api-key "sk-test"}
                       :routes  {:type/plan :openrouter-qwen3}
                       :opts    base-opts})]
      (is (= :flipped (:action r)))
      (is (= [{:path  [:embedder :routes]
               :route :type/plan
               :to    :venice-qwen3}]
             (:mutations r)))))

  (testing "secret + user-pinned route → :pinned, no mutation"
    (let [r (run-flip {:secrets {:venice-api-key "sk-test"}
                       :routes  {:type/plan :ollama-nomic}
                       :opts    base-opts})]
      (is (= :pinned (:action r)))
      (is (empty? (:mutations r)))))

  (testing "secret + already-flipped (idempotent re-run) → :pinned"
    (let [r (run-flip {:secrets {:venice-api-key "sk-test"}
                       :routes  {:type/plan :venice-qwen3}
                       :opts    base-opts})]
      (is (= :pinned (:action r)))
      (is (empty? (:mutations r)))))

  (testing "secret + nil route (fresh atom, no defaults yet) → :pinned"
    (let [r (run-flip {:secrets {:venice-api-key "sk-test"}
                       :routes  {}
                       :opts    base-opts})]
      (is (= :pinned (:action r))
          "nil != :openrouter-qwen3 — leave alone, user owns it")
      (is (empty? (:mutations r))))))

(deftest apply-route-flip!-totality
  (testing "returns one of the three known actions for any input"
    (doseq [secret-present? [true false]
            current-route   [:openrouter-qwen3 :ollama-nomic :venice-qwen3 nil]]
      (let [r (run-flip {:secrets (when secret-present?
                                    {:venice-api-key "sk-x"})
                         :routes  (cond-> {} current-route (assoc :type/plan current-route))
                         :opts    base-opts})]
        (is (#{:no-secret :flipped :pinned} (:action r))
            (str "secret? " secret-present? " route " current-route))))))

(deftest apply-route-flip!-honors-route-arg
  (testing "different :route key targets a different slot"
    (let [r (run-flip {:secrets {:venice-api-key "sk-x"}
                       :routes  {:type/decision :openrouter-qwen3}
                       :opts    (assoc base-opts :route :type/decision)})]
      (is (= :flipped (:action r)))
      (is (= :type/decision (-> r :mutations first :route))))))

;; The config PORT injected (an in-memory config source via cfg/with-config),
;; no with-redefs: every branch of both flips, with the config each leaves.
(defn- flip-in [config f opts]
  (cfg/with-config config
    (let [action (f opts)]
      {:action action :embedder (:embedder (global-config/get-global-config))})))

(deftest-golden route-and-collection-flips-golden
  "test/golden/hive-mcp/embeddings/routing-flips.edn"
  (let [route-opts {:route :type/plan :default :openrouter-qwen3 :to :venice-qwen3
                    :secret :f04-test-key :reason "golden"}
        calls      (atom 0)
        coll-opts  (fn [cfg-fn] {:collection "hive-mcp-presets" :default :ollama
                                 :to-id :openrouter :configure-fn cfg-fn :reason "golden"})
        counted    (coll-opts #(swap! calls inc))
        ;; a vector, not a map literal: the cases run in this order and
        ;; :configure-calls is read after all of them
        cases      [[:route/no-secret (flip-in {:embedder {:routes {:type/plan :openrouter-qwen3}}}
                                               routing/apply-route-flip! route-opts)]
                    [:route/flipped (flip-in {:secrets {:f04-test-key "sk"}
                                              :embedder {:routes {:type/plan :openrouter-qwen3}}}
                                             routing/apply-route-flip! route-opts)]
                    [:route/pinned (flip-in {:secrets {:f04-test-key "sk"}
                                             :embedder {:routes {:type/plan :ollama-nomic}}}
                                            routing/apply-route-flip! route-opts)]
                    [:route/unset (flip-in {:secrets {:f04-test-key "sk"}}
                                           routing/apply-route-flip! route-opts)]
                    [:coll/pinned (flip-in {:embedder {:collections {"hive-mcp-presets" :venice}}}
                                           routing/apply-collection-flip! counted)]
                    [:coll/default (flip-in {:embedder {:collections {"hive-mcp-presets" :ollama}}}
                                            routing/apply-collection-flip! counted)]
                    [:coll/unpinned (flip-in {} routing/apply-collection-flip! counted)]
                    [:coll/error (flip-in {} routing/apply-collection-flip!
                                          (coll-opts #(throw (ex-info "bad provider" {}))))]]]
    (assoc (into {} cases) :configure-calls @calls)))
