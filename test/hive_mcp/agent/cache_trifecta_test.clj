(ns hive-mcp.agent.cache-trifecta-test
  "Facets for the prompt-cache breakpoint placement, derived from its
   contracts. The invariants are the ones the namespace documents: never more
   than `max-breakpoints` markers, text is never edited, and a provider that
   declares no dialect is left byte-identical."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.cache :as cache]
            [hive-mcp.agent.schema :as schema]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Message
  [:map
   [:role [:enum "system" "user" "assistant" "tool"]]
   [:content [:or
              [:string {:min 0 :max 8}]
              [:vector {:max 2} [:map [:type [:= "text"]] [:text [:string {:max 5}]]]]]]])

(def ^:private Messages [:vector {:max 9} Message])

(defn- texts
  "Every text a message array carries, markers aside."
  [msgs]
  (mapv (fn [{:keys [content]}]
          (if (string? content) content (mapv :text content)))
        msgs))

(defn- flat-texts [msgs]
  (mapv (fn [{:keys [content]}]
          (if (string? content) [content] (vec (map :text content))))
        msgs))

(deftrifecta-from-schema mark-messages-trifecta
  hive-mcp.agent.cache/mark-messages
  {:in Messages
   :out [:sequential :map]
   :rel (fn [msgs out]
          (and (= (count msgs) (count out))
               (<= (cache/marker-count out) cache/max-breakpoints)
               (= (map :role msgs) (map :role out))
               ;; marking annotates, it never edits text
               (= (mapv #(remove empty? %) (flat-texts msgs))
                  (mapv #(remove empty? %) (flat-texts out)))))
   :num-tests 300})

(deftrifecta-from-schema marker-count-trifecta
  hive-mcp.agent.cache/marker-count
  {:in Messages
   :out :int
   :rel (fn [msgs n] (and (zero? n) (= 0 (cache/marker-count msgs))))
   :num-tests 100})

(deftrifecta-from-schema maybe-mark-undeclared-provider-trifecta
  hive-mcp.agent.cache/maybe-mark
  {:in [:cat [:maybe [:map [:cache-control {:optional true} [:enum :other]]]]
        [:maybe :string] Messages]
   :out [:sequential :map]
   :rel (fn [[_ _ msgs] out] (= msgs out))
   :num-tests 100})

(deftest arms-of-cache-control-style
  (let [entry {:cache-control :anthropic-style}]
    (is (= :anthropic-style (cache/cache-control-style entry "anthropic/claude-opus-4-7")))
    (is (= :anthropic-style (cache/cache-control-style entry "claude-sonnet-4")))
    (is (nil? (cache/cache-control-style entry "deepseek/deepseek-chat")))
    (is (nil? (cache/cache-control-style {} "claude")))
    (is (nil? (cache/cache-control-style nil "claude")))
    (is (nil? (cache/cache-control-style entry nil)))))

(deftest ttl-and-breakpoint-ceiling
  (is (= :1h (cache/ttl-of {:cache-ttl :1h})))
  (is (= :5m (cache/ttl-of {:cache-ttl :5m})))
  (is (= :5m (cache/ttl-of nil)))
  (let [msgs (vec (concat [{:role "system" :content "s"}]
                          (mapcat (fn [i] [{:role "user" :content (str "u" i)}
                                           {:role "assistant" :content (str "a" i)}])
                                  (range 6))))
        out (cache/mark-messages msgs)]
    (is (= cache/max-breakpoints (cache/marker-count out)))
    (is (= 1 (cache/marker-count (cache/mark-messages [{:role "user" :content "x"}]))))
    (is (= [] (cache/mark-messages [])))
    (testing "1h marker carries ttl, 5m omits it"
      (is (= {:type "ephemeral" :ttl "1h"}
             (:cache_control (last (:content (first (cache/mark-messages msgs :1h)))))))
      (is (= {:type "ephemeral"}
             (:cache_control (last (:content (first out)))))))))

(golden/deftest-golden-fn cache-marking-golden
  "test/golden/agent/cache-marking.edn"
  (fn []
    (let [conv [{:role "system" :content "sys"}
                {:role "user" :content "u1"}
                {:role "assistant" :content "a1"}
                {:role "user" :content "u2"}
                {:role "tool" :content "t1"}]]
      {:marked (cache/mark-messages conv)
       :marked-1h (cache/mark-messages conv :1h)
       :undeclared (= conv (cache/maybe-mark {} "claude" conv))
       :declared-count (cache/marker-count
                        (cache/maybe-mark {:cache-control :anthropic-style}
                                          "anthropic/claude-opus" conv))
       :family [(cache/anthropic-family? "anthropic/x") (cache/anthropic-family? "claude")
                (cache/anthropic-family? "gpt-5") (cache/anthropic-family? nil)]})))

(deftest-contract-coverage cache-contract-coverage
  {:paths ["src/hive_mcp/agent/cache.clj"]})
