(ns hive-mcp.tools.registry-order-test
  "The advertised tool array must be a function of the tool SET, never of the
   order the registry happened to hand it over in.

   Addon tools reach `get-base-tools` as the vals of a PersistentHashMap, so
   their order follows the hash layout of whatever key set is registered.
   Register or drop one addon and the whole tail can reshuffle with no tool
   actually changing.

   The tool array is the FIRST span of an LLM request, ahead of the system
   prompt and ahead of the messages, and a provider caches a prefix on its
   BYTES. So a reshuffle that changes nothing semantically still invalidates
   every cached span behind it and makes the caller re-pay for the whole
   prompt. These tests pin the ordering that keeps a changed tool SET the only
   thing that can cost anything."
  (:require [clojure.test :refer [deftest testing is]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as tc-prop]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.registry :as reg]
            [hive-mcp.tools.consolidated.memory :as c-memory]
            [hive-mcp.knowledge-graph.schema :as kg-schema]))

(defn- mk-tool
  "Minimal tool-def. The `zz-addon-` prefix keeps generated names clear of
   every core root and of the :tool-roots :absorbed list in config."
  [nm]
  {:name (str "zz-addon-" nm) :handler (fn [_] {:type "text" :text nm})})

(defn- advertised-names
  "What tools/list would carry, given `tools` as the addon contribution."
  [tools]
  (with-redefs [ext/get-registered-tools (fn [] tools)]
    (mapv :name (reg/get-all-tools))))

(def ^:private sample
  (mapv mk-tool ["omega" "alpha" "mike" "bravo" "zulu"]))

(deftest the-addon-tail-is-sorted-by-name
  (testing "however the registry hands them over, the tail comes out sorted"
    (let [core-count (count (reg/core-tools))
          advertised (advertised-names sample)
          tail       (drop core-count advertised)]
      (is (= ["zz-addon-alpha" "zz-addon-bravo" "zz-addon-mike"
              "zz-addon-omega" "zz-addon-zulu"]
             (vec tail))))))

(deftest the-core-prefix-is-left-alone
  (testing "core roots keep their declared order, which is already deterministic"
    (let [core (mapv :name (reg/core-tools))]
      (is (= core (vec (take (count core) (advertised-names sample))))
          "sorting the addon tail must not disturb the span in front of it"))))

(deftest an-unchanged-set-advertises-identical-bytes
  (testing "two different registry orderings of one set agree exactly"
    (is (= (advertised-names sample)
           (advertised-names (reverse sample))
           (advertised-names (shuffle sample))))))

(defspec permuting-the-registry-cannot-change-what-is-advertised 60
  (tc-prop/for-all [names (gen/set (gen/not-empty gen/string-alphanumeric)
                                   {:min-elements 1 :max-elements 12})]
    (let [tools (mapv mk-tool names)]
      (= (advertised-names (shuffle tools))
         (advertised-names (shuffle tools))
         (advertised-names (sort-by :name tools))))))

(defspec the-set-is-preserved-whatever-the-order 60
  (tc-prop/for-all [names (gen/set (gen/not-empty gen/string-alphanumeric)
                                   {:min-elements 1 :max-elements 12})]
    (let [tools (mapv mk-tool names)
          core  (set (mapv :name (reg/core-tools)))]
      (= (into core (map :name) tools)
         (set (advertised-names (shuffle tools)))))))

;; =============================================================================
;; Registry-backed enums inside a tool schema
;; =============================================================================
;;
;; The memory tool's `relation` enum is resolved at advertisement time from
;; kg-schema/relation-types, which answers a SET. Its iteration order follows
;; the hash layout of whatever is registered, so before this was sorted an
;; addon registering one relation could reorder the whole enum and invalidate
;; every prefix cached behind the tools span.

(defn- relation-enum []
  (get-in (first (c-memory/tool-defs))
          [:inputSchema :properties "relation" :enum]))

(deftest the-relation-enum-is-sorted
  (testing "advertised relation types come out in a defined order"
    (let [enum (relation-enum)]
      (is (seq enum) "the enum resolved at all")
      (is (= (vec (sort enum)) (vec enum)))
      (is (= (count (distinct enum)) (count enum)) "no duplicates"))))

(deftest registering-a-relation-cannot-reorder-the-others
  (testing "an addon's new relation slots into place instead of reshuffling"
    (let [before (relation-enum)]
      (try
        (kg-schema/register-relation-type! :zz-order-test-relation)
        (let [after (relation-enum)]
          (is (= (vec (sort after)) (vec after))
              "still sorted after an addon registers")
          (is (some #{"zz-order-test-relation"} after)
              "the new relation is advertised")
          (is (= (vec before) (vec (remove #{"zz-order-test-relation"} after)))
              "every other relation kept its exact position"))
        (finally
          (swap! @(ns-resolve 'hive-mcp.knowledge-graph.schema
                              'relation-type-extensions)
                 disj :zz-order-test-relation))))))
