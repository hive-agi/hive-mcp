(ns hive-mcp.tools.registry-compact-schema-test
  "The opt-in compact projection of the advertised MCP surface."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.registry :as reg]))

(def ^:private compact-schema #'reg/compact-schema)

(defn- props [tool] (get-in tool [:inputSchema :properties]))
(defn- prop-names [tool] (set (map name (keys (props tool)))))

(deftest the-default-surface-is-untouched
  (testing "no flag, and nil opts, both give the surface callers already have"
    (let [base (reg/get-advertised-tools)]
      (is (= base (reg/get-advertised-tools nil)))
      (is (= base (reg/get-advertised-tools {})))
      (is (= base (reg/get-advertised-tools {:compact-schema? false}))))))

(deftest compacting-changes-schemas-and-nothing-else
  (let [full    (reg/get-advertised-tools)
        compact (reg/get-advertised-tools {:compact-schema? true})]
    (testing "the tool SET is identical: this narrows schemas, never the surface"
      (is (= (mapv :name full) (mapv :name compact)))
      (is (= (mapv :description full) (mapv :description compact))))
    (testing "every tool keeps its required params"
      (doseq [t compact]
        (is (every? (prop-names t) (get-in t [:inputSchema :required]))
            (str (:name t) " dropped a required param"))))
    (testing "properties are only ever REMOVED, never added or rewritten"
      (doseq [[f c] (map vector full compact)]
        (is (every? (prop-names f) (prop-names c)))
        (is (= (select-keys (props f) (keys (props c))) (props c)))))))

(deftest only-subcommand-tagged-params-are-dropped
  (let [full    (reg/get-advertised-tools)
        compact (reg/get-advertised-tools {:compact-schema? true})]
    (doseq [[f c] (map vector full compact)
            :let  [dropped (remove (prop-names c) (prop-names f))]
            k     dropped]
      (is (re-find #"^\s*\[" (str (:description (get (props f) k))))
          (str (:name f) "/" k " was dropped without a [subcommand] tag")))))

(deftest the-projection-actually-removes-something
  (testing "a predicate that silently matched nothing would pass every test above"
    (let [full    (reg/get-advertised-tools)
          compact (reg/get-advertised-tools {:compact-schema? true})
          n-full  (reduce + (map (comp count props) full))
          n-comp  (reduce + (map (comp count props) compact))]
      (is (pos? (- n-full n-comp))
          "no property was dropped anywhere on the real advertised surface"))))

(deftest compacting-is-idempotent
  (let [once (reg/get-advertised-tools {:compact-schema? true})]
    (is (= once (mapv compact-schema once)))))

(deftest a-required-param-survives-even-when-it-carries-a-subcommand-tag
  (testing "property keys are STRINGS here; a keyword-keyed membership test
            would silently fail to protect required params"
    (let [tool {:name "probe"
                :inputSchema
                {:required ["command"]
                 :properties
                 {"command" {:description "[reload] tagged AND required"}
                  "addon"   {:description "[reload] tagged, not required"}
                  "plain"   {:description "untagged, not required"}}}}]
      (is (= #{"command" "plain"} (prop-names (compact-schema tool)))))))

(deftest a-tool-with-no-tagged-params-is-returned-unchanged
  (let [tool {:name "probe"
              :inputSchema {:required ["command"]
                            :properties {"command" {:description "do a thing"}
                                         "other"   {:description "another thing"}}}}]
    (is (= tool (compact-schema tool))))
  (testing "and an empty or absent property map is not a crash"
    (is (= {:name "p" :inputSchema {:properties {}}}
           (compact-schema {:name "p" :inputSchema {:properties {}}})))
    (is (= {:name "p" :inputSchema {}}
           (compact-schema {:name "p" :inputSchema {}})))))
