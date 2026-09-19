(ns hive-mcp.tools.multi-entity-id-test
  "Regression: a caller's entity id reaches the tool handler as `:id` through
   both multi surfaces (`dsl` and `operations`), while the batch keeps its own
   op label for `$ref` resolution and `depends_on`.

   Handlers are stubs injected through the runner's `:resolve-handler` port;
   no memory store is touched."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.batch :as batch]
            [hive-mcp.dsl.verbs :as verbs]
            [hive-mcp.test.stub.batch-extensions :as bx]))

(use-fixtures :each bx/with-batch-extensions)

(defn- json-ok [data]
  {:type "text" :text (json/write-str data)})

(defn- capturing-memory
  "resolve-handler stub. The memory handler records every args map it
   receives; `add` answers a created id, every other command echoes :id."
  [calls]
  (fn [tool]
    (when (= "memory" tool)
      (fn [args]
        (swap! calls conj args)
        (json-ok (if (= "add" (:command args))
                   {:id "mem-created" :status "created"}
                   {:id (:id args) :ok true}))))))

(defn- run [ops calls]
  (batch/run-operations ops {:resolve-handler (capturing-memory calls)}))

(defn- by-command [calls]
  (into {} (map (juxt :command identity)) calls))

(deftest dsl-memory-entity-verbs-deliver-id-test
  (testing "m= m# m^ m_ hand the handler the entity id, not the $N label"
    (let [calls  (atom [])
          ops    (verbs/compile-paragraph [["m=" {"id" "e-edit" "c" "new body"}]
                                           ["m#" {"id" "e-tags" "#" ["x"]}]
                                           ["m^" {"id" "e-promote"}]
                                           ["m_" {"id" "e-demote"}]])
          _      (is (= ["$0" "$1" "$2" "$3"] (mapv :id ops))
                     "op labels stay compiler-owned")
          result (run ops calls)
          seen   (by-command @calls)]
      (is (:success result))
      (is (= 4 (get-in result [:summary :success])))
      (is (= "e-edit" (get-in seen ["edit" :id])))
      (is (= "new body" (get-in seen ["edit" :content])))
      (is (= "e-tags" (get-in seen ["tags" :id])))
      (is (= ["x"] (get-in seen ["tags" :tags])))
      (is (= "e-promote" (get-in seen ["promote" :id])))
      (is (= "e-demote" (get-in seen ["demote" :id])))
      (is (not-any? #(contains? % :entity_id) @calls)
          "the carrier key never reaches a handler"))))

(deftest dsl-entity-id-may-be-a-ref-test
  (testing "m= with id $ref:$0.data.id depends on $0 and receives the created id"
    (let [calls  (atom [])
          ops    (verbs/compile-paragraph [["m+" {"c" "body" "t" "note"}]
                                           ["m=" {"id" "$ref:$0.data.id" "c" "edited"}]])
          _      (is (= ["$0"] (:depends_on (second ops)))
                     "a $ref entity id is collected as a dependency")
          result (run ops calls)
          seen   (by-command @calls)]
      (is (:success result))
      (is (= 2 (get-in result [:summary :waves])))
      (is (= "mem-created" (get-in seen ["edit" :id])))
      (is (not (contains? (get seen "add") :id))
          "a DSL op without an entity id gets no :id; its $N label is not forwarded"))))

(deftest dsl-remapped-verbs-unchanged-test
  (testing "m@ keeps its batch-get remap alongside the general carrier"
    (let [[get-op] (verbs/compile-paragraph [["m@" {"id" "mem-1"}]])]
      (is (= "$0" (:id get-op)))
      (is (= "batch-get" (:command get-op)))
      (is (= ["mem-1"] (:ids get-op)))
      (is (not (contains? get-op :entity_id))))))

(deftest operations-form-entity-id-test
  (testing "an explicit op's id is both its label and the handler's entity id"
    (let [calls  (atom [])
          result (run [{"id" "20260913-abc" "tool" "memory"
                        "command" "edit" "content" "x"}]
                      calls)]
      (is (:success result))
      (is (= "20260913-abc" (:id (first @calls))))
      (is (= "20260913-abc" (-> result :waves (get 1) :results first :id))
          "the op result is still keyed by the label")))

  (testing "entity_id separates the entity from the label and may be a $ref"
    (let [calls  (atom [])
          result (run [{"id" "mk" "tool" "memory" "command" "add" "content" "b"}
                       {"id" "ed" "tool" "memory" "command" "edit"
                        "entity_id" "$ref:mk.data.id"
                        "depends_on" ["mk"] "content" "c"}]
                      calls)
          seen   (by-command @calls)]
      (is (:success result))
      (is (= 2 (get-in result [:summary :success])))
      (is (= "mem-created" (get-in seen ["edit" :id])))
      (is (not-any? #(contains? % :entity_id) @calls))))

  (testing "a generated label is never handed to the handler"
    (let [calls (atom [])]
      (run [{"tool" "memory" "command" "promote"}] calls)
      (is (= 1 (count @calls)))
      (is (not (contains? (first @calls) :id))))))

(deftest execute-op-restores-entity-id-test
  (testing "the executor hands :entity_id to the handler as :id and echoes the label"
    (let [seen   (atom nil)
          result (batch/execute-op (fn [_] (fn [args] (reset! seen args) {:ok true}))
                                   {:id "$0" :tool "memory" :command "tags"
                                    :entity_id "e-1" :tags ["t"]})]
      (is (= "$0" (:id result)))
      (is (= {:command "tags" :id "e-1" :tags ["t"]} @seen))))
  (testing "no carrier, no :id"
    (let [seen (atom nil)]
      (batch/execute-op (fn [_] (fn [args] (reset! seen args) {:ok true}))
                        {:id "$1" :tool "memory" :command "add" :content "c"})
      (is (= {:command "add" :content "c"} @seen)))))
