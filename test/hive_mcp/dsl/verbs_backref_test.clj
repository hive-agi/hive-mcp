(ns hive-mcp.dsl.verbs-backref-test
  "Bare positional back-refs ($N) used as k> edge endpoints.

   A k> sentence whose from/to is a bare \"$N\" naming an op of the same
   paragraph must compile to the batch ref \"$ref:$N.data.id\" (the created
   entity id), with \"$N\" in :depends_on. A \"$N\" past the paragraph stays
   literal, so it can never silently point at an unrelated op."
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.dsl.verbs :as verbs]))

(deftest kg-edge-bare-positional-backrefs-resolve-test
  (testing "k> with bare $0/$1 from/to compiles to $ref:$N.data.id with depends_on"
    (let [ops  (verbs/compile-paragraph
                [["m+" {"c" "first" "t" "note"}]
                 ["m+" {"c" "second" "t" "note"}]
                 ["k>" {"from" "$0" "to" "$1" "relation" "implements"}]])
          edge (nth ops 2)]
      (is (= "$2" (:id edge)))
      (is (= "kg" (:tool edge)))
      (is (= "$ref:$0.data.id" (:from edge)))
      (is (= "$ref:$1.data.id" (:to edge)))
      (is (= ["$0" "$1"] (:depends_on edge)))))
  (testing "an out-of-range $9 stays literal and adds no dependency"
    (let [edge (nth (verbs/compile-paragraph
                     [["m+" {"c" "only" "t" "note"}]
                      ["k>" {"from" "$0" "to" "$9" "relation" "implements"}]])
                    1)]
      (is (= "$ref:$0.data.id" (:from edge)))
      (is (= "$9" (:to edge)))
      (is (= ["$0"] (:depends_on edge))))))
