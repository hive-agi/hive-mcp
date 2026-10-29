(ns hive-mcp.tools.composite.moved-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.composite.moved :as moved]))

(deftest replacement-root-test
  (is (= "cloak" (moved/replacement-root "Deprecated: use `cloak on`, `cloak off`.")))
  (is (= "git" (moved/replacement-root "use `git ship land`")))
  (is (= "cloak" (moved/replacement-root "DEPRECATED: Use `Cloak status`.")) "case-insensitive")
  (is (nil? (moved/replacement-root "Old thing.")))
  (is (nil? (moved/replacement-root "use ``")))
  (is (nil? (moved/replacement-root nil))))

(deftest moved-commands-text-test
  (testing "one line per replacement root, naming each command's first word once"
    (is (= "Moved to the `cloak` tool (call it with command=\"help\"): enclave, seal, work."
           (moved/moved-commands-text
            [["enclave-on" "Deprecated: use `cloak on`."]
             ["enclave-status" "Deprecated: use `cloak enclave status`."]
             ["seal-edges" "Deprecated: use `cloak edges hide`."]
             ["work-handoff-push" "Deprecated: use `cloak handoff push`."]]))))
  (testing "several roots, sorted"
    (is (= (str "Moved to the `cloak` tool (call it with command=\"help\"): seal. "
                "Moved to the `git` tool (call it with command=\"help\"): ship.")
           (moved/moved-commands-text [["ship-land" "use `git ship land`"]
                                       ["seal" "Deprecated: use `cloak hide`."]]))))
  (testing "texts that name no root -> nothing"
    (is (nil? (moved/moved-commands-text [["old-flag" "Old thing."] ["x" nil]])))
    (is (nil? (moved/moved-commands-text [])))))
