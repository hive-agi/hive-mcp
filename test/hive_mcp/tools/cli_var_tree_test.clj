(ns hive-mcp.tools.cli-var-tree-test
  "The dispatch tree is VAR-TRANSPARENT: a node may be a var, at any depth, and
   the walk classifies it by what the var currently holds.

   This is the precondition for converting the 641 frozen dispatch entries the
   dev/var_seam_census probe counts (kanban 20260821211613-0c0e618f). Storing
   `#'handler` in a table is only safe once the walker sees a var holding a MAP
   as a subtree — under a bare `map?` it read as an opaque leaf and the whole
   subtree stopped being reachable. That failure is silent: the command comes
   back `:not-found`, which reads as a missing command rather than as a broken
   table.

   The last test is the point of the exercise. It rebinds a handler var and
   dispatches again through a table built BEFORE the rebind, which is exactly
   what a namespace reload does to a table built at load time."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.cli :as cli]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn leaf-handler [_params] {:type "text" :text "leaf"})
(defn nested-handler [_params] {:type "text" :text "nested"})
(defn default-handler [_params] {:type "text" :text "default"})

(def subtree
  {:inner    #'nested-handler
   :_handler #'default-handler})

(def subtree-without-default
  "A var-held subtree with nothing to fall back to, so :not-found is still
   reachable through one."
  {:inner #'nested-handler})

(def tree
  {:leaf #'leaf-handler
   :sub  #'subtree})

(defn- text [response]
  (or (:text response)
      (some-> response :content first :text)))

(deftest a-var-held-leaf-resolves
  (testing "a leaf stored as #'handler is found, and handed on AS THE VAR"
    (let [{:keys [handler path-used error]} (cli/resolve-handler tree [:leaf])]
      (is (nil? error))
      (is (= [:leaf] path-used))
      (is (var? handler)
          "dereferencing at resolve time would re-freeze the value one call before invoke")
      (is (= "leaf" (:text (handler {})))))))

(deftest a-var-held-subtree-is-descended-into
  (testing "a var holding a MAP is a subtree, not an opaque leaf"
    (let [{:keys [handler path-used error]} (cli/resolve-handler tree [:sub :inner])]
      (is (nil? error)
          "under a bare map? the walk stopped at :sub and answered :not-found")
      (is (= [:sub :inner] path-used))
      (is (= "nested" (:text (handler {})))))))

(deftest a-var-held-subtree-still-falls-back-to-its-default
  (testing ":_handler inside a var-held subtree is reachable"
    (let [{:keys [handler path-used]} (cli/resolve-handler tree [:sub])]
      (is (= [:sub] path-used))
      (is (= "default" (:text (handler {})))))))

(deftest an-unknown-command-is-still-not-found
  (testing "var-transparency did not make the walk vacuous"
    (is (= :not-found (:error (cli/resolve-handler tree [:nope]))))
    (is (= :not-found (:error (cli/resolve-handler {:sub #'subtree-without-default}
                                                  [:sub :nope])))
        "a var-held subtree with no :_handler has nothing to fall back to"))
  (testing "but an unknown subcommand UNDER a default still reaches that default"
    (let [{:keys [handler error]} (cli/resolve-handler tree [:sub :nope :deeper])]
      (is (nil? error)
          "documented behaviour: a partial match falls back to :_handler, and
           that must keep working when the subtree arrives through a var")
      (is (= "default" (:text (handler {})))))))

(deftest help-lists-commands-behind-vars
  (testing "a var-registered command appears in help instead of being dropped"
    (let [help (cli/format-help tree)]
      (is (re-find #"- leaf" help))
      (is (re-find #"- sub inner" help))
      (is (re-find #"- sub" help)
          "a subtree carrying :_handler lists its own parent path too"))))

(deftest a-rebound-handler-takes-effect-through-a-table-built-earlier
  (testing "this is the reload, in miniature: the table predates the redefinition"
    (let [dispatch (cli/make-cli-handler tree)]
      (is (= "leaf" (text (dispatch {:command "leaf"}))))
      (with-redefs [leaf-handler (fn [_] {:type "text" :text "RELOADED"})]
        (is (= "RELOADED" (text (dispatch {:command "leaf"})))
            "the table holds the var, so the new root is what runs"))
      (testing "and the rebinding is undone, so the test leaves nothing behind"
        (is (= "leaf" (text (dispatch {:command "leaf"}))))))))

(deftest a-value-registered-handler-is-what-a-reload-cannot-reach
  (testing "the control: the same table built by VALUE does not see the rebinding"
    (let [dispatch (cli/make-cli-handler {:leaf leaf-handler})]
      (with-redefs [leaf-handler (fn [_] {:type "text" :text "RELOADED"})]
        (is (= "leaf" (text (dispatch {:command "leaf"})))
            "if this ever says RELOADED, value-capture stopped being a problem
             and the census probe is measuring nothing")))))
