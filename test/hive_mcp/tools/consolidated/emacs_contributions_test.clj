(ns hive-mcp.tools.consolidated.emacs-contributions-test
  "The `emacs` root routes and advertises the commands addons contribute to it.

   Measured 2026-09-13: hive.emacs shipped `emacs answer` on its own tool def,
   named \"emacs\". The registry drops an addon tool that collides with a core
   root, so the verb was unreachable (`Unknown command`) and absent from the
   advertised enum, while the EMACS-ATTENTION block told agents to call it."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.tools.consolidated.emacs :as emacs]))

(def ^:private contributor :emacs-contributions-test)

(def ^:private contribution
  {"answer" {:handler (fn [params] {:type "text" :text (str "answered " (:keys params))})
             :params  {"keys" {:type "string" :description "kbd keys"}}}})

(deftest a-contributed-command-routes-through-the-emacs-root
  (ext/contribute-commands! "emacs" contributor contribution)
  (try
    (let [reply (emacs/handle-emacs {:command "answer" :keys "y"})]
      (is (= "answered y" (:text reply))))
    (finally (ext/retract-commands! "emacs" contributor))))

(deftest a-contributed-command-is-advertised-on-the-emacs-root
  (ext/contribute-commands! "emacs" contributor contribution)
  (try
    (let [schema (:inputSchema (routes/make-tool emacs/tool-def))]
      (testing "the verb joins the command enum"
        (is (some #{"answer"} (get-in schema [:properties "command" :enum]))))
      (testing "its params get a slot"
        (is (contains? (:properties schema) "keys")))
      (testing "core verbs are still there"
        (is (some #{"eval"} (get-in schema [:properties "command" :enum])))))
    (finally (ext/retract-commands! "emacs" contributor))))

(deftest core-commands-still-route-without-contributions
  (is (not (contains? (ext/get-contributed-commands "emacs") "answer"))
      "precondition: nothing contributed")
  (let [reply (emacs/handle-emacs {:command "nope"})]
    (is (re-find #"(?i)unknown" (str reply)))))
