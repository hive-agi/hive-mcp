(ns hive-mcp.tools.consolidated.memory-help-contributions-test
  "`memory help` lists what addons contributed to the memory tool, read from
   the SAME registry the dispatcher routes through (card
   20260925141924-394d8ea3, MEMORY-HELP-ADDON-CMDS).

   The stub addon is contributed through the real registry port
   (ext/contribute-commands!) and retracted afterwards; nothing is redefined.
   Each test asserts both halves: help names the command, and dispatch reaches
   the stub under that same name, so the two can't drift apart."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.consolidated.memory :as memory]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private stub-addon :memory-help-stub-addon)

(defn- recording-handler
  "A handler stub that records the :command it was called with."
  [calls tag]
  (fn [params]
    (swap! calls conj [tag (:command params)])
    {:type "text" :text (str "stub " tag)}))

(defn- with-stub-contribution
  "Run F with STUB-ADDON's commands contributed to the memory tool."
  [commands f]
  (ext/contribute-commands! "memory" stub-addon commands)
  (try (f)
       (finally (ext/retract-commands! "memory" stub-addon))))

(defn- help-lines []
  (set (map str/trim (str/split-lines (:text (memory/handle-memory {:command "help"}))))))

(deftest help-lists-a-contributed-flat-command
  (let [calls (atom [])]
    (with-stub-contribution
      {"zz-stub-seal" {:handler (recording-handler calls :flat)}}
      (fn []
        (testing "help names the contributed command"
          (is (contains? (help-lines) "- zz-stub-seal")))
        (testing "dispatch reaches it under the same name"
          (memory/handle-memory {:command "zz-stub-seal"})
          (is (= [[:flat "zz-stub-seal"]] @calls)))))))

(deftest help-lists-each-verb-of-a-contributed-subdomain
  (let [calls (atom [])]
    (with-stub-contribution
      {"zz-enclave" {:handler {:status (recording-handler calls :status)
                               :on     (recording-handler calls :on)}}}
      (fn []
        (testing "every verb of the verb map is listed as `<subdomain> <verb>`"
          (is (contains? (help-lines) "- zz-enclave status"))
          (is (contains? (help-lines) "- zz-enclave on")))
        (testing "dispatch routes the listed spelling"
          (memory/handle-memory {:command "zz-enclave on"})
          (is (= [:on "zz-enclave on"] (first @calls))))))))

(deftest help-drops-a-retracted-contribution
  (with-stub-contribution
    {"zz-stub-gone" {:handler (fn [_] {:type "text" :text "x"})}}
    (fn [] (is (contains? (help-lines) "- zz-stub-gone"))))
  (testing "help is re-read per call, so a retraction disappears from it"
    (is (not (contains? (help-lines) "- zz-stub-gone")))))

(deftest help-keeps-the-core-commands
  (with-stub-contribution
    {"zz-stub-seal" {:handler (fn [_] nil)}}
    (fn []
      (is (every? (help-lines) ["- add" "- search" "- kg edge"])
          "a contribution adds to core's commands and replaces none of them"))))
