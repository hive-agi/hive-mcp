(ns hive-mcp.tools.catchup.git-contract-test
  "catchup.git through its *sh* port: git answered by a stub table, never a
   repository and never with-redefs."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.catchup.git :as git]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- sh-stub
  "A shell port answering git from a table keyed by the git subcommand."
  [answers]
  (fn [_git & args]
    (let [k (first (filter #{"rev-parse" "status" "log"} args))]
      (get answers k {:exit 128 :out "" :err "fatal"}))))

(def ^:private Answers
  "One answer per git subcommand, each a success or a failure."
  [:map
   ["rev-parse" [:or [:map [:exit [:= 0]] [:out [:enum "main\n" "feat/x\n"]]]
                 [:map [:exit [:= 128]] [:out [:= ""]]]]]
   ["status" [:or [:map [:exit [:= 0]] [:out [:enum "" " M a.clj\n"]]]
              [:map [:exit [:= 128]] [:out [:= ""]]]]]
   ["log" [:or [:map [:exit [:= 0]] [:out [:enum "abc1234 - init\n" "  d - x  \n"]]]
           [:map [:exit [:= 128]] [:out [:= ""]]]]]])

(defn run-with-answers
  "Unary adapter: gather-git-info with the *sh* port bound to ANSWERS."
  [answers]
  (binding [git/*sh* (sh-stub answers)]
    (git/gather-git-info "/repo")))

(deftrifecta-from-schema gather-git-info-per-field-fallback
  hive-mcp.tools.catchup.git-contract-test/run-with-answers
  {:in          Answers
   :out         git/GitInfo
   :rel         (fn [a out]
                  (let [ok? #(zero? (:exit (get a %)))]
                    (and (= (:branch out) (if (ok? "rev-parse") (str/trim (:out (get a "rev-parse"))) "none"))
                         (= (:uncommitted out) (boolean (and (ok? "status")
                                                             (not (str/blank? (:out (get a "status")))))))
                         (= (:last-commit out) (if (ok? "log") (str/trim (:out (get a "log"))) "none")))))
   :golden-path "test/golden/catchup/git-info-answers.edn"
   :num-tests   200})

(deftest-golden git-info-through-the-port
  "test/golden/catchup/git-info-port.edn"
  (into (sorted-map)
        (for [[label answers] {:clean   {"rev-parse" {:exit 0 :out "main\n"}
                                         "status"    {:exit 0 :out ""}
                                         "log"       {:exit 0 :out "abc1234 - init\n"}}
                               :dirty   {"rev-parse" {:exit 0 :out "feat/x\n"}
                                         "status"    {:exit 0 :out " M a.clj\n"}
                                         "log"       {:exit 0 :out "def5678 - wip\n"}}
                               :no-repo {}}]
          [label (run-with-answers answers)])))

(deftest git-port-throwing-falls-back
  (binding [git/*sh* (fn [& _] (throw (RuntimeException. "no shell")))]
    (is (m/validate git/GitInfo (git/gather-git-info "/repo")))))
