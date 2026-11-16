(ns hive-mcp.tools.magit-git-editor-chain-test
  "The :git-editor transport chain [:hive-git :emacs] behind the magit verbs.

   Emacs absent: no :vessel capability is registered, and status, stage and
   commit still work, served by the :hive-git provider on plain git.
   LSP: the :hive-git and the :vessel providers answer the same capability
   envelope, so a verb returns the same text whichever provider served it."
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.transport.registry :as reg]
            [hive-spi.editor.services :as svc]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shipped {:defaults {:retry {:attempts 1 :backoff-ms [0]} :probe-interval-ms 0}
                        :git-editor {:chain [:hive-git :emacs]}})

(use-fixtures :each
  (fn [t]
    (let [prior (get (svc/registered) :vessel)
          sink  (reg/set-switch-sink! (fn [_]))]
      (svc/unregister-services! :vessel)
      (reg/clear!)
      (reg/rebind! shipped)
      (try (t)
           (finally
             (reg/clear!)
             (reg/set-switch-sink! sink)
             (when (seq prior) (svc/register-services! :vessel prior)))))))

(defn- temp-repo []
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "magit-chain" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (doseq [args [["init" "-q"] ["config" "user.name" "test"]
                  ["config" "user.email" "test@example.invalid"]
                  ["config" "commit.gpgsign" "false"]]]
      (apply sh/sh "git" (concat args [:dir dir])))
    (spit (str dir "/a.txt") "a\n")
    dir))

(defn- active [] (get-in (reg/status) [:ports :git-editor :active]))

(deftest verbs-work-with-the-emacs-provider-absent
  (let [dir (temp-repo)]
    (is (not (svc/available? :vessel :dispatch)) "no editor vessel is registered")
    (testing "status"
      (let [r (magit/handle-magit-status {:directory dir})
            body (json/read-str (:text r) :key-fn keyword)]
        (is (nil? (:isError r)) (:text r))
        (is (= "git-cli" (:backend body)))
        (is (= [] (:staged body)))
        (is (= ["a.txt"] (:untracked body)))))
    (testing "stage"
      (let [r (magit/handle-magit-stage {:directory dir :files ["a.txt"]})]
        (is (nil? (:isError r)) (:text r))
        (is (= "a.txt" (str/trim (:out (sh/sh "git" "diff" "--cached" "--name-only" :dir dir)))))))
    (testing "commit"
      (let [r (magit/handle-magit-commit {:directory dir :message "first" :files ["a.txt"]})]
        (is (nil? (:isError r)) (:text r))
        (is (= "first" (str/trim (:out (sh/sh "git" "log" "-1" "--format=%s" :dir dir)))))))
    (testing "branches and log"
      (is (nil? (:isError (magit/handle-magit-branches {:directory dir}))))
      (is (= "first" (:subject (first (json/read-str
                                       (:text (magit/handle-magit-log {:directory dir :count 1}))
                                       :key-fn keyword))))))
    (is (= :hive-git (active)) "the chain elected plain git")))

(deftest a-failed-op-is-the-ops-own-error-and-does-not-demote-the-provider
  (let [dir (temp-repo)
        r (magit/handle-magit-commit {:directory dir :message "nothing"})]
    (is (true? (:isError r)))
    (is (str/includes? (:text r) ":magit/commit-failed"))
    (is (not= :degraded (get-in (reg/status) [:ports :git-editor :adapters :hive-git :status])))
    (is (nil? (get-in (reg/status) [:ports :git-editor :adapters :emacs :last-ok-at]))
        "the editor was never asked to rerun it")))

(def ^:private vessel-stub
  "A vessel that serves whatever plain git answers, as the hive-emacs
   translators do, through the same capability envelope."
  {:dispatch (fn [op timeout-ms]
               ((get-in (svc/registered) [:hive-git :dispatch]) op timeout-ms))})

(deftest both-providers-answer-the-same-text
  (let [dir (temp-repo)
        ops {:status #(magit/handle-magit-status {:directory dir})
             :branches #(magit/handle-magit-branches {:directory dir})
             :log #(magit/handle-magit-log {:directory dir :count 3})
             :diff #(magit/handle-magit-diff {:directory dir :target "all"})
             :feature-branches #(magit/handle-magit-feature-branches {:directory dir})}
        via-hive-git (update-vals ops #(:text (%)))]
    (reg/clear!)
    (reg/rebind! {:defaults (:defaults shipped) :git-editor {:chain [:emacs]}})
    (svc/register-services! :vessel vessel-stub)
    (let [via-emacs (update-vals ops #(:text (%)))]
      (is (= via-hive-git via-emacs))
      (is (= :emacs (active))))))

(deftest an-op-plain-git-cannot-run-falls-back-to-the-editor
  (svc/register-services! :vessel {:dispatch (fn [_ _] {:success true :timed-out false :result "from-editor"})})
  (let [r (reg/call! :git-editor {:op {:op :magit/eval :directory "/x"} :timeout-ms nil})]
    (is (= :emacs (get-in r [:ok :via])))
    (is (= :fallback (get-in r [:ok :resolved-by])))
    (is (= [:hive-git] (mapv :adapter (get-in r [:ok :skipped]))))
    (is (= "from-editor" (get-in r [:ok :value])))))

(deftest the-chain-is-registered-for-transport-status
  (let [view (get-in (reg/status) [:ports :git-editor])]
    (is (= [:hive-git :emacs] (:chain view)))
    (is (every? :registered? (vals (:adapters view))))
    (is (true? (get-in view [:adapters :hive-git :probe?])))))
