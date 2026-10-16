(ns hive-mcp.emacs.client-contract-test
  "hive-mcp.emacs.client resolves evaluation from the hive-spi editor service
   registry. These tests publish a scripted editor there — the registry IS
   the port — and check every contracted fn against it, plus the
   no-editor-registered branch."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.emacs.client :as ec]
            [hive-mcp.emacs-ext.client :as ext-client]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.editor.services :as svc]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- scripted-editor
  "An editor that echoes the elisp it was asked to evaluate, times out on
   anything mentioning `slow`, and fails `boom` outright."
  [code _timeout-ms]
  (cond
    (str/includes? code "slow") {:success false :error "timed out" :timed-out true}
    (str/includes? code "boom") {:success false :error "boom"}
    (str/includes? code "buffer-file-name") {:success true :result "nil"}
    :else {:success true :result code}))

(defn- with-editor
  "Publish `scripted-editor` for the test, then restore whatever the service
   registry held before."
  [f]
  (let [prior (svc/registered)]
    (try
      (svc/reset-services!)
      (svc/register-services! {:eval-elisp-with-timeout scripted-editor})
      (ec/reset-symbol-strikes!)
      (f)
      (finally
        (ec/reset-symbol-strikes!)
        (svc/reset-services!)
        (doseq [[k caps] prior] (svc/register-services! k caps))))))

(use-fixtures :each with-editor)

(def ^:private SafeCode
  "Elisp-ish source that cannot trip the scripted editor's fault words."
  [:string {:gen/fmap #(str "(+ 1 " (str/replace % #"[^0-9 ]" "") ")")}])

(deftrifecta-from-schema eval-elisp-trifecta ec/eval-elisp
  {:in [:cat SafeCode]
   :out ext-client/EvalResult
   :rel (fn [[code] result] (= {:success true :result code} result))
   :num-tests 100})

(deftrifecta-from-schema eval-elisp!-trifecta ec/eval-elisp!
  {:in [:cat SafeCode]
   :out ext-client/ElispReturn
   :rel (fn [[code] result] (= code result))
   :num-tests 100})

(deftest eval-elisp!-failure-arms
  (testing "a timeout is a value, not a throw"
    (let [r (ec/eval-elisp! "slow")]
      (is (= {:error :timeout :msg "timed out"} r))
      (is (m/validate ext-client/ElispFailure r))
      (is (not (m/validate :string r)))))
  (testing "any other failure throws with the code attached"
    (let [e (try (ec/eval-elisp! "boom") nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= {:error "boom" :code "boom"} (ex-data e))))))

(deftest conveniences-build-the-documented-elisp
  (is (= "(mapcar #'buffer-name (buffer-list))" (ec/buffer-list)))
  (is (= "(buffer-name)" (ec/current-buffer)))
  (is (nil? (ec/current-file)) "\"nil\" from elisp is nil")
  (is (= "(with-current-buffer \"b\" (buffer-string))" (ec/buffer-content "b")))
  (is (= "(switch-to-buffer \"b\")" (ec/switch-to-buffer "b")))
  (is (= "(find-file \"/f\")" (ec/find-file "/f")))
  (is (= "(save-buffer)" (ec/save-buffer)))
  (is (= "(goto-line 7)" (ec/goto-line 7)))
  (is (= "(insert \"a\\\"b\")" (ec/insert-text "a\"b")))
  (is (= "(project-root (project-current))" (ec/project-root)))
  (is (= "recentf-list" (ec/recent-files)))
  (is (true? (ec/emacs-running?))))

(deftest probes-answer-the-three-states
  (svc/register-services!
   {:eval-elisp-with-timeout
    (fn [code _]
      (if (str/includes? code "'present")
        {:success true :result "t"}
        {:success false :error "void-function absent"}))})
  (is (true? (ec/probe-feature! 'present)))
  (is (true? (ec/probe-fboundp! 'present)))
  (dotimes [_ ec/symbol-void-strike-limit]
    (is (false? (ec/probe-fboundp! 'absent))))
  (is (= {'absent ec/symbol-void-strike-limit} (ec/symbol-strike-counts)))
  (is (= #{'absent} (ec/disabled-symbols)))
  (is (= :disabled (ec/probe-feature! 'absent)))
  (is (= #{} (ec/reset-symbol-strikes!)))
  (is (empty? (ec/symbol-strike-counts))))

(deftest no-editor-registered
  (svc/reset-services!)
  (let [r (ec/eval-elisp "(+ 1 2)")]
    (is (m/validate ext-client/EvalResult r))
    (is (false? (:success r)))
    (is (= :eval-elisp-with-timeout (:capability r))))
  (is (false? (ec/emacs-running?))))

(deftest-contract-coverage emacs-contracts
  {:paths ["src/hive_mcp/emacs"]})
