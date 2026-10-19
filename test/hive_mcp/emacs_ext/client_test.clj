(ns hive-mcp.emacs-ext.client-test
  "The `:emacs/*` client façade forwards to the registered emacsclient and,
   when hive-emacs is not loaded, answers a uniformly-shaped failure for the
   eval paths and nil for the conveniences. The emacsclient is the
   scriptable stub Emacs (hive-mcp.test.stub.emacs-ext)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.emacs-ext.client :as client]
            [hive-mcp.test.stub.emacs-ext :as stub]
            [hive-mcp.test.stub.extensions :as ext-stub]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each stub/with-emacs)

(deftrifecta-from-schema eval-elisp-trifecta client/eval-elisp
  {:in [:cat :string]
   :out client/EvalResult
   ;; the default stub answers every form with elisp's nil
   :rel (fn [[_] result] (= {:success true :result "nil"} result))
   :num-tests 100})

(deftrifecta-from-schema eval-elisp-with-timeout-trifecta client/eval-elisp-with-timeout
  {:in [:cat :string [:maybe [:int {:min 0 :max 60000}]]]
   :out client/EvalResult
   :rel (fn [[_ _] result] (true? (:success result)))
   :num-tests 100})

(deftest scripted-faults-reach-the-caller-unchanged
  (stub/with-stub-emacs [_ {:responses [["slow" {:success false :error "timed out" :timed-out true}]
                                        ["(+ 1 2)" "3"]]}]
    (is (= {:success true :result "3"} (client/eval-elisp "(+ 1 2)")))
    (let [r (client/eval-elisp-with-timeout "slow" 10)]
      (is (m/validate client/EvalResult r))
      (is (true? (:timed-out r))))))

(deftest conveniences-forward-the-editor-state
  (stub/with-stub-emacs [_ {:current-file "/a.clj" :project-root "/p"
                            :buffers ["*scratch*" "a.clj"] :recent-files ["/b.clj"]}]
    (is (true? (client/emacs-running?)))
    (is (= ["*scratch*" "a.clj"] (client/buffer-list)))
    (is (= "/a.clj" (client/current-file)))
    (is (= "/p" (client/project-root)))
    (is (= ["/b.clj"] (client/recent-files)))
    (is (true? (client/switch-to-buffer "a.clj")))
    (is (= "a.clj" (client/current-buffer)))
    (is (true? (client/find-file "/c.clj")))
    (is (true? (client/goto-line 3)))
    (is (true? (client/insert-text "x")))
    (is (true? (client/save-buffer)))))

(deftest facade-return-arms-are-distinct
  (is (m/validate client/FacadeReturn "\"*scratch*\""))
  (is (m/validate client/FacadeReturn {:error :timeout :msg "slow"}))
  (is (not (m/validate client/ElispFailure "x")))
  (is (not (m/validate client/FacadeReturn {:error :other :msg "x"}))))

(deftest without-hive-emacs
  (ext-stub/without-extensions
   stub/ext-keys
   (fn []
     (testing "the eval paths answer a failure map naming the missing key"
       (let [r (client/eval-elisp "(+ 1 2)")]
         (is (false? (:success r)))
         (is (str/includes? (:error r) ":emacs/eval-elisp")))
       (is (m/validate client/EvalResult (client/eval-elisp-with-timeout "t"))))
     (testing "the conveniences answer nil and emacs-running? false"
       (is (false? (client/emacs-running?)))
       (is (nil? (client/buffer-list)))
       (is (nil? (client/current-file)))
       (is (nil? (client/goto-line 1)))))))

(deftest-contract-coverage emacs-ext-contracts
  {:paths ["src/hive_mcp/emacs_ext"]})
