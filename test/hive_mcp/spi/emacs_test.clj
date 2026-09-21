;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.emacs-test
  "With no Emacs host, an elisp eval must FAIL in the shape callers already
   destructure, and a daemon question must answer nil. Both are what the
   kernel sees once hive-emacs owns this code, so both are bound through
   `soft/*resolve*` rather than described."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.spi.emacs :as emacs]
            [hive-mcp.swarm.adapters.soft :as soft]))

(defn- clean [f]
  (emacs/uninstall!)
  (try (f) (finally (emacs/uninstall!))))

(use-fixtures :each clean)

(deftest with-no-emacs-an-eval-fails-in-the-callers-shape
  (binding [soft/*resolve* (constantly nil)]
    (emacs/reset-cache!)
    (let [{:keys [success result error]} (emacs/eval-elisp "(featurep 'magit)")]
      (is (false? success) "a probe reads as absent, it does not throw")
      (is (nil? result))
      (is (string? error)))
    (is (= emacs/no-host-result (emacs/eval-elisp-with-timeout "(+ 1 1)" 50)))
    (is (false? (emacs/emacs-running?)))))

(deftest with-no-emacs-every-daemon-question-answers-nil
  (binding [soft/*resolve* (constantly nil)]
    (emacs/reset-cache!)
    (is (nil? (emacs/ensure-default-daemon!)))
    (is (nil? (emacs/select-daemon-for-ling "ling-1")))
    (is (nil? (emacs/bind-ling! "d1" "ling-1")))
    (is (nil? (emacs/unbind-ling! "d1" "ling-1")))
    (is (nil? (emacs/get-daemon-for-ling "ling-1")))
    (is (nil? (emacs/default-daemon-id)))))

(deftest the-port-late-binds-to-the-host-by-symbol
  (let [seen (atom [])]
    (binding [soft/*resolve* (fn [sym]
                               (when (#{"hive-mcp.emacs-ext.client"
                                        "hive-mcp.emacs-ext.daemon-store"} (namespace sym))
                                 (fn [& args]
                                   (swap! seen conj (vec (cons (symbol (name sym)) args)))
                                   {:success true :result "t"})))]
      (emacs/reset-cache!)
      (emacs/eval-elisp "(featurep 'magit)")
      (emacs/select-daemon-for-ling "ling-1")
      (is (= '[[eval-elisp "(featurep 'magit)"]
               [select-daemon-for-ling "ling-1"]]
             @seen)))))

(deftest an-installed-host-wins
  (let [host (atom [])]
    (binding [soft/*resolve* (fn [_] (fn [& _] (swap! host conj :host) nil))]
      (emacs/reset-cache!)
      (emacs/install! (reify
                        emacs/IElispEval
                        (-eval-elisp [_ _] {:success true :result "installed"})
                        (-eval-elisp-with-timeout [_ _ _] {:success true :result "installed"})
                        (-emacs-running? [_] true)
                        emacs/IEmacsDaemons
                        (-ensure-default-daemon! [_] :d)
                        (-select-daemon-for-ling [_ _] {:daemon-id :d})
                        (-bind-ling! [_ _ _] :bound)
                        (-unbind-ling! [_ _ _] :unbound)
                        (-get-daemon-for-ling [_ _] {:emacs-daemon/id :d})
                        (-default-daemon-id [_] :d)))
      (is (= "installed" (:result (emacs/eval-elisp "(+ 1 1)"))))
      (is (true? (emacs/emacs-running?)))
      (is (= {:daemon-id :d} (emacs/select-daemon-for-ling "ling-1")))
      (is (= [] @host) "nothing reached the late-bound host while one was installed")
      (testing "and uninstall! returns to the late-bound host"
        (emacs/uninstall!)
        (emacs/eval-elisp "(+ 1 1)")
        (is (= [:host] @host))))))
