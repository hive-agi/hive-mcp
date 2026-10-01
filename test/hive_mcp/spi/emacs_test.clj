;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.spi.emacs-test
  "With no Emacs host, a daemon question must answer nil. That is what the
   kernel sees once hive-emacs owns this code, so it is bound through
   `soft/*resolve*` rather than described."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.spi.emacs :as emacs]
            [hive-mcp.swarm.adapters.soft :as soft]))

(defn- clean [f]
  (emacs/uninstall!)
  (try (f) (finally (emacs/uninstall!))))

(use-fixtures :each clean)

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
                               (when (= "hive-mcp.emacs-ext.daemon-store" (namespace sym))
                                 (fn [& args]
                                   (swap! seen conj (vec (cons (symbol (name sym)) args)))
                                   nil)))]
      (emacs/reset-cache!)
      (emacs/select-daemon-for-ling "ling-1")
      (emacs/default-daemon-id)
      (is (= '[[select-daemon-for-ling "ling-1"]
               [default-daemon-id]]
             @seen)))))

(deftest an-installed-host-wins
  (let [host (atom [])]
    (binding [soft/*resolve* (fn [_] (fn [& _] (swap! host conj :host) nil))]
      (emacs/reset-cache!)
      (emacs/install! (reify
                        emacs/IEmacsDaemons
                        (-ensure-default-daemon! [_] :d)
                        (-select-daemon-for-ling [_ _] {:daemon-id :d})
                        (-bind-ling! [_ _ _] :bound)
                        (-unbind-ling! [_ _ _] :unbound)
                        (-get-daemon-for-ling [_ _] {:emacs-daemon/id :d})
                        (-default-daemon-id [_] :d)))
      (is (= {:daemon-id :d} (emacs/select-daemon-for-ling "ling-1")))
      (is (= :d (emacs/default-daemon-id)))
      (is (= [] @host) "nothing reached the late-bound host while one was installed")
      (testing "and uninstall! returns to the late-bound host"
        (emacs/uninstall!)
        (emacs/default-daemon-id)
        (is (= [:host] @host))))))
