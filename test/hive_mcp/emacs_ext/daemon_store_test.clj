(ns hive-mcp.emacs-ext.daemon-store-test
  "The daemon-store façade forwards ling<->daemon bookkeeping to the
   registered store, and degrades to nil when hive-emacs is not loaded.
   The store is the recording stub Emacs (hive-mcp.test.stub.emacs-ext)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.emacs-ext.daemon-store :as ds]
            [hive-mcp.test.stub.emacs-ext :as stub]
            [hive-mcp.test.stub.extensions :as ext-stub]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each stub/with-emacs)

(deftrifecta-from-schema bind-then-lookup-trifecta ds/bind-ling!
  {:in [:cat ds/DaemonId [:string {:min 1}]]
   :out ds/TxReport
   :rel (fn [[daemon-id slave-id] _]
          ;; the binding is observable through the getter
          (= daemon-id (ds/get-daemon-for-ling slave-id)))
   :num-tests 100})

(deftest unbind-forgets-the-ling
  (ds/bind-ling! "server" "ling-1")
  (is (= "server" (ds/get-daemon-for-ling "ling-1")))
  (ds/unbind-ling! "server" "ling-1")
  (is (nil? (ds/get-daemon-for-ling "ling-1"))))

(deftest defaults-and-selection-forward-the-store
  (is (= "daemon-0" (ds/default-daemon-id)))
  (is (m/validate ds/TxReport (ds/ensure-default-daemon!)))
  (testing "swarm.sync passes the selector's opts map, not a ling id"
    (is (m/validate ds/Selection (ds/select-daemon-for-ling {:project-id "p"})))))

(deftest selection-arms-are-distinct
  (let [full {:daemon-id "server" :reason :selected}]
    (is (m/validate ds/Selection full))
    (is (m/validate ds/Selection "server"))
    (is (not (m/validate ds/Selection {:daemon-id "server"})))
    (is (not (m/validate ds/DaemonId full)))))

(deftest without-hive-emacs-every-answer-is-nil
  (ext-stub/without-extensions
   stub/ext-keys
   (fn []
     (is (nil? (ds/ensure-default-daemon!)))
     (is (nil? (ds/select-daemon-for-ling {:project-id "p"})))
     (is (nil? (ds/bind-ling! "d" "s")))
     (is (nil? (ds/unbind-ling! "d" "s")))
     (is (nil? (ds/get-daemon-for-ling "s")))
     (is (nil? (ds/default-daemon-id))))))
