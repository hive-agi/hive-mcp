(ns hive-mcp.test.stub.isolated-registry-test
  "Contract of `with-fresh-registries`: a polluted process registry is empty
   inside the fixture, and whatever the test registers inside is gone after,
   on normal and exceptional exit, with the prior contents restored exactly."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.extensions.departures :as departures]
            [hive-mcp.test.stub.isolated-registry :as iso]
            [hive-addon.registry.commands :as addon-cmds]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tool-def
  {:name        "iso-probe-tool"
   :description "probe"
   :inputSchema {:type "object" :properties {"x" {:type "string"}}}
   :handler     identity})

(defn- pollute!
  "Register one entry in every store, as a loaded addon would."
  [tag]
  (ext/register! (keyword "iso-probe" (name tag)) identity)
  (ext/register-tool! (assoc tool-def :name (str "iso-probe-" (name tag))))
  (ext/register-schema! tag "iso-probe-tool" {"p" {:type "string"}})
  (ext/contribute-commands! "iso-probe" tag {"cmd" {:handler identity :params {} :description "d"}})
  (departures/record! "iso-probe" tag ["gone"]))

(defn- with-polluted-registries
  "Run F with every store holding an :outer entry, restoring afterwards."
  [f]
  (iso/with-fresh-registries
    (fn []
      (pollute! :outer)
      (f))))

(deftest inside-is-empty
  (with-polluted-registries
    (fn []
      (is (not (iso/fresh-registries?)) "precondition: every store polluted")
      (iso/with-fresh-registries
        (fn []
          (is (iso/fresh-registries?))
          (is (nil? (ext/get-extension :iso-probe/outer)))
          (is (empty? (ext/get-registered-tools)))
          (is (nil? (ext/get-schema-extensions "iso-probe-tool")))
          (is (nil? (addon-cmds/get-commands "iso-probe")))
          (is (nil? (departures/departure "iso-probe" "gone"))))))))

(deftest inside-writes-do-not-leak
  (with-polluted-registries
    (fn []
      (let [before (iso/registry-state)]
        (iso/with-fresh-registries #(pollute! :inner))
        (is (= before (iso/registry-state)))
        (is (nil? (ext/get-extension :iso-probe/inner)))))))

(deftest restored-on-exception
  (with-polluted-registries
    (fn []
      (let [before (iso/registry-state)]
        (is (thrown-with-msg? clojure.lang.ExceptionInfo #"boom"
                              (iso/with-fresh-registries
                                (fn []
                                  (pollute! :inner)
                                  (ext/clear-all!)
                                  (pollute! :inner2)
                                  (throw (ex-info "boom" {}))))))
        (is (= before (iso/registry-state)))))))

(deftest every-store-is-covered
  (testing "the fixture names every store the registry facade writes"
    (iso/with-fresh-registries
      (fn []
        (pollute! :probe)
        (is (= #{:extensions :tools :schemas :contributions :departures}
               (set (keep (fn [[k v]] (when (seq v) k)) (iso/registry-state)))))))))
