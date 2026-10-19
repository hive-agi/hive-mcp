(ns hive-mcp.extensions.registry-contract-test
  "Contract facets for the extension registry: the pure schema-ledger reads,
   and round trips through the global slots that clean up after themselves."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.registry :as ext]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Owner [:enum :addon/a :addon/b "addon-c"])
(def ^:private Ledger
  "A schema ledger over three tools and three owners. Built from optional map
   keys rather than :map-of, whose distinct-key generator spins on a domain
   this small."
  (let [props [:map ["p" {:optional true} [:map [:type [:= "string"]]]]
               ["q" {:optional true} [:map [:type [:= "string"]]]]]
        contribution [:map [:seq [:int {:min 0 :max 9}]] [:props props]]
        contribs [:map [:addon/a {:optional true} contribution]
                  [:addon/b {:optional true} contribution]
                  ["addon-c" {:optional true} contribution]]]
    [:map ["memory" {:optional true} contribs]
     ["kanban" {:optional true} contribs]
     ["code" {:optional true} contribs]]))

(deftrifecta-from-schema schema-owner-tools-lists-what-an-owner-touches
  #'ext/schema-owner-tools
  {:in [:cat Ledger Owner]
   :out [:vector [:enum "memory" "kanban" "code"]]
   :rel (fn [[ledger owner] out]
          (= (vec (sort (keep (fn [[tool contribs]] (when (contains? contribs owner) tool)) ledger)))
             out))
   :golden-path "test/golden/hive-mcp/extensions/schema-owner-tools.edn"
   :num-tests 200})

(deftest register-round-trips-through-the-extension-slot
  (let [k ::m11-probe f (fn [x] [:probed x])]
    (try
      (is (= k (ext/register! k f)))
      (is (true? (ext/extension-available? k)))
      (is (contains? (ext/registered-keys) k))
      (is (= [::m11-many] (vec (ext/register-many! {::m11-many identity}))))
      (is (= [:probed 1] ((ext/get-extension k) 1)))
      (is (= k (ext/deregister! k)))
      (is (false? (ext/extension-available? k)))
      (finally (ext/deregister! k) (ext/deregister! ::m11-many)))))

(deftest schemas-are-owned-and-retracted-by-owner
  (let [tool "m11-probe-tool" owner :m11/probe-owner]
    (try
      (is (= tool (ext/register-schema! owner tool {"p" {:type "string"}})))
      (is (= {owner {"p" {:type "string"}}} (ext/schema-contributions tool)))
      (is (= {"p" {:type "string"}} (ext/get-schema-extensions tool)))
      (is (= [tool] (ext/retract-schemas-by-owner! owner)))
      (is (nil? (ext/get-schema-extensions tool)))
      (finally (ext/retract-schemas-by-owner! owner)))))

(deftest a-tool-round-trips-through-the-tool-slot
  (let [tool {:name "m11-probe-dynamic" :description "probe" :handler identity
              :inputSchema {:type "object" :properties {"x" {:type "string"}}}}]
    (try
      (is (= "m11-probe-dynamic" (ext/register-tool! tool)))
      (is (some #(= "m11-probe-dynamic" (:name %)) (ext/get-registered-tools)))
      (is (= "m11-probe-dynamic" (ext/deregister-tool! "m11-probe-dynamic")))
      (is (not-any? #(= "m11-probe-dynamic" (:name %)) (ext/get-registered-tools)))
      (finally (ext/deregister-tool! "m11-probe-dynamic")))))

(deftest a-listener-sees-a-contribution-and-its-retraction
  (let [seen (atom []) tool "m11-probe-composite"]
    (try
      (is (= ::m11-listener (ext/add-contribution-listener! ::m11-listener #(swap! seen conj (:type %)))))
      (is (boolean? (ext/ensure-seam-listener!)))
      (is (contains? (ext/contribute-commands! tool :m11/addon {"go" {:handler identity}}) tool))
      (is (empty? (get (ext/retract-commands! tool :m11/addon) tool)))
      (is (map? (ext/retract-all-by-addon! :m11/addon)))
      (is (= [:contribute :retract] (take 2 @seen)))
      (finally
        (ext/remove-contribution-listener! ::m11-listener)
        (ext/retract-all-by-addon! :m11/addon)))))
