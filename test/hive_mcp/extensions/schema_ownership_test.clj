(ns hive-mcp.extensions.schema-ownership-test
  "Schema extensions are OWNED (seam S7).

   Three layers, pinned separately:

     pure      effective-schema-properties / contribute-schema /
               retract-schema-owner, as a trifecta (golden + property +
               mutation) over plain ledger values
     registry  register-schema! under two owners, retract one, and the read
               API still answers the shape callers have always read
     shutdown  addons.core/shutdown-addon! withdraws an addon's params, driven
               through a reified IAddon stub and a recording listener: no
               with-redefs, only the ports."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addons]
            [hive-mcp.extensions.registry :as ext]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (ext/clear-all-schemas!)
    (addons/reset-registry!)
    (try (f)
         (finally (ext/clear-all-schemas!)
                  (addons/reset-registry!)))))

;; =============================================================================
;; Pure stratum: the read-time merge
;; =============================================================================

(def ^:private gen-param (gen/elements ["path" "limit" "scope" "force" "mode"]))

(def ^:private gen-props
  (gen/map gen-param (gen/fmap (fn [t] {:type t}) (gen/elements ["string" "integer" "boolean"]))
           {:max-elements 4}))

(def ^:private gen-owner (gen/elements [:a :b :c "hive.rss" ext/default-schema-owner]))

(def ^:private gen-ops
  "A registration history: [[owner props] ...] against one tool."
  (gen/vector (gen/tuple gen-owner gen-props) 0 8))

(defn- ledger-of
  [ops]
  (reduce (fn [l [o p]] (ext/contribute-schema l o "tool" p)) {} ops))

(defn run-effective
  "Unary adapter for the trifecta: a registration history -> advertised props."
  [ops]
  (ext/effective-schema-properties (get (ledger-of ops) "tool")))

(deftrifecta effective-schema-properties-contract
  hive-mcp.extensions.schema-ownership-test/run-effective
  {:golden-path "test/golden/extensions/schema-ownership-effective.edn"
   :cases       {:empty          []
                 :one-owner      [[:a {"path" {:type "string"}}]]
                 :two-owners     [[:a {"path" {:type "string"}}]
                                  [:b {"limit" {:type "integer"}}]]
                 :later-wins     [[:a {"path" {:type "string"}}]
                                  [:b {"path" {:type "integer"}}]]
                 :re-register    [[:a {"path" {:type "string"}}]
                                  [:b {"path" {:type "integer"}}]
                                  [:a {"mode" {:type "string"}}]]
                 :anon-shadowed  [[:a {"path" {:type "string"}}]
                                  [ext/default-schema-owner {"path" {:type "string"}}]]}
   :gen         gen-ops
   :pred        #(or (nil? %) (map? %))
   :num-tests   200
   :mutations   [["first-wins — merge order reversed"
                  (fn [ops]
                    (let [l (reduce (fn [l [o p]] (ext/contribute-schema l o "tool" p)) {} ops)]
                      (some->> (vals (get l "tool")) (sort-by :seq) reverse (map :props)
                               (apply merge) not-empty)))]
                 ["empty-map-not-nil"
                  (fn [ops]
                    (let [l (reduce (fn [l [o p]] (ext/contribute-schema l o "tool" p)) {} ops)]
                      (apply merge {} (map :props (vals (get l "tool"))))))]
                 ["drops-all" (constantly nil)]]
   :assert      (fn []
                  (is (= {"path" {:type "integer"}}
                         (run-effective [[:a {"path" {:type "string"}}]
                                         [:b {"path" {:type "integer"}}]]))
                      "on a clash the later registration wins")
                  (is (nil? (run-effective [])) "nothing contributed reads as nil"))})

(defspec retracting-an-owner-equals-never-registering-it 200
  (prop/for-all [ops (gen/vector (gen/tuple (gen/elements [:a :b :c "hive.rss"]) gen-props) 0 8)
                 gone (gen/elements [:a :b :c "hive.rss"])]
    ;; The property S7 exists for, over NAMED owners: after retraction nothing
    ;; the owner gave is advertised, and every other owner's params read
    ;; exactly as if the owner had never been there. (Anonymous registrations
    ;; are deliberately NOT independent of named ones; see the next spec.)
    (let [with    (ext/retract-schema-owner (ledger-of ops) gone)
          without (ledger-of (remove #(= gone (first %)) ops))]
      (= (ext/effective-schema-properties (get with "tool"))
         (ext/effective-schema-properties (get without "tool"))))))

(defspec anonymous-copies-never-outlive-their-owner 200
  (prop/for-all [props gen-props
                 anon-first? gen/boolean]
    ;; A re-drain republishes an addon's params with no owner. Whichever
    ;; arrives first, retracting the addon leaves nothing behind.
    (let [ops (if anon-first?
                [[ext/default-schema-owner props] ["hive.rss" props]]
                [["hive.rss" props] [ext/default-schema-owner props]])]
      (nil? (ext/effective-schema-properties
             (get (ext/retract-schema-owner (ledger-of ops) "hive.rss") "tool"))))))

;; =============================================================================
;; Registry boundary
;; =============================================================================

(deftest two-owners-retract-one-test
  (ext/register-schema! :carto "code" {"scope" {:type "string"}})
  (ext/register-schema! :kondo "code" {"lint_level" {:type "string"}})
  (ext/register-schema! :kondo "analysis" {"config" {:type "string"}})
  (testing "the read API merges every owner, same shape as before"
    (is (= {"scope" {:type "string"} "lint_level" {:type "string"}}
           (ext/get-schema-extensions "code"))))
  (testing "ownership is inspectable"
    (is (= {:carto {"scope" {:type "string"}} :kondo {"lint_level" {:type "string"}}}
           (ext/schema-contributions "code"))))
  (testing "retracting one owner withdraws only its params, across tools"
    (is (= ["analysis" "code"] (ext/retract-schemas-by-owner! :kondo)))
    (is (= {"scope" {:type "string"}} (ext/get-schema-extensions "code")))
    (is (nil? (ext/get-schema-extensions "analysis")) "an emptied tool reads nil"))
  (testing "retracting an owner with nothing is a no-op"
    (is (= [] (ext/retract-schemas-by-owner! :absent)))))

(deftest retraction-restores-the-shadowed-value-test
  (ext/register-schema! :a "tool" {"p" {:type "string"}})
  (ext/register-schema! :b "tool" {"p" {:type "integer"}})
  (is (= "integer" (get-in (ext/get-schema-extensions "tool") ["p" :type])))
  (ext/retract-schemas-by-owner! :b)
  (is (= "string" (get-in (ext/get-schema-extensions "tool") ["p" :type]))))

(deftest retraction-notifies-the-surface-test
  (let [seen (atom [])]
    (ext/add-contribution-listener! ::recorder #(swap! seen conj %))
    (try
      (ext/register-schema! :probe "code" {"x" {:type "string"}})
      (ext/retract-schemas-by-owner! :probe)
      (is (= [{:type :retract-schema :tool-name "code" :addon-id :probe}] @seen))
      (finally (ext/remove-contribution-listener! ::recorder)))))

;; =============================================================================
;; Shutdown wires the retraction
;; =============================================================================

(defn- schema-addon
  "A reified IAddon stub contributing SCHEMA-EXTS."
  [id schema-exts]
  (reify proto/IAddon
    (addon-id [_] id)
    (addon-type [_] :native)
    (capabilities [_] #{})
    (initialize! [_ _] {:success? true :errors [] :metadata {}})
    (shutdown! [_] {:success? true :errors []})
    (tools [_] [])
    (schema-extensions [_] schema-exts)
    (health [_] {:status :ok})))

(deftest shutdown-withdraws-the-addons-schema-test
  (addons/register-addon! (schema-addon :rss-probe {"memory" {"rss-url" {:type "string"}}}))
  (addons/register-addon! (schema-addon :other {"memory" {"keg" {:type "string"}}}))
  (addons/init-addon! :rss-probe)
  (addons/init-addon! :other)
  (is (= #{"rss-url" "keg"} (set (keys (ext/get-schema-extensions "memory")))))
  (testing "shutdown takes back exactly what the addon gave"
    (addons/shutdown-addon! :rss-probe)
    (is (= #{"keg"} (set (keys (ext/get-schema-extensions "memory"))))))
  (testing "re-init advertises it again"
    (addons/init-addon! :rss-probe)
    (is (= #{"rss-url" "keg"} (set (keys (ext/get-schema-extensions "memory")))))))
