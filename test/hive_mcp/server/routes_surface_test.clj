(ns hive-mcp.server.routes-surface-test
  "Every transport reads ONE advertised table: a refresh computes it once and
   installs it into every registered surface, whatever kind each one is."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.server.routes :as routes]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; -----------------------------------------------------------------------------
;; Stub transports. Two are new surface KINDS registered the way a new
;; transport would register one (a defmethod), one is the stock :tools-atom.
;; -----------------------------------------------------------------------------

(def ^:private installs (atom []))

(defmethod routes/install-table! ::recording
  [{::keys [id]} table]
  (swap! installs conj [id table])
  true)

(defmethod routes/install-table! ::faulty
  [_ _]
  (throw (ex-info "transport down" {})))

(def ^:private test-ids [::a ::b ::faulty ::atom-backed ::no-context])

(defn- isolate [f]
  (let [before (routes/registered-surfaces)]
    (reset! installs [])
    (try (f)
         (finally
           (doseq [id test-ids] (routes/unregister-surface! id))
           (is (= (keys before) (keys (routes/registered-surfaces))))))))

(use-fixtures :each isolate)

(defn- names [table] (set (keys table)))

(deftest one-table-reaches-every-surface
  (let [tools-atom (atom {"stale" {:tool {:name "stale"} :handler identity}})]
    (routes/register-surface! ::a {:surface/kind ::recording ::id ::a})
    (routes/register-surface! ::b {:surface/kind ::recording ::id ::b})
    (routes/register-surface! ::atom-backed {:surface/kind :tools-atom
                                             :surface/tools-atom tools-atom})
    (let [out           (routes/refresh-surfaces!)
          [[_ ta] [_ tb]] (sort-by (comp str first) @installs)]
      (testing "every stub transport got the very same table"
        (is (= 2 (count @installs)))
        (is (identical? ta tb))
        (is (= ta @tools-atom) "the stock atom surface holds it too"))
      (testing "the table replaced, not merged into, what the surface held"
        (is (not (contains? @tools-atom "stale"))))
      (testing "the report names the unique count and the surfaces reached"
        (is (= (count ta) (:count out)))
        (is (= (count (names ta)) (:count out)) "no name twice")
        (is (every? (set (:surfaces out)) [::a ::b ::atom-backed])))
      (testing "the table is the advertised defs, one entry per name"
        (is (= (set (map :name (routes/advertised-tool-defs))) (names ta)))))))

(deftest a-failing-surface-is-reported-and-does-not-stop-the-others
  (routes/register-surface! ::faulty {:surface/kind ::faulty})
  (routes/register-surface! ::a {:surface/kind ::recording ::id ::a})
  (let [out (routes/refresh-surfaces!)]
    (is (some #{::faulty} (:failed out)))
    (is (some #{::a} (:surfaces out)))
    (is (= 1 (count @installs)))))

(deftest a-context-surface-without-a-context-is-skipped
  (routes/register-surface! ::no-context {:surface/kind :context-atom
                                          :surface/context-atom (atom nil)})
  (is (not-any? #{::no-context} (:surfaces (routes/refresh-surfaces!)))))

(deftest changed-names-are-the-diff-of-two-tables
  (is (= #{"added" "changed" "removed"}
         (routes/changed-tool-names {"same" {:d 1} "changed" {:d 1} "removed" {:d 1}}
                                    {"same" {:d 1} "changed" {:d 2} "added" {:d 1}})))
  (is (empty? (routes/changed-tool-names {"a" {:d 1}} {"a" {:d 1}}))))

(deftest boot-spec-and-refresh-agree
  (testing "a transport created from build-server-spec starts with the table a refresh installs"
    (routes/register-surface! ::a {:surface/kind ::recording ::id ::a})
    (let [spec-names (set (map :name (:tools (routes/build-server-spec))))
          _          (routes/refresh-surfaces!)
          [[_ table]] @installs]
      (is (= spec-names (names table))))))
