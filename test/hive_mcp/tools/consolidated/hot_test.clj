(ns hive-mcp.tools.consolidated.hot-test
  "The `hot` verbs' pure strata and their lifecycle seam (S14, S15).

   Pure: reload-all-seeds (seed filtering), no-reload-split, pin-decl,
   dormant-pin-note. Seam: lifecycle verbs run against a REAL
   hive-addon.lifecycle manager built over a reified ILifecycleHost stub, so
   the test depends on the port and never on a concretion (no with-redefs)."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.lifecycle :as lc]
            [hive-addon.lifecycle.port :as lport]
            [hive-mcp.tools.consolidated.hot :as hot]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

;; =============================================================================
;; reload-all seed filtering
;; =============================================================================

(def ^:private gen-row
  (gen/let [id   (gen/fmap #(str "addon." %) (gen/choose 0 30))
            kind (gen/elements [:directory :jar :absent :weird])]
    {:addon/id id :hot/source-kind kind}))

(def ^:private gen-plan
  (gen/fmap (fn [rows]
              (let [rows (vals (into {} (map (juxt :addon/id identity)) rows))
                    {ok true no false} (group-by #(= :directory (:hot/source-kind %)) rows)]
                {:hot/registered (vec ok) :hot/skipped (vec no)}))
            (gen/vector gen-row 0 12)))

(defspec reload-all-seeds-partitions-the-plan 200
  (prop/for-all [plan gen-plan]
    (let [{:keys [seeds skipped]} (hot/reload-all-seeds plan)
          skipped-ids (set (map :addon/id skipped))]
      (and (= seeds (set (map :addon/id (:hot/registered plan))))
           (= skipped-ids (set (map :addon/id (:hot/skipped plan))))
           (empty? (filter skipped-ids seeds))
           (every? (comp string? :reason) skipped)))))

(deftest reload-all-never-seeds-a-jar-addon-test
  (let [plan {:hot/registered [{:addon/id "hive.carto" :hot/source-kind :directory}]
              :hot/skipped    [{:addon/id "hive.rss" :hot/source-kind :jar}
                               {:addon/id "hive.gen" :hot/source-kind :absent}]}
        {:keys [seeds skipped]} (hot/reload-all-seeds plan)]
    (is (= #{"hive.carto"} seeds))
    (is (= ["hive.rss" "hive.gen"] (mapv :addon/id skipped)))
    (is (str/includes? (:reason (first skipped)) "restart-required"))))

(deftest skip-reason-is-open-by-registration-test
  (testing "a new source kind is one defmethod"
    (defmethod hot/skip-reason ::remote [_] "remote source: fetched, not watched")
    (try
      (is (= "remote source: fetched, not watched"
             (:reason (first (:skipped (hot/reload-all-seeds
                                        {:hot/skipped [{:addon/id "x" :hot/source-kind ::remote}]}))))))
      (finally (remove-method hot/skip-reason ::remote)))))

;; =============================================================================
;; status: no-reload split
;; =============================================================================

(defspec no-reload-split-conserves-every-pin 200
  (prop/for-all [core  (gen/set gen/symbol {:max-elements 8})
                 addon (gen/set gen/symbol {:max-elements 8})]
    (let [{:keys [effective counts] :as split} (hot/no-reload-split core addon)]
      (and (= (set effective) (set (map str (into core addon))))
           (= (:core counts) (count (:core split)) (count core))
           (= (:addon counts) (count (:addon split)) (count addon))
           (= effective (sort effective))))))

;; =============================================================================
;; pin: validation, idle reset, dormant note
;; =============================================================================

(def ^:private policies [:enum :eager :lazy :pinned])

(deftest pin-decl-test
  (testing "the policy is validated"
    (is (str/includes? (:error (hot/pin-decl {:policy "pinnned"} policies 1000)) "pinnned")))
  (testing "default policy is pinned"
    (is (= :pinned (:policy (hot/pin-decl {} policies 1000)))))
  (testing "idle_ms absent RESETS to the baseline"
    (is (= 1000 (:idle-ms (hot/pin-decl {:policy "lazy"} policies 1000)))))
  (testing "idle_ms given wins"
    (is (= 42 (:idle-ms (hot/pin-decl {:policy "lazy" :idle_ms 42} policies 1000)))))
  (testing "idle_ms must be positive"
    (is (:error (hot/pin-decl {:policy "lazy" :idle_ms -1} policies 1000)))))

(deftest dormant-pin-note-test
  (is (string? (hot/dormant-pin-note :pinned :dormant "a")))
  (is (string? (hot/dormant-pin-note :eager :failed "a")))
  (is (nil? (hot/dormant-pin-note :lazy :dormant "a")) "lazy is meant to be dormant")
  (is (nil? (hot/dormant-pin-note :pinned :active "a")) "an active addon needs no note"))

;; =============================================================================
;; Lifecycle verbs over a stub host
;; =============================================================================

(def ^:private stub-host
  (reify lport/ILifecycleHost
    (-mount! [_ specs _]
      {:ok? true :mounted (mapv (fn [s] {:addon/id (:addon/id s) :success? true}) specs)})
    (-unmount! [_ _] {:ok? true :errors []})
    (-install-stubs! [_ _ _ _] nil)
    (-remove-stubs! [_ _] nil)
    (-observe-surface [_ _] nil)))

(def ^:private stub-spec
  {:addon/id "stub.lazy" :addon/init-ns 'stub.lazy
   :addon/lifecycle {:policy :lazy :idle-ms 5000}
   :addon/surface {:tools [{:name "stub_tool"}]}})

(use-fixtures :each
  (fn [f]
    (let [mgr (lc/manager {:host stub-host :specs [stub-spec]})]
      (lc/boot! mgr {:mount-eager? false})
      (lc/install! mgr)
      (try (f) (finally (lc/uninstall!))))))

(deftest pin-on-a-dormant-addon-says-it-stays-dormant-test
  (let [out (body (hot/handle-pin {:addon "stub.lazy" :policy "pinned"}))]
    (is (= "pinned" (get-in out [:lifecycle :policy])))
    (is (= "dormant" (:phase out)))
    (is (str/includes? (:note out) "activate"))))

(deftest pin-resets-idle-ms-to-the-declared-value-test
  (hot/handle-pin {:addon "stub.lazy" :policy "lazy" :idle_ms 99})
  (is (= 99 (get-in (body (hot/handle-pin {:addon "stub.lazy" :policy "lazy" :idle_ms 99}))
                    [:lifecycle :idle-ms])))
  (is (= 5000 (get-in (body (hot/handle-pin {:addon "stub.lazy" :policy "lazy"}))
                      [:lifecycle :idle-ms]))
      "omitting idle_ms goes back to the manifest's 5000, not the earlier 99"))

(deftest pin-rejects-an-unknown-policy-test
  (let [resp (hot/handle-pin {:addon "stub.lazy" :policy "sticky"})]
    (is (:isError resp))
    (is (str/includes? (:text resp) "sticky"))))

(deftest lifecycle-verbs-share-one-seam-test
  (testing "activate and evict are LifecycleVerb descriptors over the same manager"
    (is (true? (:ok? (body (hot/handle-activate {:addon "stub.lazy"})))))
    (is (true? (:evicted? (body (hot/handle-evict {:addon "stub.lazy"})))))))

(deftest a-new-lifecycle-verb-is-one-descriptor-test
  (testing "a bridge that exists is called with (mgr & args)"
    (let [verb (hot/lifecycle-verb {:bridge 'hive-addon.lifecycle/phase :requires [:addon]})]
      (is (= "dormant" (body (verb {:addon "stub.lazy"}))))))
  (testing "a bridge this hive-addon lacks answers an actionable error"
    (let [verb (hot/lifecycle-verb {:bridge 'hive-addon.lifecycle/eject! :requires [:addon]})
          resp (verb {:addon "stub.lazy"})]
      (is (:isError resp))
      (is (str/includes? (:text resp) "eject!"))))
  (testing "a missing required param is refused before the bridge"
    (is (:isError ((hot/lifecycle-verb {:bridge 'hive-addon.lifecycle/phase :requires [:addon]}) {}))))
  (testing "a malformed descriptor is refused at construction"
    (is (thrown? AssertionError (hot/lifecycle-verb {:bridge "not-a-symbol"})))))

(deftest advertised-commands-follow-the-verb-table-test
  (is (= (set (conj (map name (keys hot/canonical-handlers)) "help"))
         (set (get-in hot/tool-def [:inputSchema :properties "command" :enum])))))

(deftest lifecycle-gate-hint-names-the-real-config-path-test
  (lc/uninstall!)
  (is (str/includes? (:text (hot/handle-lifecycle {})) "{:services {:addons {:lifecycle")))
