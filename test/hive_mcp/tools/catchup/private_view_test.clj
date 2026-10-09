(ns hive-mcp.tools.catchup.private-view-test
  "Catchup's private-view seams: pre-query hooks run before the bundle, and a
   private view bypasses BOTH shared cache tiers, read and write. The leak this
   closes (card ENCLAVE-CATCHUP-LEAK): the bundle cache is keyed by project-id
   only, so a bundle computed under an enclave member's view was served to
   every caller of the project.

   Extensions are registered through the real registry (the seam under test)
   and removed after each test. No store."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.catchup.bundle-cache :as bc]
            [hive-mcp.tools.catchup.private-view :as pv]
            [hive-schemas.test :as hst]))

(def ^:private keys-used [:catchup/pre-query :catchup/private-view?])

(use-fixtures :each
  (fn [t]
    (bc/reset-cache!)
    (let [saved (into {} (keep (fn [k] (when-let [v (ext/get-extension k)] [k v]))) keys-used)]
      (try
        (run! ext/deregister! keys-used)
        (t)
        (finally
          (run! ext/deregister! keys-used)
          (doseq [[k v] saved] (ext/register! k v))
          (bc/reset-cache!))))))

;; =============================================================================
;; Pure: hooks-of
;; =============================================================================

(def ^:private NonFnValue [:or :nil :string :int [:vector {:max 3} :int]])

(defn- value-class [[v] _]
  (cond (nil? v) :nil (string? v) :string (int? v) :int :else :coll))

(hst/deftrifecta-from-schema hooks-of-law
  hive-mcp.tools.catchup.private-view/hooks-of
  {:in              [:cat NonFnValue]
   :out             [:vector fn?]
   :rel             (fn [_ out] (empty? out))
   :classify        value-class
   :classify-domain #{:nil :string :int :coll}
   :golden-path     "test/golden/catchup/private-view-hooks-of.edn"
   :num-tests       100})

(deftest hooks-of-keeps-only-fns-in-order
  (is (= [inc] (pv/hooks-of inc)))
  (is (= [inc dec] (pv/hooks-of [inc "junk" dec 7]))))

;; =============================================================================
;; The cache tiers under a private view
;; =============================================================================

(defn- bundle [n] {:axioms [{:id (str "a" n)}] :decisions []})

(deftest a-private-bundle-is-never-stored-nor-served
  (let [computes (atom 0)
        compute  (fn [] (bundle (swap! computes inc)))]
    (testing "a shared caller caches"
      (is (= (bundle 1) (bc/cached-bundle "p" compute)))
      (is (= (bundle 1) (bc/cached-bundle "p" compute)))
      (is (= 1 @computes)))
    (testing "a private caller reads around the cache, every time"
      (is (= (bundle 2) (pv/in-view true #(bc/cached-bundle "p" compute))))
      (is (= (bundle 3) (pv/in-view true #(bc/cached-bundle "p" compute))))
      (is (= 3 @computes)))
    (testing "what the private caller computed was not stored for the next one"
      (is (= (bundle 1) (bc/cached-bundle "p" compute)))
      (is (= 3 @computes)))))

(deftest a-private-first-caller-leaves-the-cache-empty
  (let [compute (constantly {:axioms [{:id "enclave-secret"}] :decisions []})]
    (pv/in-view true #(bc/cached-bundle "p" compute))
    (is (= {:axioms [{:id "public"}] :decisions []}
           (bc/cached-bundle "p" (constantly {:axioms [{:id "public"}] :decisions []})))
        "a non-member computes its own view, it is not served the member's")))

(deftest the-content-tier-is-bypassed-under-a-private-view
  (let [fetches (atom [])
        fetch   (fn [ids] (swap! fetches conj ids) (map (fn [id] {:id id :content (str "c-" id)}) ids))]
    (pv/in-view true #(bc/cached-entries ["x" "y"] fetch))
    (pv/in-view true #(bc/cached-entries ["x" "y"] fetch))
    (is (= [["x" "y"] ["x" "y"]] @fetches) "every private read fetches")
    (bc/cached-entries ["x"] fetch)
    (is (= 3 (count @fetches)) "nothing a private read fetched was stored for a shared one")))

(deftest the-private-binding-reaches-a-future
  (let [seen (pv/in-view true #(deref (future bc/*private-view*)))]
    (is (true? seen))))

;; =============================================================================
;; Seams
;; =============================================================================

(deftest private-view-follows-the-provider
  (is (false? (pv/private-view? "coordinator:1" "p")) "no provider: shared")
  (ext/register! :catchup/private-view? (fn [caller _] (= "member" caller)))
  (is (true? (pv/private-view? "member" "p")))
  (is (false? (pv/private-view? "other" "p")))
  (testing "a throwing provider counts as private: slower, never a leak"
    (ext/register! :catchup/private-view? (fn [_ _] (throw (ex-info "boom" {}))))
    (is (true? (pv/private-view? "anyone" "p")))))

(deftest pre-query-hooks-run-in-order-and-survive-failure
  (let [calls (atom [])]
    (ext/register! :catchup/pre-query
                   [(fn [ctx] (swap! calls conj [:a ctx]) :bound)
                    (fn [_] (throw (ex-info "no display" {})))
                    (fn [ctx] (swap! calls conj [:c (:caller-id ctx)]) :ok)])
    (let [res (pv/run-pre-query! {:caller-id "c1" :directory "/d" :project-id "p"})]
      (is (= [{:ok :bound} {:error "no display"} {:ok :ok}] res))
      (is (= [[:a {:caller-id "c1" :directory "/d" :project-id "p"}] [:c "c1"]] @calls)))))

(deftest a-hook-that-hangs-is-cut-at-the-budget
  (ext/register! :catchup/pre-query (fn [_] (Thread/sleep 5000) :late))
  (let [t0  (System/currentTimeMillis)
        res (pv/run-pre-query! {:caller-id "c"} 100)]
    (is (= [{:timeout true}] res))
    (is (< (- (System/currentTimeMillis) t0) 2000))))

(deftest no-hooks-no-results
  (is (= [] (pv/run-pre-query! {:caller-id "c"}))))
