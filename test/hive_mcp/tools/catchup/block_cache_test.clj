(ns hive-mcp.tools.catchup.block-cache-test
  "Cold tests for the catchup block cache: a herd of catchups on one project
   pays one block computation, opt-in only, write invalidation by tag,
   status providers, and shared context-store refs. No store."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.tools.catchup.block-cache :as bc]
            [hive-mcp.spi.catchup-registry :as blocks]
            [hive-mcp.events.write-events :as write-events]
            [hive-mcp.channel.context-store :as context-store])
  (:import [java.util.concurrent CountDownLatch TimeUnit]))

(use-fixtures :each (fn [t] (bc/reset-cache!) (t) (bc/reset-cache!)))

(defn- wait-until
  [pred timeout-ms]
  (let [deadline (+ (System/currentTimeMillis) timeout-ms)]
    (loop []
      (cond (pred) true
            (> (System/currentTimeMillis) deadline) false
            :else (do (Thread/sleep 20) (recur))))))

(defn- counting-block
  [computes cache]
  (cond-> {:block/id    :probe
           :block/fn    (fn [{:keys [project-id]}] {:project project-id :n (swap! computes inc)})
           :block/order 1}
    (some? cache) (assoc :block/cache cache)))

;; =============================================================================
;; Herd: N catchups on one project, one computation
;; =============================================================================

(deftest concurrent-catchups-share-one-block-computation-test
  (let [computes (atom 0)
        release  (CountDownLatch. 1)
        started  (CountDownLatch. 1)
        block    {:block/id    :probe
                  :block/fn    (fn [_]
                                 (swap! computes inc)
                                 (.countDown started)
                                 (.await release 5 TimeUnit/SECONDS)
                                 {:ok true})
                  :block/order 1
                  :block/cache true}
        ctx      (fn [i] {:project-id "p" :directory "/d" :caller-id (str "agent-" i)})
        callers  (mapv (fn [i] (future (bc/run-block block (ctx i)))) (range 12))]
    (is (.await started 5 TimeUnit/SECONDS))
    (Thread/sleep 100)
    (.countDown release)
    (is (every? #(= {:ok true} (deref % 5000 ::timeout)) callers))
    (is (= 1 @computes) "twelve agents, one computation")
    (testing "a later catchup on the same project is a hit"
      (bc/run-block block (ctx 99))
      (is (= 1 @computes)))))

(deftest uncached-block-runs-every-time-test
  (let [computes (atom 0)
        block    (counting-block computes nil)]
    (dotimes [_ 3] (bc/run-block block {:project-id "p"}))
    (is (= 3 @computes) "a block that does not opt in is never cached")))

(deftest key-by-separates-projects-and-ignores-caller-test
  (let [computes (atom 0)
        block    (counting-block computes {:key-by [:project-id]})]
    (is (= {:project "a" :n 1} (bc/run-block block {:project-id "a" :caller-id "x" :directory "/1"})))
    (is (= {:project "a" :n 1} (bc/run-block block {:project-id "a" :caller-id "y" :directory "/2"})))
    (is (= {:project "b" :n 2} (bc/run-block block {:project-id "b" :caller-id "x"})))
    (is (= 2 @computes))))

(deftest compute-failure-is-not-cached-test
  (let [boom  {:block/id :probe :block/order 1 :block/cache true
               :block/fn (fn [_] (throw (ex-info "boom" {})))}
        fine  (assoc boom :block/fn (fn [_] :fine))]
    (is (thrown? Exception (bc/run-block boom {:project-id "p"})))
    (is (= :fine (bc/run-block fine {:project-id "p"})))))

;; =============================================================================
;; Freshness
;; =============================================================================

(deftest stale-while-revalidate-test
  (let [clock    (atom 1000000)
        computes (atom 0)
        block    (counting-block computes true)
        ctx      {:project-id "p"}
        {:keys [fresh-ms max-age-ms]} bc/default-block-policy]
    (with-redefs [bc/now-ms (fn [] @clock)]
      (is (= 1 (:n (bc/run-block block ctx))))
      (swap! clock + (dec fresh-ms))
      (is (= 1 (:n (bc/run-block block ctx))) "fresh")
      (swap! clock + 1)
      (is (= 1 (:n (bc/run-block block ctx))) "stale is served")
      (is (wait-until #(and (= 2 @computes) (zero? (:refreshing (bc/stats)))) 5000)
          "and refreshed in the background")
      (is (= 2 (:n (bc/run-block block ctx))) "the refreshed value is served")
      (swap! clock + max-age-ms)
      (is (= 3 (:n (bc/run-block block ctx))) "past max-age: computed synchronously"))))

;; =============================================================================
;; Invalidation
;; =============================================================================

(deftest tagged-write-drops-entry-before-notify-returns-test
  (let [computes (atom 0)
        block    (counting-block computes {:drop-tags #{"kanban"}})
        ctx      {:project-id "p"}]
    (bc/run-block block ctx)
    (write-events/notify! :added {:id "x" :memory-type "decision" :tags ["decision"]})
    (bc/run-block block ctx)
    (is (= 1 @computes) "an unrelated write leaves the entry")
    (write-events/notify! :updated {:id "k" :memory-type "note" :tags ["kanban" "done"]})
    (bc/run-block block ctx)
    (is (= 2 @computes) "a kanban write drops it synchronously")))

;; =============================================================================
;; Registry runner
;; =============================================================================

(deftest compose-uses-the-runner-test
  (let [computes (atom 0)]
    (try
      (blocks/register-block! (counting-block computes true))
      (dotimes [_ 5] (blocks/compose {:project-id "p"} bc/run-block))
      (is (= 1 @computes))
      (blocks/compose {:project-id "p"})
      (is (= 2 @computes) "the 1-arity compose stays uncached")
      (finally (blocks/unregister-block! :probe)))))

;; =============================================================================
;; Status providers
;; =============================================================================

(deftest status-provider-cached-per-project-test
  (let [calls    (atom 0)
        provider (fn [pid] (swap! calls inc) {:pid pid})]
    (dotimes [_ 4] (bc/run-status-provider :carto-status provider "p"))
    (bc/run-status-provider :carto-status provider "q")
    (is (= 2 @calls))))

;; =============================================================================
;; Shared context refs
;; =============================================================================

(deftest identical-bundle-shares-context-refs-test
  (let [puts   (atom 0)
        put-fn (fn []
                 (swap! puts inc)
                 {:axioms (context-store/context-put! {:n @puts} :tags #{"catchup-test"} :ttl-ms 60000)})
        b1     {:axioms [{:id "a"}]}]
    (try
      (let [r1 (bc/shared-context-refs "p" b1 put-fn)
            r2 (bc/shared-context-refs "p" b1 put-fn)]
        (is (= r1 r2))
        (is (= 1 @puts) "same bundle object, one write")
        (testing "a different bundle value writes fresh refs"
          (bc/shared-context-refs "p" {:axioms [{:id "a"}]} put-fn)
          (is (= 2 @puts)))
        (testing "an evicted ref forces a rewrite"
          (let [b2 {:axioms []}
                r  (bc/shared-context-refs "p" b2 put-fn)]
            (context-store/context-evict! (:axioms r))
            (bc/shared-context-refs "p" b2 put-fn)
            (is (= 4 @puts)))))
      (finally (context-store/evict-by-tags! #{"catchup-test"})))))
