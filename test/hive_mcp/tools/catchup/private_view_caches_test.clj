(ns hive-mcp.tools.catchup.private-view-caches-test
  "Private-view catchup: the block cache, status providers and piggyback buffer never serve a private caller's content to another caller."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.memory-piggyback :as mp]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.tools.catchup.block-cache :as bc]
            [hive-mcp.tools.catchup.bundle-cache :as bundle-cache]
            [hive-spi.catchup.registry :as blocks]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private extension-keys
  [:catchup/private-view? :cu/a :catchup/lens :catchup/persona-lens
   :catchup/status-providers])

(def ^:private probe-block-id :zz-private-view-caches-probe)

(defn- restore-state [t]
  (let [prior (mapv (fn [k] [k (ext/get-extension k ::absent)]) extension-keys)]
    (try
      (run! ext/deregister! extension-keys)
      (blocks/unregister-block! probe-block-id)
      (bc/reset-cache!)
      (bundle-cache/reset-cache!)
      (mp/reset-all!)
      (t)
      (finally
        (blocks/unregister-block! probe-block-id)
        (doseq [[k value] prior]
          (if (= ::absent value)
            (ext/deregister! k)
            (ext/register! k value)))
        (bc/reset-cache!)
        (bundle-cache/reset-cache!)
        (mp/reset-all!)))))

(use-fixtures :each mem-stub/with-stub-store restore-state)

;; =============================================================================
;; block-cache private bypass
;; =============================================================================

(def ^:private policy
  "An existing block-cache policy: fresh for a minute, stale to two."
  {:fresh-ms 60000 :max-age-ms 120000})

(deftest a-private-block-cache-call-computes-and-stores-nothing
  (let [computes (atom 0)
        f        (fn [] (swap! computes inc))]
    (testing "POSITIVE CONTROL: without the binding the second call is a hit"
      (is (= 1 (bc/cached [:block :t "control"] policy f)))
      (is (= 1 (bc/cached [:block :t "control"] policy f)))
      (is (= 1 @computes)))
    (testing "under *private-view* every call computes"
      (binding [bundle-cache/*private-view* true]
        (is (= 2 (bc/cached [:block :t "p"] policy f)))
        (is (= 3 (bc/cached [:block :t "p"] policy f))))
      (is (= 3 @computes) "the private caller computed both times"))
    (testing "and nothing was stored for that key"
      (is (= 1 (:entries (bc/stats)))
          "only the control key is in the cache"))))

;; =============================================================================
;; piggyback adoption refuses a private buffer
;; =============================================================================

(def ^:private entry
  {:id "ax-pv-1" :type "axiom" :content "Private to aaa" :tags ["axiom"]})

(deftest a-private-buffer-is-not-adopted
  (testing "POSITIVE CONTROL: a non-private buffer is adopted"
    (mp/enqueue! "coordinator:ctl" [entry] nil)
    (is (true? (mp/adopt-buffer! "coordinator:ctl2")))
    (is (mp/has-pending? "coordinator:ctl2")))
  (testing "a private buffer is refused"
    (mp/reset-all!)
    (mp/enqueue! "coordinator:aaa" [entry] nil {:private? true})
    (is (not (mp/adopt-buffer! "coordinator:bbb")))
    (testing "and it stays under the aaa key"
      (is (mp/has-pending? "coordinator:aaa"))
      (is (not (mp/has-pending? "coordinator:bbb"))))))

(deftest private-is-sticky-on-the-buffer
  (mp/enqueue! "coordinator:aaa" [entry] nil {:private? true})
  (mp/enqueue! "coordinator:aaa"
               [(assoc entry :id "ax-pv-2")] nil)
  (is (not (mp/adopt-buffer! "coordinator:bbb"))
      "a later non-private enqueue does not clear the flag")
  (is (mp/has-pending? "coordinator:aaa")))

;; =============================================================================
;; a) drain-then-adopt: a partly drained private buffer stays private
;; =============================================================================

(def ^:private big-entries
  "Twenty 1,000-char axioms: well over the 12,000-char drain budget, so one
   drain leaves the buffer partly delivered and still pending."
  (vec (for [i (range 20)]
         {:id      (str "ax-big-" i)
          :type    "axiom"
          :content (str "Axiom " i ": " (apply str (repeat 1000 "x")))
          :tags    ["axiom"]})))

(deftest a-drained-private-buffer-is-not-adopted
  (is (> (reduce + (map (comp count pr-str) big-entries)) mp/drain-char-budget)
      "the fixture is larger than one drain")
  (testing "POSITIVE CONTROL: a partly drained non-private buffer is adopted"
    (mp/enqueue! "coordinator:aaa" big-entries nil)
    (is (some? (mp/drain! "coordinator:aaa")))
    (is (mp/has-pending? "coordinator:aaa") "one drain did not exhaust it")
    (is (true? (mp/adopt-buffer! "coordinator:bbb"))))
  (testing "a partly drained private buffer is refused"
    (mp/reset-all!)
    (mp/enqueue! "coordinator:aaa" big-entries nil {:private? true})
    (is (some? (mp/drain! "coordinator:aaa")))
    (is (mp/has-pending? "coordinator:aaa") "one drain did not exhaust it")
    (is (false? (boolean (mp/adopt-buffer! "coordinator:bbb"))))
    (is (not (mp/has-pending? "coordinator:bbb")))))

;; =============================================================================
;; end to end through handle-native-catchup
;; =============================================================================

(defn- tmp-project! [pid]
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "catchup-pvc-" pid))
              (.mkdirs))]
    (spit (io/file dir ".hive-project.edn") (pr-str {:project-id pid}))
    (.getAbsolutePath dir)))

(defn- rm-tree! [dir]
  (run! io/delete-file (reverse (file-seq (io/file dir)))))

(defn- seed-axioms!
  "Seed the current stub store with twenty 1,000-char axioms scoped to PID:
   more than one 12,000-char drain."
  [pid]
  (let [store (mem-stub/->stub)
        scope (str "scope:project:" pid)]
    (mem-stub/install! store)
    (mem-stub/seed!
     store
     (for [i (range 20)]
       {:content (str "Private axiom " i ": " (apply str (repeat 1000 "a")))
        :type    "axiom"
        :tags    ["catchup-pvc" scope]}))))

(defn- catchup!
  "Run handle-native-catchup on DIR, private when PRIVATE? (through the
   :catchup/private-view? extension). CALLER nil sends no :_caller_id."
  [dir private? caller]
  (ext/register! :catchup/private-view? (fn [_ _] private?))
  (try
    (catchup/handle-native-catchup (cond-> {:directory dir :inline_budget 0}
                                     caller (assoc :_caller_id caller)))
    (finally
      (ext/deregister! :catchup/private-view?))))

(defn- with-project
  "Call (f dir) on a fresh temp project, seeded with axioms when SEED?."
  [seed? f]
  (let [pid (str "catchup-pvc-" (random-uuid))
        dir (tmp-project! pid)]
    (try
      (when seed? (seed-axioms! pid))
      (f dir)
      (finally (rm-tree! dir)))))

;; -----------------------------------------------------------------------------
;; b) the catchup buffer, drained once, is still not adoptable
;; -----------------------------------------------------------------------------

(deftest a-private-catchup-leaves-no-adoptable-buffer
  (with-project true
    (fn [dir]
      (catchup! dir true "coordinator:aaa")
      (is (some? (mp/drain! "coordinator:aaa")))
      (is (mp/has-pending? "coordinator:aaa") "one drain did not exhaust it")
      (testing "the buffer the private caller wrote cannot be adopted by another"
        (is (false? (boolean (mp/adopt-buffer! "coordinator:bbb"))))))))

(deftest a-shared-catchup-buffer-is-adoptable
  (with-project true
    (fn [dir]
      (catchup! dir false "coordinator:aaa")
      (is (some? (mp/drain! "coordinator:aaa")))
      (is (mp/has-pending? "coordinator:aaa") "one drain did not exhaust it")
      (testing "POSITIVE CONTROL: the same run under a shared view is adopted"
        (is (true? (mp/adopt-buffer! "coordinator:bbb")))))))

;; -----------------------------------------------------------------------------
;; c) seam: a private run does not seed the block cache for a shared run
;; -----------------------------------------------------------------------------

(defn- register-probes!
  "A cached catchup block and a :carto-status status provider, each answering
   the next value of its own counter. Returns {:block atom :status atom}."
  []
  (let [block-n  (atom 0)
        status-n (atom 0)]
    (blocks/register-block! {:block/id    probe-block-id
                             :block/order 99
                             :block/cache true
                             :block/fn    (fn [_] {:n (swap! block-n inc)})})
    (ext/register! :catchup/status-providers
                   {:carto-status (fn [_]
                                    {:indexed-forms (swap! status-n inc)
                                     :readiness     "ready"})})
    {:block block-n :status status-n}))

(defn- probe-values
  "The probe block's :n and the carto-status :indexed-forms in a catchup answer."
  [response]
  (let [by-name (into {}
                      (map (fn [r] (let [m (json/read-str (:text r) :key-fn keyword)]
                                     [(:_block m) m])))
                      response)]
    {:block  (get-in by-name [(name probe-block-id) :n])
     :status (get-in by-name ["carto-status" :indexed-forms])}))

(deftest a-shared-run-recomputes-after-a-private-run
  (with-project false
    (fn [dir]
      (let [counters (register-probes!)
            private  (probe-values (catchup! dir true "coordinator:aaa"))
            shared   (probe-values (catchup! dir false "coordinator:bbb"))]
        (is (= {:block 1 :status 1} private) "the private run computed both")
        (is (= {:block 2 :status 2} shared)
            "the shared run recomputed and did not get the private values")
        (is (= 2 @(:block counters)))
        (is (= 2 @(:status counters)))))))

(deftest two-shared-runs-hit-the-cache
  (with-project false
    (fn [dir]
      (let [counters (register-probes!)
            first-r  (probe-values (catchup! dir false "coordinator:aaa"))
            second-r (probe-values (catchup! dir false "coordinator:bbb"))]
        (testing "POSITIVE CONTROL: inside the fresh window the second run is a hit"
          (is (= {:block 1 :status 1} first-r))
          (is (= {:block 1 :status 1} second-r))
          (is (= 1 @(:block counters)))
          (is (= 1 @(:status counters))))))))

;; -----------------------------------------------------------------------------
;; d) a private catchup with no caller id writes no shared "coordinator" buffer
;; -----------------------------------------------------------------------------

(deftest a-private-catchup-without-caller-id-leaves-no-buffer
  (with-project true
    (fn [dir]
      (catchup! dir true nil)
      (is (not (mp/has-pending? "coordinator"))
          "nothing was enqueued under the default coordinator key"))))

(deftest a-shared-catchup-without-caller-id-leaves-a-buffer
  (with-project true
    (fn [dir]
      (catchup! dir false nil)
      (testing "POSITIVE CONTROL: a shared run enqueues under coordinator"
        (is (mp/has-pending? "coordinator"))))))
