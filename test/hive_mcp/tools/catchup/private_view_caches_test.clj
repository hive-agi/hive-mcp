(ns hive-mcp.tools.catchup.private-view-caches-test
  "The catchup caches under a private view, and the piggyback buffer that must
   not be adopted across callers.

   Two leaks this closes (card 20261125140846-11b9a3f1):

     1. block-cache served contributed blocks and status providers computed
        under a private view to every caller of the project, and stored them.
        bundle-cache already bypassed both tiers; block-cache did not.

     2. the memory piggyback buffer written by a private caller was adoptable
        by another coordinator instance (adopt-buffer! matches any
        coordinator: prefix), leaking the private caller's axioms.

   The private view is conveyed with bundle-cache/*private-view* (the seam
   private-view/in-view binds). Extensions are registered through the real
   registry and restored after each test; no with-redefs. Store: mem-stub."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.memory-piggyback :as mp]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.tools.catchup.block-cache :as bc]
            [hive-mcp.tools.catchup.bundle-cache :as bundle-cache]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private extension-keys
  [:catchup/private-view? :cu/a :catchup/lens :catchup/persona-lens
   :catchup/status-providers])

(defn- restore-state [t]
  (let [prior (mapv (fn [k] [k (ext/get-extension k ::absent)]) extension-keys)]
    (try
      (run! ext/deregister! extension-keys)
      (bc/reset-cache!)
      (mp/reset-all!)
      (t)
      (finally
        (doseq [[k value] prior]
          (if (= ::absent value)
            (ext/deregister! k)
            (ext/register! k value)))
        (bc/reset-cache!)
        (mp/reset-all!)))))

(use-fixtures :each mem-stub/with-stub-store restore-state)

;; =============================================================================
;; a) block-cache private bypass
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
;; b) piggyback adoption refuses a private buffer
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

;; =============================================================================
;; c) stickiness: private is a one-way flag
;; =============================================================================

(deftest private-is-sticky-on-the-buffer
  (mp/enqueue! "coordinator:aaa" [entry] nil {:private? true})
  (mp/enqueue! "coordinator:aaa"
               [(assoc entry :id "ax-pv-2")] nil)
  (is (not (mp/adopt-buffer! "coordinator:bbb"))
      "a later non-private enqueue does not clear the flag")
  (is (mp/has-pending? "coordinator:aaa")))

;; =============================================================================
;; d) end to end: catchup under a private view leaves no adoptable buffer
;; =============================================================================

(defn- tmp-project! [pid]
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "catchup-pvc-" pid))
              (.mkdirs))]
    (spit (io/file dir ".hive-project.edn") (pr-str {:project-id pid}))
    (.getAbsolutePath dir)))

(defn- rm-tree! [dir]
  (run! io/delete-file (reverse (file-seq (io/file dir)))))

(defn- seed-project! [pid]
  (let [store (mem-stub/->stub)
        scope (str "scope:project:" pid)]
    (mem-stub/install! store)
    (mem-stub/seed!
     store
     (for [i (range 6)]
       {:content (str "Private axiom " i ": " (apply str (repeat 200 "a")))
        :type    "axiom"
        :tags    ["catchup-pvc" scope]}))))

(defn- run-catchup [pid private? caller]
  (let [dir (tmp-project! pid)]
    (seed-project! pid)
    (ext/register! :catchup/private-view? (fn [_ _] private?))
    (try
      (catchup/handle-native-catchup {:directory     dir
                                      :_caller_id    caller
                                      :inline_budget 1500})
      (finally
        (ext/deregister! :catchup/private-view?)
        (rm-tree! dir)))))

(deftest a-private-catchup-leaves-no-adoptable-buffer
  (let [pid (str "catchup-pvc-" (random-uuid))]
    (mp/reset-all!)
    (run-catchup pid true "coordinator:aaa")
    (testing "the buffer the private caller wrote cannot be adopted by another"
      (is (not (mp/adopt-buffer! "coordinator:bbb"))))))

(deftest a-shared-catchup-buffer-is-adoptable
  (let [pid (str "catchup-pvc-" (random-uuid))]
    (mp/reset-all!)
    (run-catchup pid false "coordinator:aaa")
    (testing "POSITIVE CONTROL: the same run under a shared view is adopted"
      (is (true? (mp/adopt-buffer! "coordinator:bbb"))))))
