(ns hive-mcp.hot.lifecycle.conformance-test
  "A LIVE hive-mcp conforms to the reloadable-core lifecycle model.

   The subject here IS the running integration (hive-hot, the addon lifecycle,
   the served tool table), so this suite only runs inside a disposable
   instance booted by bin/instance2.sh (HIVE_PROFILE=instance2). Anywhere else
   every test reports itself skipped by assertion, never silently green. It
   mutates the host: never point it at the live server.

   For each scenario the same Script runs against the model of the live
   host's own HostSpec and against the live host; the canonical traces must be
   equal and the live trace must break no registered invariant."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.hot.lifecycle.handlers-host :as live]
            [hive-mcp.hot.lifecycle.model :as model]
            [hive-mcp.hot.lifecycle.port :as port]
            [hive-mcp.hot.lifecycle.runner :as runner]))

(def ^:private instance? (= "instance2" (System/getenv "HIVE_PROFILE")))

(def probe-calls
  "How a :call op exercises each addon through its advertised tool."
  {"hive.compose" {:tool "compose" :args {:command "status"}}
   "hive.rss"     {:tool "rss" :args {:command "status"}}})

(defn- op
  ([kind] {:op/kind kind})
  ([kind addon] {:op/kind kind :op/addon addon})
  ([kind addon more] (merge (op kind addon) more)))

(def scenarios
  {:core-reload-with-addons-mounted [(op :core-reload) (op :core-reload)]
   :addon-reload-each               [(op :addon-reload "hive.rss") (op :addon-reload "hive.compose")]
   :evict-then-call-remounts        [(op :evict "hive.rss") (op :call "hive.rss")]
   :evict-then-activate-roundtrip   [(op :evict "hive.rss") (op :activate "hive.rss") (op :activate "hive.rss")]
   :hook-only-refuses-eviction      [(op :evict "hive.guard.projections")]
   :reload-needs-a-mounted-addon    [(op :evict "hive.compose") (op :addon-reload "hive.compose")]
   :core-reload-while-dormant       [(op :evict "hive.rss") (op :core-reload) (op :activate "hive.rss")]
   :eject-then-call-refused         [(op :eject "hive.rss") (op :call "hive.rss")]
   :eject-then-inject-restores      [(op :eject "hive.rss") (op :inject "hive.rss") (op :inject "hive.rss")]
   :unmount-then-call-refused       [(op :unmount "hive.rss") (op :call "hive.rss") (op :unmount "hive.rss")]
   :unmount-then-inject-restores    [(op :unmount "hive.rss") (op :inject "hive.rss")]
   :pin-lazy-on-hook-only           [(op :pin "hive.guard.projections" {:op/policy :lazy})
                                     (op :pin "hive.guard.projections" {:op/policy :lazy :op/force? true})]})

(defn- restore-baseline!
  "Bring the host back to SPEC's boot shape so the next scenario starts at
   baseline: inject every ejected addon, activate every dormant one, and set
   every addon back to its declared policy (forced, since a declared :lazy on
   a hook-only addon is what boot already accepted)."
  [host spec]
  (doseq [id (keys (:spec/addon-tools spec))
          :when (not (contains? (:obs/phases (port/observe host)) id))]
    (port/apply-op! host (op :inject id)))
  (doseq [[id phase] (:obs/phases (port/observe host)) :when (= :dormant phase)]
    (port/apply-op! host (op :activate id)))
  (doseq [[id policy] (:spec/policy spec)]
    (port/apply-op! host (op :pin id {:op/policy policy :op/force? true}))))

(defn run-scenarios
  "Run every scenario on the live host. Returns {scenario {:live :model :violations}}."
  []
  (let [spec (live/live-spec)
        host (live/handlers-host probe-calls (:spec/inject-paths spec))]
    (into (sorted-map)
          (for [[k script] scenarios]
            (let [live-trace (runner/canonical (runner/run-script host script))]
              (restore-baseline! host spec)
              [k {:live live-trace
                  :model (runner/canonical (model/run spec script))
                  :violations (model/violations spec live-trace)
                  :spec spec}])))))

(deftest live-host-conforms-to-the-model
  (if-not instance?
    (is true "skipped: not inside a bin/instance2.sh instance (HIVE_PROFILE=instance2)")
    (doseq [[k {:keys [live model violations]}] (run-scenarios)]
      (testing (name k)
        (is (empty? violations) (pr-str violations))
        (is (= model live))))))

(deftest live-replies-read-as-outcomes
  (testing "pin: a lifecycle back is applied; :refused on it, or no lifecycle at all, is refused"
    (is (= :applied (live/outcome-of (op :pin "x" {:op/policy :lazy}) {:lifecycle {:policy "lazy"}})))
    (is (= :refused (live/outcome-of (op :pin "x" {:op/policy :lazy})
                                     {:lifecycle {:policy "eager" :refused "no-surface"}})))
    (is (= :refused (live/outcome-of (op :pin "x" {:op/policy :lazy}) {:ok? false :unparsed "boom"}))))
  (testing "eject: the report's :ok?"
    (is (= :applied (live/outcome-of (op :eject "x") {:ok? true :hot/ejected ["x"]})))
    (is (= :refused (live/outcome-of (op :eject "x") {:ok? false :hot/refused? true}))))
  (testing "unmount: the projected plug-out! Result's :ok?"
    (is (= :applied (live/outcome-of (op :unmount "x") {:ok? true :hot/ejected ["x"]})))
    (is (= :refused (live/outcome-of (op :unmount "x") {:ok? false :reason "hot/eject-refused"})))
    (is (= :refused (live/outcome-of (op :unmount "x") {:ok? false :reason "hot/eject-unknown"}))))
  (testing "the model reads unmount as eject: a second unmount of the same addon is refused"
    (let [spec {:spec/core-tools #{"hot"} :spec/addon-tools {"x" #{"x_tool"}}
                :spec/hook-only #{} :spec/policy {"x" :eager} :spec/deps {}}
          t    (model/run spec [(op :unmount "x") (op :unmount "x")])]
      (is (= [:applied :refused] (mapv :step/outcome (:trace/steps t))))))
  (testing "inject: presence before and after decides"
    (let [o (op :inject "x")]
      (is (= :noop (live/outcome-of o {:ok? true ::live/present-before? true ::live/present-after? true})))
      (is (= :applied (live/outcome-of o {:ok? true ::live/present-before? false ::live/present-after? true})))
      (is (= :refused (live/outcome-of o {:ok? false ::live/present-before? false ::live/present-after? false}))))))
