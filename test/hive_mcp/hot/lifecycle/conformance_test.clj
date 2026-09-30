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
  ([kind addon] {:op/kind kind :op/addon addon}))

(def scenarios
  {:core-reload-with-addons-mounted [(op :core-reload) (op :core-reload)]
   :addon-reload-each               [(op :addon-reload "hive.rss") (op :addon-reload "hive.compose")]
   :evict-then-call-remounts        [(op :evict "hive.rss") (op :call "hive.rss")]
   :evict-then-activate-roundtrip   [(op :evict "hive.rss") (op :activate "hive.rss") (op :activate "hive.rss")]
   :hook-only-refuses-eviction      [(op :evict "hive.guard.projections")]
   :reload-needs-a-mounted-addon    [(op :evict "hive.compose") (op :addon-reload "hive.compose")]
   :core-reload-while-dormant       [(op :evict "hive.rss") (op :core-reload) (op :activate "hive.rss")]})

(defn- restore-all-active!
  "Bring every dormant addon back so the next scenario starts at baseline."
  [host]
  (doseq [[id phase] (:obs/phases (port/observe host)) :when (= :dormant phase)]
    (port/apply-op! host (op :activate id))))

(defn run-scenarios
  "Run every scenario on the live host. Returns {scenario {:live :model :violations}}."
  []
  (let [spec (live/live-spec)
        host (live/handlers-host probe-calls)]
    (into (sorted-map)
          (for [[k script] scenarios]
            (let [live-trace (runner/canonical (runner/run-script host script))]
              (restore-all-active! host)
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
