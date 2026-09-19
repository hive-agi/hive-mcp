(ns lifecycle-live
  "Live check of the addon lifecycle against a REAL addon (hive.crawl, found on
   the classpath through its META-INF manifest) on hive-mcp's real addon
   registry, extension registry and tool dispatch. No MCP server, no shared
   store: the learned surface goes to a temp dir.

   (run-check!) returns one map per step. Only `crawl status` is called."
  (:require [hive-addon.lifecycle :as lc]
            [hive-addon.mount.boundary :as boundary]
            [hive-mcp.addons.core :as addon-core]
            [hive-mcp.extensions.lifecycle :as lcm]
            [hive-mcp.extensions.registry :as ext]
            [hive-addon.lifecycle.store :as store]))

(defn- crawl-tool-handler
  "The crawl tool as the server would dispatch it: the stub while dormant, the
   wrapped addon tool while active."
  []
  (or (:handler (first (filter #(and (= "crawl" (:name %)) (::lcm/stub-of %))
                               (ext/get-registered-tools))))
      (:handler (first (filter #(= "crawl" (:name %)) (addon-core/active-addon-tools))))))

(defn- stub? []
  (boolean (some #(and (= "crawl" (:name %)) (::lcm/stub-of %)) (ext/get-registered-tools))))

(defn- snapshot [mgr label]
  {:step label
   :phase (lc/phase mgr "hive.crawl")
   :registered? (addon-core/addon-registered? "hive.crawl")
   :stub? (stub?)
   :state (select-keys (lc/state mgr "hive.crawl")
                       [:activations :evictions :in-flight :last-used-ms :surface/source :downgraded])})

(defn run-check! []
  (let [spec (first (filter #(= "hive.crawl" (:addon/id %)) (:specs (boundary/discover-specs))))
        t    (atom 1000)
        dir  (str (System/getProperty "java.io.tmpdir") "/lifecycle-live-" (System/nanoTime))
        mgr  (lc/manager {:host (lcm/host)
                          :specs [spec]
                          :overrides {"hive.crawl" {:policy :lazy :idle-ms 500}}
                          :now-ms #(deref t)
                          :surface-store (store/edn-dir-store dir)
                          :reload-ns! (fn [_ _] nil)})]
    (lc/install! mgr)
    (ext/register! lcm/wrap-handler-key lcm/wrap-handler)
    (let [boot  (lc/boot! mgr)
          s1    (assoc (snapshot mgr :boot) :boot (select-keys boot [:eager :dormant :downgraded]))
          ev1   (lc/evict! mgr "hive.crawl")
          s2    (assoc (snapshot mgr :evicted) :learned-tools (mapv :name (:tools (first (lc/surface-of mgr "hive.crawl")))))
          _     (swap! t + 10)
          out1  ((crawl-tool-handler) {:command "status"})
          s3    (assoc (snapshot mgr :first-call-through-stub) :error? (boolean (:isError out1))
                       :text (subs (str (:text out1)) 0 (min 160 (count (str (:text out1))))))
          _     (swap! t + 100)
          out2  ((crawl-tool-handler) {:command "status"})
          s4    (assoc (snapshot mgr :second-call-dispatched) :error? (boolean (:isError out2)))
          _     (swap! t + 200)
          sw1   (lc/sweep! mgr)
          s5    (assoc (snapshot mgr :sweep-while-fresh) :evicted (:evicted sw1))
          _     (swap! t + 1000)
          sw2   (lc/sweep! mgr)
          s6    (assoc (snapshot mgr :sweep-after-idle) :evicted (:evicted sw2))]
      (lcm/shutdown!)
      [s1 (assoc s2 :evict-report (select-keys ev1 [:evicted? :reason :errors])) s3 s4 s5 s6])))
