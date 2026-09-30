(ns hive-mcp.extensions.lifecycle-test
  "A lazy addon hosted by hive-mcp: advertised by stubs while dormant, mounted
   by the first call that reaches a stub, counted as used by dispatch, and
   returned to stubs by a sweep."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.lifecycle :as lc]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addon-core]
            [hive-mcp.extensions.lifecycle :as lcm]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.composite :as composite]
            [hive-addon.registry.commands :as acmds]
            [hive-mcp.hot.reseat :as reseat]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce generations (atom 0))
(defonce seen-in-flight (atom nil))
(def ^:dynamic *mgr* nil)

(defrecord ProbeAddon [gen]
  proto/IAddon
  (addon-id [_] "probe.lc")
  (addon-type [_] :native)
  (capabilities [_] #{:tools})
  (initialize! [_ _]
    (ext/contribute-commands! "code" "probe.lc"
                              {"probe-lc" {:handler (fn [_]
                                                      (reset! seen-in-flight
                                                              (:in-flight (lc/state *mgr* "probe.lc")))
                                                      {:type "text" :text (str "pong " gen)})}})
    {:success? true :errors []})
  (shutdown! [_] nil)
  (tools [_] [{:name "probe_lc_tool" :description "probe"
               :inputSchema {:type "object" :properties {}}
               :handler (fn [_] {:type "text" :text (str "tool " gen)})}])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] {}))

(defn make-probe [_config] (->ProbeAddon (swap! generations inc)))

(def probe-spec
  {:addon/id "probe.lc" :addon/type :native
   :addon/init-ns "hive-mcp.extensions.lifecycle-test" :addon/init-fn "make-probe"
   :addon/capabilities #{:tools}
   :addon/lifecycle {:policy :lazy :idle-ms 100}
   :addon/surface {:tools [{:name "probe_lc_tool" :description "probe"}]
                   :commands {"code" ["probe-lc"]}}})

(defn- clean [f]
  (ext/clear-all!)
  (addon-core/reset-registry!)
  (lcm/shutdown!)
  (reset! generations 0)
  (reset! seen-in-flight nil)
  (try (f)
       (finally
         (lcm/shutdown!)
         (ext/clear-all!)
         (addon-core/reset-registry!))))

(use-fixtures :each clean)

(defn- world []
  (let [t   (atom 1000)
        mgr (lc/manager {:host (lcm/host {:resolve-config (constantly {})})
                         :specs [probe-spec]
                         :now-ms #(deref t)
                         :reload-ns! (fn [_ _] nil)})]
    (lc/install! mgr)
    (ext/register! lcm/wrap-handler-key lcm/wrap-handler)
    {:mgr mgr :t t}))

(defn- code-call [cmd]
  ((composite/build-merged-handler "code" {}) {:command cmd}))

(defn- command-owner [cmd]
  (:addon (get (acmds/get-commands "code") cmd)))

(defn- registered-tool [n]
  (first (filter #(= n (:name %)) (ext/get-registered-tools))))

(deftest a-dormant-addon-is-advertised-by-stubs-and-mounts-on-first-call
  (let [{:keys [mgr]} (world)]
    (binding [*mgr* mgr]
      (let [boot (lc/boot! mgr)]
        (is (= ["probe.lc"] (:dormant boot)))
        (is (zero? @generations) "nothing constructed at boot")
        (is (= "probe.lc#dormant" (command-owner "probe-lc")))
        (is (= "probe.lc" (get (registered-tool "probe_lc_tool") ::lcm/stub-of))))
      (testing "the first call mounts it and reaches the real handler"
        (is (= "pong 1" (:text (code-call "probe-lc"))))
        (is (= :active (lc/phase mgr "probe.lc")))
        (is (= "probe.lc" (command-owner "probe-lc")) "the real contribution replaced the stub")
        (is (nil? (get (registered-tool "probe_lc_tool") ::lcm/stub-of)) "the stub tool is gone"))
      (testing "a dispatched call is counted in flight while it runs"
        (reset! seen-in-flight nil)
        (is (= "pong 1" (:text (code-call "probe-lc"))))
        (is (= 1 @seen-in-flight))
        (is (zero? (:in-flight (lc/state mgr "probe.lc"))))))))

(deftest an-idle-addon-is-swept-back-to-stubs-and-remounts-fresh
  (let [{:keys [mgr t]} (world)]
    (binding [*mgr* mgr]
      (lc/boot! mgr)
      (code-call "probe-lc")
      (swap! t + 50)
      (code-call "probe-lc")
      (swap! t + 60)
      (is (= [] (:evicted (lc/sweep! mgr))) "the second call reset the idle clock")
      (swap! t + 200)
      (is (= ["probe.lc"] (:evicted (lc/sweep! mgr))))
      (is (not (addon-core/addon-registered? "probe.lc")))
      (is (= "probe.lc#dormant" (command-owner "probe-lc")))
      (is (= "probe.lc" (get (registered-tool "probe_lc_tool") ::lcm/stub-of)))
      (is (= "pong 2" (:text (code-call "probe-lc"))) "a new instance"))))

(deftest a-stub-tool-activates-and-answers-from-the-real-tool
  (let [{:keys [mgr]} (world)]
    (binding [*mgr* mgr]
      (lc/boot! mgr)
      (let [stub (registered-tool "probe_lc_tool")]
        (is (= "tool 1" (:text ((:handler stub) {}))))
        (is (= :active (lc/phase mgr "probe.lc")))))))

(deftest addon-tool-dispatch-is-wrapped-only-while-a-wrap-is-registered
  (let [{:keys [mgr]} (world)]
    (lc/boot! mgr)
    (lc/activate! mgr "probe.lc")
    (let [before (:last-used-ms (lc/state mgr "probe.lc"))
          tool   (first (filter #(= "probe_lc_tool" (:name %)) (addon-core/active-addon-tools)))]
      (is (= "tool 1" (:text ((:handler tool) {}))))
      (is (<= before (:last-used-ms (lc/state mgr "probe.lc")))))
    (ext/deregister! lcm/wrap-handler-key)
    (let [raw (:handler (first (filter #(= "probe_lc_tool" (:name %)) (addon-core/active-addon-tools))))]
      (is (= "tool 1" (:text (raw {})))))))

(deftest a-failed-activation-answers-an-mcp-error-and-keeps-the-stubs
  (let [t   (atom 0)
        mgr (lc/manager {:host (lcm/host {:resolve-config (constantly {})})
                         :specs [(assoc probe-spec :addon/init-fn "no-such-ctor")]
                         :now-ms #(deref t)
                         :reload-ns! (fn [_ _] nil)})]
    (lc/install! mgr)
    (lc/boot! mgr)
    (let [out (code-call "probe-lc")]
      (is (:isError out))
      (is (re-find #"could not be activated" (:text out))))
    (is (= "probe.lc#dormant" (command-owner "probe-lc")))))

(deftest a-reload-of-the-host-namespace-is-re-seated-keeping-the-manager-state
  (let [{:keys [mgr]} (world)]
    (binding [*mgr* mgr]
      (lc/boot! mgr)
      (lc/start-sweeper! mgr {:interval-ms 3600000})
      (let [old-class (class (:host mgr))]
        (is (not (lcm/stale-host? (:host mgr))) "a host built by the current constructor is current")
        (is (= {:reseated? false :reason :current} (lcm/reseat-host! [])))
        ;; What a core reload does to this namespace: a new McpLifecycleHost class.
        (require 'hive-mcp.extensions.lifecycle :reload)
        (is (lcm/stale-host? (:host (lc/installed-manager))) "the installed host is now the OLD class")
        (let [report (first (reseat/reseat! ["hive-mcp.other" "hive-mcp.extensions.lifecycle"]))
              mgr'   (lc/installed-manager)]
          (testing "the registry ran the re-seater the reloaded namespace registered"
            (is (= "hive-mcp.extensions.lifecycle" (:ns report)))
            (is (true? (get-in report [:result :reseated?])))
            (is (= ["probe.lc"] (get-in report [:result :rearmed])) "the dormant addon's stubs were re-armed"))
          (testing "the installed host is the NEW record class, carrying the old fields"
            (is (identical? (class (lcm/host)) (class (:host mgr'))))
            (is (not (identical? old-class (class (:host mgr')))))
            (is (= (into {} (:host mgr)) (into {} (:host mgr')))))
          (testing "no state was copied: the manager's atoms are the same objects"
            (is (identical? (:states mgr) (:states mgr')))
            (is (identical? (:specs mgr) (:specs mgr')))
            (is (some? @(:sweeper mgr')) "the sweeper moved to the re-seated manager"))
          (testing "activate and evict still work through the re-seated host"
            (binding [*mgr* mgr']
              (is (= "pong 1" (:text (code-call "probe-lc"))) "the re-armed stub activates")
              (is (= :active (lc/phase mgr' "probe.lc")))
              (is (:evicted? (lc/evict! mgr' "probe.lc")))
              (is (= "probe.lc#dormant" (command-owner "probe-lc")))
              (is (= "pong 2" (:text (code-call "probe-lc"))) "and re-activates a fresh instance")))
          (is (= {:reseated? false :reason :current} (lcm/reseat-host! []))
              "a second run finds nothing stale"))))))
