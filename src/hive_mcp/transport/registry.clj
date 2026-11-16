(ns hive-mcp.transport.registry
  "TransportChain registry: one chain per capability port, read from config.edn
   [:transports <port>] merged over [:transports :defaults].

   Pure half:
     ports          the configured port keywords
     port-spec      the ChainSpec of one port (defaults filled)
     rebind-state   carry a ChainState across a config change
     switch-reason  why :active moved from one adapter to another

   Open set:
     open-adapter   (defmulti, dispatch [port adapter-kw]) answers
                    {:call (fn [op]) -> Result, :probe (fn []) -> Result} or a
                    bare call fn; nil (the default) means not registered

   Effectful half:
     rebind!, rebind-from-config!    (re)build the registry from config
     call!                           run an op through a port's chain
     probe-port!                     re-test adapters ranked above :active
     start-probe-loops!, stop-probe-loops!, install!, status, clear!
     set-switch-sink!                where an :active switch is announced

   Every change of a port's :active adapter is recorded once as
   {:port :from :to :reason :at} and handed to the switch sink (default: a
   status-neutral hivemind shout)."
  (:require [hive-dsl.result :as r]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.merge :as merge]
            [hive-mcp.transport.chain :as chain]
            [malli.core :as m]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Schemas
;; =============================================================================

(def Transports
  "The :transports section of config.edn: port keyword -> partial ChainSpec,
   plus :defaults merged under every port."
  [:maybe [:map-of :keyword [:maybe :map]]])

(def SwitchReason
  [:enum :elected :lost :recovered :failover :reconfigured])

(def Switch
  [:map
   [:port :keyword]
   [:from [:maybe :keyword]]
   [:to [:maybe :keyword]]
   [:reason SwitchReason]
   [:at :int]])

(def AdapterView
  [:map
   [:status chain/AdapterStatus]
   [:registered? :boolean]
   [:probe? :boolean]
   [:last-ok-at [:maybe :int]]
   [:last-ok-age-ms [:maybe :int]]
   [:fails :int]
   [:last-err [:maybe :map]]])

(def PortView
  [:map
   [:chain [:vector :keyword]]
   [:active [:maybe :keyword]]
   [:resolved-by [:maybe [:enum :primary :fallback]]]
   [:sticky? :boolean]
   [:probe-interval-ms :int]
   [:probe-loop? :boolean]
   [:adapters [:map-of :keyword AdapterView]]
   [:switches [:vector Switch]]])

(def Status
  [:map
   [:ports [:map-of :keyword PortView]]
   [:probe-loops? :boolean]
   [:switches [:vector Switch]]])

;; =============================================================================
;; Pure core
;; =============================================================================

(defn ports
  "The configured ports of `transports`, sorted; :defaults is not a port.
   nil means the shipped defaults."
  [transports]
  (->> (keys (or transports merge/default-transports))
       (remove #(= :defaults %))
       sort
       vec))

(defn port-spec
  "ChainSpec of `port`: the port entry merged over :defaults (both over the
   shipped defaults). A port absent from config has an empty chain."
  [transports port]
  (let [t        (or transports merge/default-transports)
        defaults (merge/deep-merge (:defaults merge/default-transports)
                                   (or (:defaults t) {}))
        entry    (merge {:chain []} (get t port))]
    (chain/normalize-spec (merge/deep-merge defaults entry))))

(defn rebind-state
  "Carry `state` from a chain ordered `old-chain` over to `spec`: adapters
   still in the chain keep their history, new ones start :unknown, dropped
   ones go. :active survives only when it is still in the chain and nothing
   now ranks above it that did not before; otherwise it is cleared, so the
   next call walks the new config order (config order is the preference)."
  [state old-chain spec]
  (let [chain-kws (:chain spec)
        fresh     (chain/init-state spec)
        kept      (select-keys (:adapters state) chain-kws)
        active    (:active state)
        above     (fn [ch] (set (take-while #(not= active %) ch)))
        keep?     (and (some #(= active %) chain-kws)
                       (every? (above old-chain) (above chain-kws)))]
    {:active   (when keep? active)
     :adapters (merge (:adapters fresh) kept)}))

(defn switch-reason
  "Why :active moved from `from` to `to` on a chain ranked by `chain-kws`.
   nil to: :lost. nil from: :elected. to ranked above from: :recovered.
   Otherwise :failover."
  [chain-kws from to]
  (let [rank (fn [k] (or (first (keep-indexed #(when (= k %2) %1) chain-kws))
                         Long/MAX_VALUE))]
    (cond
      (nil? to)                 :lost
      (nil? from)               :elected
      (< (rank to) (rank from)) :recovered
      :else                     :failover)))

(m/=> ports [:=> [:cat Transports] [:vector :keyword]])
(m/=> port-spec [:=> [:cat Transports :keyword] chain/ChainSpec])
(m/=> rebind-state [:=> [:cat chain/ChainState [:vector :keyword] chain/ChainSpec] chain/ChainState])
(m/=> switch-reason [:=> [:cat [:vector :keyword] [:maybe :keyword] [:maybe :keyword]] SwitchReason])

;; =============================================================================
;; Open set: adapters
;; =============================================================================

(defmulti open-adapter
  "The adapter `adapter-kw` of `port`: {:call (fn [op]) -> Result and optional
   :probe (fn []) -> Result}, or a bare call fn. nil means not registered, so
   the chain skips it and says so."
  (fn [port adapter-kw] [port adapter-kw]))

(defmethod open-adapter :default [_ _] nil)

(defn- as-adapter
  "An open-adapter answer as {:call f :probe f?}, or nil."
  [a]
  (cond
    (map? a) (when (ifn? (:call a)) a)
    (ifn? a) {:call a}
    :else    nil))

;; =============================================================================
;; Registry state
;; =============================================================================

(defonce ^:private registry
  ;; port -> {:spec ChainSpec :state (atom ChainState) :adapters {kw adapter}}
  (atom {}))

(defonce ^:private switch-log (atom []))

(def ^:private switch-log-cap 50)

(defonce ^:private loops
  ;; nil, or {:executor ScheduledExecutorService :ports {port interval-ms}}
  (atom nil))

;; =============================================================================
;; Switch announcements
;; =============================================================================

(defn- switch-message
  [{:keys [port from to reason]}]
  (str "transport " (name port) ": " (or from "none") " -> " (or to "none")
       " (" (name reason) ")"))

(defn- shout-switch!
  "Default switch sink: a status-neutral hivemind shout."
  [sw]
  (when-let [shout! (requiring-resolve 'hive-mcp.hivemind.messaging/shout!)]
    (shout! "transport" :progress
            (assoc (select-keys sw [:port :from :to :reason])
                   :message (switch-message sw)
                   :status-neutral? true))))

(defonce ^:private switch-sink (atom #'shout-switch!))

(defn set-switch-sink!
  "Announce :active switches through `f` (fn [Switch]); nil restores the
   hivemind shout. Answers the previous sink."
  [f]
  (let [[prev _] (reset-vals! switch-sink (or f #'shout-switch!))]
    prev))

(m/=> set-switch-sink! [:=> [:cat [:maybe ifn?]] ifn?])

(defn- record-switch!
  "Record and announce `port` moving :active from `from` to `to`."
  [port reason from to at]
  (let [sw {:port port :from from :to to :reason reason :at at}]
    (swap! switch-log #(vec (take-last switch-log-cap (conj % sw))))
    (log/info (switch-message sw))
    (r/rescue-log "transport switch sink" nil (@switch-sink sw))
    sw))

(defn- tracked!
  "Run `f` (which may move `state-atom`'s :active any number of times) and
   record ONE switch when :active ends elsewhere than it started: a failover
   inside one call is a->b, not a->none->b. `reason` nil derives it from the
   chain order. Answers what f answers."
  [port chain-kws state-atom reason f]
  (let [from (:active @state-atom)
        out  (f)
        to   (:active @state-atom)]
    (when (not= from to)
      (record-switch! port (or reason (switch-reason chain-kws from to))
                      from to (System/currentTimeMillis)))
    out))

;; =============================================================================
;; Status view (pure)
;; =============================================================================

(defn- adapter-view
  [entry adapter now]
  (let [last-ok (:last-ok-at entry)]
    {:status         (:status entry :unknown)
     :registered?    (some? adapter)
     :probe?         (ifn? (:probe adapter))
     :last-ok-at     last-ok
     :last-ok-age-ms (when last-ok (- now last-ok))
     :fails          (or (:fails entry) 0)
     :last-err       (:last-err entry)}))

(defn port-view
  "What `transport status` shows for one port: the chain, the active adapter
   and whether it is the preferred one, each adapter's health and the age of
   its last success, and the port's recent switches."
  [spec state adapters switches loop? now]
  (let [active (:active state)]
    {:chain             (:chain spec)
     :active            active
     :resolved-by       (when active
                          (if (= active (first (:chain spec))) :primary :fallback))
     :sticky?           (boolean (:sticky? spec))
     :probe-interval-ms (:probe-interval-ms spec)
     :probe-loop?       (boolean loop?)
     :adapters          (into {}
                              (map (fn [k]
                                     [k (adapter-view (get-in state [:adapters k])
                                                      (get adapters k) now)]))
                              (:chain spec))
     :switches          (vec switches)}))

(m/=> port-view [:=> [:cat chain/ChainSpec chain/ChainState [:map-of :keyword :map]
                      [:sequential Switch] :boolean :int]
                 PortView])

;; =============================================================================
;; Binding
;; =============================================================================

(defn- open-adapters
  "{kw adapter} for every adapter of `chain-kws` that open-adapter answers.
   A throwing defmethod counts as not registered and is logged."
  [port chain-kws]
  (into {}
        (keep (fn [k]
                (when-let [a (as-adapter
                              (r/rescue-log (str "open-adapter " port " " k) nil
                                            (open-adapter port k)))]
                  [k a])))
        chain-kws))

(defn- bind-port
  "The registry entry of `port` for `spec`, reusing `prev`'s state atom so its
   history survives a rebind. A rebind that clears :active is announced as
   :reconfigured."
  [port spec prev]
  (let [state-atom (or (:state prev) (atom (chain/init-state spec)))]
    (tracked! port (:chain spec) state-atom :reconfigured
              #(swap! state-atom rebind-state (get-in prev [:spec :chain] []) spec))
    {:spec spec :state state-atom :adapters (open-adapters port (:chain spec))}))

(declare start-probe-loops!)

(defn rebind!
  "(Re)build the registry from a :transports config section (nil: the shipped
   defaults). Kept ports carry their state over (`rebind-state`); ports no
   longer configured go. Running probe loops restart on the new intervals.
   Answers {port {:chain [..] :registered [..]}}."
  [transports]
  (let [ps    (ports transports)
        prev  @registry
        bound (into {} (map (fn [p] [p (bind-port p (port-spec transports p) (get prev p))])) ps)]
    (reset! registry bound)
    (when @loops (start-probe-loops!))
    (into {}
          (map (fn [[p {:keys [spec adapters]}]]
                 [p {:chain (:chain spec) :registered (filterv adapters (:chain spec))}]))
          bound)))

(defn rebind-from-config!
  "`rebind!` from config.edn's :transports. The config-reload port."
  []
  (rebind! (config/get-in-config [:transports])))

(def ^:private BindReport
  [:map-of :keyword [:map [:chain [:vector :keyword]] [:registered [:vector :keyword]]]])

(m/=> rebind! [:=> [:cat Transports] BindReport])
(m/=> rebind-from-config! [:=> [:cat] BindReport])

(defn- ensure-bound!
  "The registry, bound from config on first use."
  []
  (when (empty? @registry) (rebind-from-config!))
  @registry)

(defn- refresh-adapters!
  "Entry of `port` with adapters registered since the last bind picked up
   (an addon may load after boot)."
  [port]
  (when-let [{:keys [spec adapters]} (get (ensure-bound!) port)]
    (let [missing (remove adapters (:chain spec))]
      (when (seq missing)
        (let [found (open-adapters port missing)]
          (when (seq found)
            (swap! registry update-in [port :adapters] merge found)))))
    (get @registry port)))

;; =============================================================================
;; Calls and probes
;; =============================================================================

(defn- unknown-port
  [port]
  (r/err :transport/unknown-port
         {:error/kind :addressing
          :port       port
          :message    (str "no transport chain for port " port)
          :via        nil
          :skipped    []}))

(defn call!
  "Run `op` through `port`'s chain (`chain/run-chain!`): sticky active first,
   retry with backoff, fall back in config order. The Result names :via,
   :resolved-by and :skipped. An unconfigured port is an addressing error.
   opts: :now-fn / :sleep-fn (tests)."
  ([port op] (call! port op nil))
  ([port op opts]
   (if-let [{:keys [spec state adapters]} (refresh-adapters! port)]
     (tracked! port (:chain spec) state nil
               #(chain/run-chain! spec state (update-vals adapters :call) op opts))
     (unknown-port port))))

(m/=> call! [:function
             [:=> [:cat :keyword :any] :map]
             [:=> [:cat :keyword :any [:maybe :map]] :map]])

(defn- run-probe
  [probe-fn]
  (try
    (r/ensure-result (probe-fn))
    (catch Exception e
      (r/err :transport/probe-exception {:error/kind :unavailable :message (ex-message e)}))))

(defn probe-port!
  "Probe `port` once: in config order, every adapter ranked above :active
   that is due (`chain/probe-candidates`) and has a :probe fn is tested, each
   outcome folded through `chain/step`. The first success stops the round and,
   being ranked above :active, takes over (law 4). Adapters without a :probe
   are not probed. Answers {:port :probed [{:adapter :ok?}] :active}."
  ([port] (probe-port! port nil))
  ([port {:keys [now-fn] :or {now-fn #(System/currentTimeMillis)}}]
   (if-let [{:keys [spec state adapters]} (refresh-adapters! port)]
     (loop [todo   (chain/probe-candidates @state spec (now-fn))
            probed []]
       (let [k (first todo)
             p (get-in adapters [k :probe])]
         (cond
           (nil? k) {:port port :probed probed :active (:active @state)}
           (nil? p) (recur (rest todo) probed)
           :else
           (let [out (run-probe p)]
             (tracked! port (:chain spec) state nil
                       #(swap! state chain/step k out (now-fn)))
             (if (r/ok? out)
               {:port port :probed (conj probed {:adapter k :ok? true}) :active (:active @state)}
               (recur (rest todo) (conj probed {:adapter k :ok? false})))))))
     {:port port :probed [] :active nil})))

(def ^:private ProbeReport
  [:map [:port :keyword] [:probed [:vector [:map [:adapter :keyword] [:ok? :boolean]]]]
   [:active [:maybe :keyword]]])

(m/=> probe-port! [:function
                   [:=> [:cat :keyword] ProbeReport]
                   [:=> [:cat :keyword [:maybe :map]] ProbeReport]])

;; =============================================================================
;; Probe loops
;; =============================================================================

(defn- probe-tick!
  [port]
  (r/rescue-log (str "transport probe " port) nil (probe-port! port)))

(defn stop-probe-loops!
  "Stop every probe loop. Answers the ports that had one."
  []
  (let [[old _] (reset-vals! loops nil)]
    (when-let [^java.util.concurrent.ScheduledExecutorService ex (:executor old)]
      (.shutdownNow ex))
    (vec (sort (keys (:ports old))))))

(defn start-probe-loops!
  "(Re)start one background probe loop per bound port whose
   :probe-interval-ms is positive, on that interval. A daemon thread, so it
   never holds the JVM open. Answers {port interval-ms}."
  []
  (stop-probe-loops!)
  (let [entries (ensure-bound!)
        due     (into {}
                      (keep (fn [[p {:keys [spec]}]]
                              (let [ms (:probe-interval-ms spec)]
                                (when (and (pos-int? ms) (seq (:chain spec))) [p ms]))))
                      entries)
        tf      (reify java.util.concurrent.ThreadFactory
                  (newThread [_ runnable]
                    (doto (Thread. ^Runnable runnable "transport-probe")
                      (.setDaemon true))))
        ex      (java.util.concurrent.Executors/newSingleThreadScheduledExecutor tf)]
    (doseq [[p ms] due]
      (.scheduleWithFixedDelay ex ^Runnable (fn [] (probe-tick! p))
                               (long ms) (long ms)
                               java.util.concurrent.TimeUnit/MILLISECONDS))
    (reset! loops {:executor ex :ports due})
    due))

(m/=> stop-probe-loops! [:=> [:cat] [:vector :keyword]])
(m/=> start-probe-loops! [:=> [:cat] [:map-of :keyword :int]])

;; =============================================================================
;; Lifecycle and status
;; =============================================================================

(defn install!
  "Bind the registry from config and start the probe loops. Idempotent."
  []
  {:bound  (rebind-from-config!)
   :probes (start-probe-loops!)})

(m/=> install! [:=> [:cat] [:map [:bound BindReport] [:probes [:map-of :keyword :int]]]])

(defn clear!
  "Stop the probe loops and forget every port and switch (tests, shutdown)."
  []
  (stop-probe-loops!)
  (reset! registry {})
  (reset! switch-log [])
  nil)

(m/=> clear! [:=> [:cat] :nil])

(defn status
  "Every bound port's view (`port-view`) plus the recent switches across ports."
  []
  (let [now     (System/currentTimeMillis)
        entries (ensure-bound!)
        sws     @switch-log
        looping (:ports @loops)]
    {:ports        (into {}
                         (map (fn [[p {:keys [spec state adapters]}]]
                                [p (port-view spec @state adapters
                                              (filterv #(= p (:port %)) sws)
                                              (contains? looping p) now)]))
                         entries)
     :probe-loops? (some? @loops)
     :switches     sws}))

(m/=> status [:=> [:cat] Status])
