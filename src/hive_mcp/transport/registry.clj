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
   [:lane {:optional true} :keyword]
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

(def LaneView
  "One lane of a port: its narrowed chain, its own :active adapter and the
   health each adapter has in that lane."
  [:map
   [:chain [:vector :keyword]]
   [:active [:maybe :keyword]]
   [:resolved-by [:maybe [:enum :primary :fallback]]]
   [:adapters [:map-of :keyword chain/AdapterStatus]]])

(def PortView
  [:map
   [:chain [:vector :keyword]]
   [:active [:maybe :keyword]]
   [:resolved-by [:maybe [:enum :primary :fallback]]]
   [:sticky? :boolean]
   [:probe-interval-ms :int]
   [:probe-loop? :boolean]
   [:adapters [:map-of :keyword AdapterView]]
   [:lanes {:optional true} [:map-of :keyword LaneView]]
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
   ones go. :active (the port's, and each lane's) survives only when it is
   still in the chain and nothing now ranks above it that did not before;
   otherwise it is cleared, so the next call walks the new config order
   (config order is the preference)."
  [state old-chain spec]
  (let [chain-kws (:chain spec)
        fresh     (chain/init-state spec)
        kept      (select-keys (:adapters state) chain-kws)
        above     (fn [active ch] (set (take-while #(not= active %) ch)))
        keep      (fn [active]
                    (when (and (some #(= active %) chain-kws)
                               (every? (above active old-chain) (above active chain-kws)))
                      active))
        lanes     (into {} (map (fn [[l a]] [l (keep a)])) (:lanes state))]
    (cond-> {:active   (keep (:active state))
             :adapters (merge (:adapters fresh) kept)}
      (seq lanes) (assoc :lanes lanes))))

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
;; Lanes: one sticky :active per kind of target
;; =============================================================================
;;
;; An op may name a :lane (e.g. the delivery port's target kinds). A lane walks
;; the port's chain narrowed to the adapters that serve it (an adapter's
;; optional :serves set; no :serves serves every lane) and keeps its OWN
;; :active in the port state under [:lanes lane]. Adapter health stays one map
;; per port: an adapter that is down is down for every lane. So a call for one
;; kind of target never moves the :active another kind relies on.

(defn- serves?
  "True when adapter `a` serves `lane`. An unregistered adapter (nil) and one
   without :serves serve every lane."
  [a lane]
  (let [s (:serves a)]
    (or (nil? a) (nil? s) (contains? (set s) lane))))

(defn- lane-chain
  "The port chain narrowed to the adapters serving `lane`, config order kept."
  [spec adapters lane]
  (filterv #(serves? (get adapters %) lane) (:chain spec)))

(defn- lane-view
  "The ChainState a lane sees: its own :active over the port's adapter health."
  [state lane]
  {:active (get-in state [:lanes lane]) :adapters (:adapters state)})

(defn- merge-lane
  "Fold a lane's ChainState `v` back into the port `state`."
  [state lane v]
  (-> state
      (assoc :adapters (:adapters v))
      (assoc-in [:lanes lane] (:active v))))

(defn- lane-atom
  "An atom-like view of `lane` inside the port's `state-atom`: deref answers
   `lane-view`, swap! applies f to it and folds the result back atomically,
   so run-chain! and the probe drive a lane exactly as they drive a port."
  [state-atom lane]
  (let [apply-f (fn [f & args]
                  (lane-view (swap! state-atom
                                    (fn [s] (merge-lane s lane (apply f (lane-view s lane) args))))
                             lane))]
    (reify
      clojure.lang.IDeref
      (deref [_] (lane-view @state-atom lane))
      clojure.lang.IAtom
      (swap [_ f] (apply-f f))
      (swap [_ f x] (apply-f f x))
      (swap [_ f x y] (apply-f f x y))
      (swap [_ f x y more] (apply apply-f f x y more))
      (compareAndSet [_ _ _] (throw (UnsupportedOperationException. "lane views do not compare-and-set")))
      (reset [_ v] (lane-view (swap! state-atom merge-lane lane v) lane)))))

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
  [{:keys [port lane from to reason]}]
  (str "transport " (name port) (when lane (str "[" (subs (str lane) 1) "]"))
       ": " (or from "none") " -> " (or to "none")
       " (" (name reason) ")"))

(defn- shout-switch!
  "Default switch sink: a status-neutral hivemind shout."
  [sw]
  (when-let [shout! (requiring-resolve 'hive-mcp.hivemind.messaging/shout!)]
    (shout! "transport" :progress
            (assoc (select-keys sw [:port :lane :from :to :reason])
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
  "Record and announce `port` (`lane` of it, when non-nil) moving :active
   from `from` to `to`."
  [port lane reason from to at]
  (let [sw (cond-> {:port port :from from :to to :reason reason :at at}
             lane (assoc :lane lane))]
    (swap! switch-log #(vec (take-last switch-log-cap (conj % sw))))
    (log/info (switch-message sw))
    (r/rescue-log "transport switch sink" nil (@switch-sink sw))
    sw))

(defn- tracked!
  "Run `f` (which may move `state-atom`'s :active any number of times) and
   record ONE switch when :active ends elsewhere than it started: a failover
   inside one call is a->b, not a->none->b. `reason` nil derives it from the
   chain order. `lane` names the lane whose state this is (nil: the port's
   own). Answers what f answers."
  ([port chain-kws state-atom reason f]
   (tracked! port chain-kws state-atom reason f nil))
  ([port chain-kws state-atom reason f lane]
   (let [from (:active @state-atom)
         out  (f)
         to   (:active @state-atom)]
     (when (not= from to)
       (record-switch! port lane (or reason (switch-reason chain-kws from to))
                       from to (System/currentTimeMillis)))
     out)))

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
   its last success, the port's recent switches, and, for a port called with
   lanes, each lane's narrowed chain and its own active adapter."
  [spec state adapters switches loop? now]
  (let [active (:active state)
        lanes  (:lanes state)]
    (cond->
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
      :switches          (vec switches)}
      (seq lanes)
      (assoc :lanes
             (into {}
                   (map (fn [[l a]]
                          (let [ch (lane-chain spec adapters l)]
                            [l {:chain       ch
                                :active      a
                                :resolved-by (when a (if (= a (first ch)) :primary :fallback))
                                :adapters    (into {}
                                                   (map (fn [k] [k (get-in state [:adapters k :status] :unknown)]))
                                                   ch)}])))
                   lanes)))))

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
   An op map carrying :lane walks only the adapters serving that lane and
   keeps that lane's own sticky :active (see `lane-atom`), so :resolved-by
   is relative to the head of the lane's chain.
   opts: :now-fn / :sleep-fn (tests)."
  ([port op] (call! port op nil))
  ([port op opts]
   (if-let [{:keys [spec state adapters]} (refresh-adapters! port)]
     (let [lane (when (map? op) (:lane op))
           calls (update-vals adapters :call)]
       (if lane
         (let [lspec (assoc spec :chain (lane-chain spec adapters lane))]
           (tracked! port (:chain lspec) (lane-atom state lane) nil
                     #(chain/run-chain! lspec (lane-atom state lane) calls op opts)
                     lane))
         (tracked! port (:chain spec) state nil
                   #(chain/run-chain! spec state calls op opts))))
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

(defn- probe-round!
  "One probe round over one chain view (the port's, or one lane's): in config
   order, every adapter ranked above :active that is due and has a :probe fn
   is tested and folded through `chain/step`; the first success stops the
   round. Answers {:probed [..] :active kw}."
  [port lane spec state adapters now-fn force?]
  (loop [todo   (chain/probe-candidates @state
                                        (cond-> spec force? (assoc :probe-interval-ms 0))
                                        (now-fn))
         probed []]
    (let [k (first todo)
          p (get-in adapters [k :probe])]
      (cond
        (nil? k) {:probed probed :active (:active @state)}
        (nil? p) (recur (rest todo) probed)
        :else
        (let [out (run-probe p)]
          (tracked! port (:chain spec) state nil
                    #(swap! state chain/step k out (now-fn))
                    lane)
          (if (r/ok? out)
            {:probed (conj probed {:adapter k :ok? true}) :active (:active @state)}
            (recur (rest todo) (conj probed {:adapter k :ok? false}))))))))

(defn probe-port!
  "Probe `port` once: in config order, every adapter ranked above :active
   that is due (`chain/probe-candidates`) and has a :probe fn is tested, each
   outcome folded through `chain/step`. The first success stops the round and,
   being ranked above :active, takes over (law 4). Adapters without a :probe
   are not probed. A port called with lanes is probed once per lane, on the
   lane's narrowed chain and against the lane's own :active, so an adapter
   degraded in any lane gets its recovery probe. Answers {:port :probed
   [{:adapter :ok?}] :active}."
  ([port] (probe-port! port nil))
  ([port {:keys [now-fn force?] :or {now-fn #(System/currentTimeMillis)}}]
   (if-let [{:keys [spec state adapters]} (refresh-adapters! port)]
     (let [lanes (keys (:lanes @state))]
       (if (seq lanes)
         {:port   port
          :probed (into [] (mapcat (fn [l]
                                     (:probed (probe-round! port l
                                                            (assoc spec :chain (lane-chain spec adapters l))
                                                            (lane-atom state l) adapters now-fn force?))))
                        lanes)
          :active (:active @state)}
         (assoc (probe-round! port nil spec state adapters now-fn force?) :port port)))
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
