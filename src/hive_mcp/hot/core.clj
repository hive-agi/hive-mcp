(ns hive-mcp.hot.core
  "Hot-reload of hive-mcp's OWN namespaces through hive-hot, scoped the way
   the `hot` tool scopes an addon reload.

   Collect: the classpath root core's source resolves to, the loaded
   namespaces that define a protocol (hive-mcp.hot.self) and the loaded
   namespaces that hold live state (hive-mcp.hot.keep). Promote: the
   interlock those sets imply and the projection of a hive-hot plan.
   Boundary: `plan` (what a reload would do) and `reload!` (do it, then the
   repairs a namespace reload cannot make by itself: refresh the tool table
   and the advertised surface, remount the addons whose constructor
   namespace the pass loaded).

   A protocol definer is pinned :no-reload (reloading it orphans every
   instance built against the old protocol object). State is carried across
   the pass as clj-reload keeps derived by hive-mcp.hot.keep: the namespace
   unloads and reloads normally, its defonce roots and resource-holding defs
   keep their identity. Memory 20260628171812-74607e8c names the failure
   modes this exists for.

   Every effect goes through a port in `default-ports`, resolved through the
   var at call time; a caller may inject its own."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [hive-dsl.result :as r]
            [hive-mcp.hot.keep :as keep]
            [hive-mcp.hot.self :as self]
            [taoensso.timbre :as log])
  (:import [java.io File]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Collect
;; =============================================================================

(def anchor-ns
  "The namespace whose classpath location names core's source root."
  'hive-mcp.server.core)

(defn ns-root
  "Classpath ROOT directory `ns-sym`'s source is loaded from, or nil when the
   namespace resolves to a jar or to nothing."
  [ns-sym]
  (let [rel (str (str/replace (munge (str ns-sym)) "." "/") ".clj")
        url (io/resource rel)]
    (when (and url (= "file" (.getProtocol url)))
      (let [path   (.getPath (io/file (.toURI url)))
            suffix (str File/separator (str/replace rel "/" File/separator))]
        (when (str/ends-with? path suffix)
          (subs path 0 (- (count path) (count suffix))))))))

(defn core-roots
  "Directory roots core is loaded from: [root], or [] when core runs from a jar."
  ([] (core-roots anchor-ns))
  ([ns-sym] (if-let [root (ns-root ns-sym)] [root] [])))

(defn defines-record?
  "Does `ns-sym` define a record? `defrecord` interns map->Name."
  [ns-sym]
  (boolean (some #(str/starts-with? (name %) "map->") (keys (ns-publics ns-sym)))))

(defn- loaded-under
  [prefix]
  (into [] (comp (map ns-name) (filter #(str/starts-with? (str %) prefix))) (all-ns)))

(defn classify
  "The loaded namespaces under `prefix`, in the three classes the interlock
   and the report read: {:protocol #{..} :state #{..} :record #{..}}."
  ([] (classify self/default-prefix))
  ([prefix]
   (let [nses (loaded-under prefix)]
     {:protocol (into #{} (filter self/defines-protocol?) nses)
      :state    (into #{} (filter keep/holds-state?) nses)
      :record   (into #{} (filter defines-record?) nses)})))

;; =============================================================================
;; Promote
;; =============================================================================

(defn interlock
  "hive-hot options a core reload runs under: {:no-reload #{..}}. State is
   not pinned; it is kept (hive-mcp.hot.keep)."
  [{:keys [protocol]}]
  {:no-reload (set protocol)})

(defn file->ns
  "Namespace a source file under one of `roots` conventionally defines, or nil."
  [roots f]
  (let [p (.getPath (io/file f))]
    (some (fn [root]
            (let [root (str root)]
              (when (str/starts-with? p (str root File/separator))
                (-> (subs p (inc (count root)))
                    (str/replace #"\.clj[cs]?$" "")
                    (str/replace File/separator ".")
                    (str/replace "_" "-")))))
          roots)))

(defn affected
  "The namespaces a pass over `scope-plan` may unload: the cascade minus what
   the interlock pins."
  [{:keys [cascade]} classes]
  (vec (remove (:no-reload (interlock classes)) cascade)))

(defn plan-report
  "Project a hive-hot scope-plan onto the reload it describes. `ns-of` maps a
   file to the namespace it defines (nil keeps the path)."
  [{:keys [want dragged skipped cascade] :as sp} roots classes ns-of]
  (let [{:keys [no-reload]} (interlock classes)
        cascade (set cascade)
        nses    (fn [files]
                  (into [] (comp (map #(or (ns-of %) (str %))) (distinct)) files))
        names   (fn [s] (mapv str (sort s)))]
    {:roots             (vec roots)
     :pending           (nses want)
     :dragged           (nses dragged)
     :skipped           (nses skipped)
     :cascade           (names cascade)
     :pinned            (names (set/intersection cascade no-reload))
     :kept-state        (names (set/intersection (set (affected sp classes)) (set (:state classes))))
     :records-redefined (names (set/intersection cascade (set (:record classes))))
     :interlock         {:no-reload (count no-reload)}
     :unchanged?        (empty? want)}))

;; =============================================================================
;; Boundary
;; =============================================================================

(defn- soft [sym] (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn- jvm-start-ms []
  (try (.getStartTime (java.lang.management.ManagementFactory/getRuntimeMXBean))
       (catch Throwable _ nil)))

(defn default-ports
  "The effects a reload drives, each resolved through its var now.
   :host/remount! is nil here: only the `hot` tool knows the mounted specs."
  []
  {:hot/ensure-init!      (soft 'hive-hot.core/ensure-init!)
   :hot/status            (soft 'hive-hot.core/status)
   :hot/scope-plan        (soft 'hive-hot.core/scope-plan)
   :hot/repair-preview    keep/repair-preview
   :hot/prepare-pass!     keep/prepare-pass!
   :hot/reload-scoped!    (soft 'hive-hot.core/reload-scoped!)
   :hot/reload-pending!   keep/run-pending!
   :host/refresh-tools!   (when-let [refresh (soft 'hive-mcp.server.routes/refresh-tools!)]
                            (when-let [ctx (soft 'hive-mcp.server.core/server-context-atom)]
                              (fn [] (refresh @ctx))))
   :host/refresh-surface! (when-let [refresh (soft 'hive-mcp.extensions.reactive/refresh-surface!)]
                            (fn [] (refresh nil)))
   :host/remount!         nil})

(defn- prepare!
  "Extend hive-hot with core's roots and the interlock. Returns
   {:roots :classes :interlock :hot-init} or {:error kw :message str}."
  [{:keys [prefix ports roots]}]
  (let [classes (classify prefix)
        lock    (interlock classes)]
    (cond
      (empty? roots)
      {:error :restart-required
       :message "hive-mcp core is loaded from a jar; its bytes cannot change without a restart"}

      (nil? (:hot/ensure-init! ports))
      {:error :hive-hot-absent :message "hive-hot is not on the classpath"}

      :else
      (let [opts (cond-> (assoc lock :dirs (vec roots))
                   (jvm-start-ms) (assoc :since (jvm-start-ms)))
            res  (r/try-effect ((:hot/ensure-init! ports) opts))]
        (if (r/err? res)
          {:error :hot-init-failed :message (:message res)}
          {:roots (vec roots) :classes classes :interlock lock :hot-init (:ok res)})))))

(defn- options
  [{:keys [prefix ports roots] :as opts}]
  (assoc opts
         :prefix (or prefix self/default-prefix)
         :ports  (or ports (default-ports))
         :roots  (or roots (core-roots))))

(defn- repair!
  "Run one host port, folding a throw into the report."
  [ports k & args]
  (when-let [f (get ports k)]
    (let [res (r/try-effect (apply f args))]
      (if (r/err? res) {:error (:message res)} (:ok res)))))

(defn plan
  "What a core reload WOULD do: the changes under core's root, and the repairs
   the image needs regardless of any change. Effect-free apart from extending
   hive-hot with core's root and the interlock."
  ([] (plan {}))
  ([opts]
   (let [{:keys [ports prefix] :as opts} (options opts)
         {:keys [roots classes error] :as prep} (prepare! opts)]
     (if error
       (select-keys prep [:error :message])
       (let [dirs (mapv str (:dirs ((:hot/status ports))))
             sp   ((:hot/scope-plan ports) roots)]
         (assoc (plan-report sp roots classes (partial file->ns dirs))
                :repair (repair! ports :hot/repair-preview prefix)))))))

(defn reload!
  "Reload the changes under core's roots, plus whatever repair the image
   needs, and repair what a namespace reload leaves behind. Never throws;
   answers a report:
     :ok? :pass :roots :loaded :unloaded :failed :error :ms :skipped :dragged
     :unchanged? :multi-file :stale-registrations :interlock :kept-vars
     :forced-unload :orphans :dead-links :kept-state :records-redefined
     :remount :tools-refreshed :surface :hive-hot

   :pass is :scoped when a change under the roots drove hive-hot, :pending
   when nothing changed but the image had repairs queued, :none otherwise."
  ([] (reload! {}))
  ([opts]
   (let [{:keys [ports prefix] :as opts} (options opts)
         {:keys [roots classes interlock hot-init error] :as prep} (prepare! opts)]
     (if error
       (assoc (select-keys prep [:error :message]) :ok? false)
       (let [sp       ((:hot/scope-plan ports) roots)
             pass     (repair! ports :hot/prepare-pass! prefix (affected sp classes))
             changed? (seq (:want sp))
             queued?  (or (seq (:forced pass)) (seq (:orphans pass)))
             kind     (cond changed? :scoped queued? :pending :else :none)
             res      (let [r (case kind
                                :scoped  ((:hot/reload-scoped! ports) roots)
                                :pending ((:hot/reload-pending! ports))
                                :none    {:success true :loaded [] :unloaded [] :unchanged? true})]
                        (if (map? r) r {:success false :error (str "reloader answered " (pr-str r))}))
             ok?      (boolean (:success res))
             loaded   (mapv str (:loaded res))
             loaded*  (set (map symbol loaded))
             names    (fn [s] (mapv str (sort (set/intersection loaded* (set s)))))
             repair?  (and ok? (seq loaded))
             remount  (when repair? (repair! ports :host/remount! loaded))
             tools    (when repair? (repair! ports :host/refresh-tools!))
             surface  (when repair? (repair! ports :host/refresh-surface!))]
         (log/info "core hot-reload" {:ok? ok? :pass kind :loaded (count loaded)
                                      :failed (:failed res) :ms (:ms res)})
         {:ok?                 ok?
          :pass                kind
          :roots               roots
          :loaded              loaded
          :unloaded            (mapv str (:unloaded res))
          :failed              (some-> (:failed res) str)
          :error               (:error res)
          :ms                  (:ms res)
          :skipped             (vec (:skipped res))
          :dragged             (vec (:dragged res))
          :unchanged?          (boolean (:unchanged? res))
          :multi-file          (:multi-file res)
          :stale-registrations (:stale-registrations res)
          :interlock           {:no-reload (count (:no-reload interlock))}
          :kept-vars           (:kept pass)
          :forced-unload       (mapv str (:forced pass))
          :orphans             (mapv str (:orphans pass))
          :dead-links          (:dead-links pass)
          :kept-state          (names (:state classes))
          :records-redefined   (names (:record classes))
          :remount             remount
          :tools-refreshed     tools
          :surface             surface
          :hive-hot            hot-init})))))
