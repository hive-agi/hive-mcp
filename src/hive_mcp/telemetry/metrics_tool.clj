(ns hive-mcp.telemetry.metrics-tool
  "MCP verbs `project metrics snapshot|hotspots|resources|reset`, contributed through
   hive-addon.registry.commands. Every verb reads the metrics port late via
   hive-mcp.telemetry.metrics; with the port absent they answer empty data."
  (:require [hive-addon.registry.commands :as addon-cmds]
            [hive-mcp.telemetry.metrics :as metrics]
            [hive-mcp.telemetry.resources :as resources]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def target-tool
  "The consolidated tool the verbs are contributed to."
  "project")

(def owner
  "Contribution owner id."
  "hive-mcp.metrics")

(def ^:private by-values #{"total" "count" "mean" "p99" "max" "errors"})

(defn- param [params k]
  (or (get params k) (get params (name k))))

(defn hotspots-opts
  "Port hotspots opts from MCP params by (one of total count mean p99 max
   errors, default total), limit (default 20) and area (e.g. \"mcp/tool\")."
  [params]
  (let [by    (some-> (param params :by) name)
        limit (param params :limit)
        limit (cond (integer? limit) limit
                    (string? limit) (parse-long limit)
                    :else nil)
        area  (some-> (param params :area) str not-empty)]
    (cond-> {:by (keyword (if (contains? by-values by) by "total"))
             :limit (or limit 20)}
      area (assoc :area area))))

(m/=> hotspots-opts [:=> [:cat [:maybe :map]]
                     [:map [:by :keyword] [:limit [:maybe :int]] [:area {:optional true} :string]]])

(defn- text [v] {:type "text" :text (pr-str v)})

(defn handle-snapshot
  "Answer the port snapshot, rows sorted by key."
  [_params]
  (text {:port? (metrics/port-available?)
         :snapshot (into (sorted-map-by #(compare (pr-str %1) (pr-str %2))) (metrics/snapshot))}))

(defn handle-hotspots
  "Answer the ranked hotspot rows for params by, limit, area."
  [params]
  (let [opts (hotspots-opts params)]
    (text {:port? (metrics/port-available?) :opts opts :rows (metrics/hotspots opts)})))

(defn handle-resources
  "Answer the JVM resource report: sampler status, trend (Metaspace and class
   growth per hour, RSS slope), the latest sample and the gauge names."
  [_params]
  (text (resources/report)))

(defn handle-reset
  "Clear the port aggregates."
  [_params]
  (metrics/reset!)
  (text {:reset true :port? (metrics/port-available?)}))

(def contribution
  "The `metrics` command group."
  {"metrics" {:handler {:snapshot #'handle-snapshot
                        :hotspots #'handle-hotspots
                        :resources #'handle-resources
                        :reset    #'handle-reset}
              :description (str "Internal op timing: metrics snapshot (every op key), "
                                "metrics hotspots [by=total|count|mean|p99|max|errors] [limit] "
                                "[area e.g. mcp/tool, mcp/mw], metrics resources (JVM CPU/heap/Metaspace/"
                                "classes/threads/GC/RSS trend), metrics reset.")}})

(defn install!
  "Contribute the `metrics` group to `target-tool`. Never throws; returns the
   contributed names or nil."
  []
  (try (addon-cmds/contribute! target-tool owner contribution)
       (catch Throwable _ nil)))

(def ^:private TextContent
  [:map [:type [:= "text"]] [:text :string]])

(m/=> handle-snapshot [:=> [:cat [:maybe :map]] TextContent])
(m/=> handle-hotspots [:=> [:cat [:maybe :map]] TextContent])
(m/=> handle-resources [:=> [:cat [:maybe :map]] TextContent])
(m/=> handle-reset [:=> [:cat [:maybe :map]] TextContent])
(m/=> install! [:=> [:cat] :any])
