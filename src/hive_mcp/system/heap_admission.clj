(ns hive-mcp.system.heap-admission
  "Heap admission: should this JVM take on more work? The spawn tool asks it
   before launching a ling (Layer 4 of the spawn guard); lings launch inside
   or alongside this JVM, and N concurrent heavy spawns atop the multi-GB KG
   floor have driven kernel OOMs.

   It reads hive-cache.mem-guard, lazily: hive-mcp does NOT statically depend
   on hive-cache. The level is decided on the old generation's occupancy after
   its last collection (`check-post-gc`), not on Runtime total-free, which
   counts uncollected garbage and swung 80% -> 95% between two calls a minute
   apart on the same live set (friction 20261111145728-573b7979). Only the
   HARD watermark refuses; soft pressure admits with a log warning. Nothing
   is queued: the caller decides what a refusal means for its request.

   `hive-mcp.system.sweepers.heap-pressure` is the other half of the heap
   story: it returns committed-but-empty heap to the OS."
  (:require [hive-mcp.config.core :as config]
            [malli.core :as m]
            [taoensso.timbre :as log]
            [hive-mcp.extensions.soft :as ext-soft]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const retry-after-s
  "Seconds a refused caller should wait before asking again. The post-GC
   reading moves only when the old generation is collected again (a mixed or
   full collection), so asking sooner reads the same figure."
  60)

(def Watermarks
  "Heap fractions, as hive-cache.mem-guard takes them. Open map: mem-guard's
   defaults also carry :gc-overhead-frac."
  [:map
   [:soft {:optional true} [:double {:min 0.0 :max 1.0}]]
   [:hard {:optional true} [:double {:min 0.0 :max 1.0}]]])

(def Reading
  "A mem-guard reading. :heap-pct is the figure the level was decided on;
   :basis says which one it is. :watermarks rides on what `hard-pressure`
   returns, so a caller can name the watermark it crossed."
  [:map
   [:level [:enum :ok :soft :hard]]
   [:basis {:optional true} [:enum :post-gc :instant]]
   [:heap-pct {:optional true} [:int {:min 0 :max 100}]]
   [:now-pct {:optional true} [:int {:min 0 :max 100}]]
   [:post-gc-pct {:optional true} [:maybe [:int {:min 0 :max 100}]]]
   [:pool {:optional true} [:maybe :string]]
   [:watermarks {:optional true} Watermarks]])

(defn- resolve-mem-guard
  "The var `hive-cache.mem-guard/<sym>`, or nil when hive-cache (or that var,
   on an older hive-cache) is not on the classpath."
  [sym]
  (ext-soft/resolve-soft (symbol "hive-cache.mem-guard" (name sym))))

(defn- disabled?
  "Kill switch. hive-mcp.config.resolve/get-service-value uses (or val default),
   which swallows boolean false, so this is a default-FALSE *disable* flag:
   [:swarm :heap-admission-disabled] true (or env
   HIVE_MCP_SWARM_HEAP_ADMISSION_DISABLED=true) forces the gate off."
  []
  (boolean (config/get-service-value :swarm :heap-admission-disabled
                                     :env "HIVE_MCP_SWARM_HEAP_ADMISSION_DISABLED"
                                     :parse #(Boolean/parseBoolean %)
                                     :default false)))

(defn- watermarks
  "mem-guard's default watermarks (soft 0.80, hard 0.92) overlaid with
   [:swarm :heap-admission-soft] and [:swarm :heap-admission-hard], each a
   0.0-1.0 heap fraction (env HIVE_MCP_SWARM_HEAP_ADMISSION_SOFT / _HARD).
   Only :hard refuses; :soft is the warn level."
  []
  (let [soft (config/get-service-value :swarm :heap-admission-soft
                                       :env "HIVE_MCP_SWARM_HEAP_ADMISSION_SOFT"
                                       :parse parse-double
                                       :default nil)
        hard (config/get-service-value :swarm :heap-admission-hard
                                       :env "HIVE_MCP_SWARM_HEAP_ADMISSION_HARD"
                                       :parse parse-double
                                       :default nil)]
    (cond-> (or (some-> (resolve-mem-guard 'default-watermarks) deref) {})
      (number? soft) (assoc :soft (double soft))
      (number? hard) (assoc :hard (double hard)))))

(defn heap-probe
  "IO. The mem-guard reading for `wm`: `check-post-gc` (decided on post-GC
   old-gen occupancy) when the hive-cache on the classpath has it, else
   `check` (Runtime total-free), flagged :basis :instant. nil when no
   hive-cache is on the classpath."
  [wm]
  (if-let [post-gc (resolve-mem-guard 'check-post-gc)]
    (post-gc wm)
    (when-let [check (resolve-mem-guard 'check)]
      (assoc (check nil wm) :basis :instant))))

(m/=> heap-probe [:=> [:cat Watermarks] [:maybe Reading]])

(defn hard-pressure
  "The reading, with the :watermarks it was taken against, when the heap is
   at or above the HARD watermark; nil otherwise (admit). A :soft reading
   admits and logs a warning.

   `probe` is (fn [watermarks] reading), `heap-probe` in production. The
   0-arity asks `heap-probe`; the 1-arity asks `probe` against the
   configured watermarks and honours the kill switch, so a caller can stand
   a stub in for the reading and keep the decision; the 2-arity takes both
   and reads no config. FAIL-OPEN: no governor, a probe or config read that
   throws, or the kill switch all answer nil, so the gate can never wedge its
   caller."
  ([] (hard-pressure heap-probe))
  ([probe]
   (try
     (when-not (disabled?)
       (hard-pressure probe (watermarks)))
     (catch Throwable t
       (log/debug "heap admission config read failed; admitting:" (ex-message t))
       nil)))
  ([probe wm]
   (try
     (let [reading (probe wm)]
       (case (:level reading)
         :hard (assoc reading :watermarks wm)
         :soft (do (log/warn "Admitted under soft heap pressure"
                             (dissoc reading :snapshot))
                   nil)
         nil))
     (catch Throwable t
       (log/debug "heap admission probe failed; admitting:" (ex-message t))
       nil))))

(m/=> hard-pressure [:function
                     [:=> [:cat] [:maybe Reading]]
                     [:=> [:cat ifn?] [:maybe Reading]]
                     [:=> [:cat ifn? Watermarks] [:maybe Reading]]])
