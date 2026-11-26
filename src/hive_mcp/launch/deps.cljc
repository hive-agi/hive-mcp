(ns hive-mcp.launch.deps
  "The `:launch` section as ONE -Sdeps overlay, plus the local.deps.edn importer.

   Contract:

   (overlay launch-cfg preset-maps) -> {:deps .. :aliases .. :extra-paths ..}
     The single map the launcher hands to `clojure -Sdeps`. Presets merge in
     their given order first and `:launch :deps` merges last, so the person's
     own coordinate wins on the same lib. Aliases merge per alias key under the
     same rule. A relative `:local/root` resolves against `:launch :workspace`
     by plain string join when that key is a non-blank string; an absolute one,
     or any one when :workspace is absent, is kept exactly as written. A
     section that merges to nothing is omitted.

   (import-local-deps m) -> {:deps .. :aliases .. :extra-paths ..}
     The `:launch` fragment for a parsed local.deps.edn, and nothing else: no
     other top-level key survives. Round-trip law, for every m whose present
     sections are non-empty:

       (overlay (import-local-deps m) []) ==
       (select-keys m [:deps :aliases :extra-paths])

     An explicitly empty section is the one exception, and it is the
     documented normalization: an empty :deps is absent from both sides of the
     round trip, because the importer and the overlay both drop it.

   (load-preset k) -> the EDN map at the classpath resource
     `hive_mcp/launch/presets/<name>.edn`, or nil when the resource is absent.
     io/resource is the only lookup, so it answers the same under babashka and
     the JVM.

   Every function here is pure except load-preset, which reads one resource."
  (:require [clojure.string :as str]
            [malli.core :as m]
            ;; cljs has neither clojure.edn nor a classpath resource stream.
            ;; babashka's feature set carries :clj, so :default serves it and
            ;; the JVM alike.
            #?@(:cljs []
                :default [[clojure.edn :as edn]
                          [clojure.java.io :as io]])))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Paths: absolute, and a plain string join
;; =============================================================================

;; Both path helpers below are PRIVATE and TEMPORARY: hive-system.path owns
;; path arithmetic (plan SYSOPS-PATH, step SP3 replaces them). Nothing outside
;; this namespace may depend on them.
(defn- absolute-path?
  "True when `p` is a POSIX absolute path: a string whose first character is
   a slash. A tilde prefix is NOT absolute here, because expanding it is the
   shell's act, not the launcher's."
  [p]
  (and (string? p)
       (str/starts-with? p "/")))

(defn- join-path
  "`p` appended to `base` with exactly one separator between them.

   A plain string join: no canonicalization, so `..` and `.` segments survive
   into the overlay exactly as the caller wrote them. A nil or empty `base`
   answers `p`."
  [base p]
  (cond
    (not (string? base)) p
    (empty? base) p
    (str/ends-with? base "/") (str base p)
    :else (str base "/" p)))

(defn resolve-local-root
  "COORD with its relative `:local/root` resolved against WORKSPACE.

   A coordinate without :local/root, a non-string root, an absolute root, a
   tilde root, and any root when WORKSPACE is not a non-blank string all come
   back unchanged."
  [workspace coord]
  (let [root (:local/root coord)]
    (if (and (map? coord)
             (string? root)
             (string? workspace)
             (not (str/blank? workspace))
             (not (absolute-path? root)))
      (assoc coord :local/root (join-path workspace root))
      coord)))

(defn resolve-deps
  "DEPS with every coordinate's relative :local/root resolved against
   WORKSPACE. Libs and coordinates it does not touch are preserved."
  [workspace deps]
  (into {}
        (map (fn [[lib coord]]
               [lib (if (map? coord) (resolve-local-root workspace coord) coord)]))
        deps))

;; =============================================================================
;; The overlay
;; =============================================================================

(defn- merge-section
  "Reduce the map section `k` over PRESETS in order, then over CFG; a later map
   wins per key.

   CFG is consulted even when it carries no such section, so a caller does not
   have to build a placeholder map per section. A non-map section is skipped."
  [k presets cfg]
  (reduce (fn [acc m]
            (let [v (get m k)]
              (if (map? v) (merge acc v) acc)))
          {}
          (concat presets [cfg])))

(defn overlay
  "The ONE -Sdeps map for LAUNCH-CFG over PRESET-MAPS.

   PRESET-MAPS is a sequential collection of already-loaded preset maps (the
   shape `load-preset` answers), in the order they are to be applied. LAUNCH-CFG
   is a `:launch` section: its :workspace, :deps, :aliases and :extra-paths are
   the only keys read here; every other key belongs to another part of the
   launcher. Answers only the non-empty sections."
  [launch-cfg preset-maps]
  (let [cfg (if (map? launch-cfg) launch-cfg {})
        presets (remove nil? (or preset-maps []))
        workspace (:workspace cfg)
        deps (merge-section :deps presets cfg)
        aliases (merge-section :aliases presets cfg)
        extra-paths (->> (concat (mapcat (fn [p] (or (:extra-paths p) [])) presets)
                                 (or (:extra-paths cfg) []))
                         (remove nil?)
                         vec)]
    (cond-> {}
      (seq deps) (assoc :deps (resolve-deps workspace deps))
      (seq aliases) (assoc :aliases aliases)
      (seq extra-paths) (assoc :extra-paths extra-paths))))

;; =============================================================================
;; The importer
;; =============================================================================

(defn import-local-deps
  "The `:launch` fragment for the parsed local.deps.edn map M.

   Only :deps, :aliases and :extra-paths cross over; every other top-level key
   (and any key that file might grow later) is left behind. A section that is
   absent or empty is omitted, so the fragment always validates against
   LaunchConfig. Total: a non-map answers {}."
  [m]
  (let [m (if (map? m) m {})
        deps (:deps m)
        aliases (:aliases m)
        extra (:extra-paths m)]
    (cond-> {}
      (and (map? deps) (seq deps)) (assoc :deps deps)
      (and (map? aliases) (seq aliases)) (assoc :aliases aliases)
      (and (sequential? extra) (seq extra)) (assoc :extra-paths (vec extra)))))

;; =============================================================================
;; Presets on the classpath
;; =============================================================================

(def preset-root
  "Directory under the classpath root holding one EDN file per preset."
  "hive_mcp/launch/presets/")

(defn preset-resource
  "The classpath resource name of preset K (:starter or \"starter\")."
  [k]
  (str preset-root (name k) ".edn"))

(defn load-preset
  "The map of preset K, read from its classpath resource, or nil when the
   resource is absent.

   Read with clojure.edn and unknown tags answered as tagged literals, so a
   preset carrying a tag this image does not know still loads instead of
   throwing."
  [k]
  ;; cljs has no classpath EDN stream, so no preset is ever available there and
  ;; nil is the honest answer; the JVM and babashka read the resource.
  #?@(:cljs [(do (preset-resource k) nil)]
      :default
      [(when-let [r (io/resource (preset-resource k))]
         (edn/read-string {:default tagged-literal} (slurp r)))]))

(defn load-presets
  "The maps of KS, in order, each through `load-preset`; an absent one is
   dropped rather than yielding a nil the overlay has to guard."
  [ks]
  (into [] (keep load-preset) ks))

;; =============================================================================
;; Contracts
;; =============================================================================

(m/=> resolve-local-root [:=> [:cat [:maybe :string] :map] :map])
(m/=> resolve-deps [:=> [:cat [:maybe :string] [:map-of :symbol :any]] [:map-of :symbol :any]])
(m/=> overlay [:=> [:cat [:maybe :map] [:sequential [:maybe :map]]] [:map]])
(m/=> import-local-deps [:=> [:cat :any] [:map]])
(m/=> preset-resource [:=> [:cat [:or :keyword :string]] :string])
(m/=> load-preset [:=> [:cat [:or :keyword :string]] [:maybe :map]])
(m/=> load-presets [:=> [:cat [:sequential [:or :keyword :string]]] [:vector :map]])

(comment
  ;; A relative root resolves against the workspace, by plain join:
  (resolve-local-root "/ws" {:local/root "../hive-carto"})
  ;=> {:local/root "/ws/../hive-carto"}

  ;; An absolute one does not:
  (resolve-local-root "/ws" {:local/root "/opt/hive-carto"})
  ;=> {:local/root "/opt/hive-carto"}

  ;; No workspace: kept as written.
  (resolve-local-root nil {:local/root "../hive-carto"})
  ;=> {:local/root "../hive-carto"}

  ;; The user's coordinate wins over the preset's:
  (overlay {:deps {'a/b {:mvn/version "2"}}}
           [{:deps {'a/b {:mvn/version "1"} 'a/c {:mvn/version "1"}}}])
  ;=> {:deps {a/b {:mvn/version "2"}, a/c {:mvn/version "1"}}}

  ;; Empty sections never appear:
  (overlay {} [{}]) ;=> {}

  ;; The round-trip law, on a two-lib file:
  (let [m {:deps {'x/y {:local/root "../y"} 'z/w {:mvn/version "1"}}
           :aliases {:heap {:jvm-opts ["-Xmx4g"]}}
           :extra-paths ["test"]
           :paths ["src"]}]
    (= (overlay (import-local-deps m) [])
       (select-keys m [:deps :aliases :extra-paths])))
  ;=> true, and :paths never reaches the overlay
  )