(ns hive-mcp.addons.boot-health
  "What the addon boot DID, compared with what it was EXPECTED to do.

   A boot whose classpath carries only one addon manifest still serves tools:
   the command sets are smaller, the memory store is missing, and every error
   reads like a missing capability. Nothing says the boot was degraded. This
   namespace keeps the evidence and says it:

     - `record-roster!`  the loader's facts: discovered, mounted, failed
     - `record-memory!`  the memory backend's facts: which addon a backend
                         deferred to, and whether a store was registered
     - `snapshot`        the facts, the expectations, the issues, :degraded?
     - `notice`          one line for a tool response, nil when healthy

   Expectations come from three places, none of them compulsory:
     - config `:services :addons {:expected-min n :expected #{\"hive.carto\"}}`
     - the memory backend: deferring to an addon is a promise it will mount
     - the last HEALTHY boot's roster, persisted as a baseline. A boot that
       discovers fewer than half of it is degraded. A degraded boot never
       lowers the baseline, so the second bad boot is caught as well as the
       first; delete the file to accept a smaller roster on purpose.

   Kept dependency-light (no config, no registry requires) because
   hive-mcp.tools.cli consults it on every help and unknown-command answer.
   Rationale and incident: hive memory 20260901002309-05854dd7."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state (atom nil))

(def baseline-drop-ratio
  "A boot discovering fewer than this fraction of the last healthy boot's
   manifests is degraded."
  1/2)

(defn default-baseline-path []
  (str (System/getProperty "user.home")
       "/.config/hive-mcp/data/addon-roster/last-healthy.edn"))

;; -----------------------------------------------------------------------------
;; Baseline persistence
;; -----------------------------------------------------------------------------

(defn read-baseline
  "The last healthy boot's roster {:discovered [...] :mounted [...] :at ...},
   or nil when absent or unreadable."
  [path]
  (try
    (let [f (io/file path)]
      (when (.exists f)
        (let [m (edn/read-string (slurp f))]
          (when (map? m) m))))
    (catch Throwable t
      (log/debug "addon roster baseline unreadable" {:path path :error (ex-message t)})
      nil)))

(defn write-baseline!
  [path snap]
  (try
    (let [f (io/file path)]
      (io/make-parents f)
      (spit f (pr-str {:discovered (vec (sort (:discovered snap)))
                       :mounted    (vec (sort (:mounted snap)))
                       :at         (str (java.time.Instant/now))})))
    (catch Throwable t
      (log/debug "addon roster baseline not written" {:path path :error (ex-message t)}))))

;; -----------------------------------------------------------------------------
;; Assessment (pure)
;; -----------------------------------------------------------------------------

(defn assess
  "Issues for FACTS, a map of
     :discovered   [id ...]   manifests found on the classpath
     :mounted      [id ...]   addons that mounted (or sit dormant behind stubs)
     :failed       [id ...]   discovered but not mounted
     :expected-min n          optional floor on discovered manifests
     :expected     #{id ...}  optional ids that must mount
     :baseline     {:discovered [...]} optional last healthy roster
     :memory       {:backend s :deferred-to id :store-set? bool}
   Returns a vector of {:issue kw :severity :error|:warn :message s ...}."
  [{:keys [discovered mounted failed expected-min expected baseline memory]}]
  (let [n-disc   (count discovered)
        mounted* (set mounted)
        missing  (sort (set/difference (set expected) mounted*))
        base-n   (count (:discovered baseline))
        lost     (sort (set/difference (set (:discovered baseline)) (set discovered)))
        {:keys [backend deferred-to store-set?]} memory]
    (cond-> []
      (and (some? deferred-to) (false? store-set?))
      (conj {:issue    :memory-store-missing
             :severity :error
             :backend  backend
             :addon    deferred-to
             :message  (str "memory backend '" backend "' was deferred to addon " deferred-to
                            (if (contains? (set discovered) deferred-to)
                              " which was discovered but registered no store"
                              " which is NOT on the classpath")
                            "; memory, kanban and catchup will answer 'Memory store not configured'")})

      (and expected-min (< n-disc expected-min))
      (conj {:issue    :below-floor
             :severity :error
             :expected expected-min
             :found    n-disc
             :message  (str "discovered " n-disc " addon manifest(s), configured floor is "
                            expected-min " (:services :addons :expected-min)")})

      (seq missing)
      (conj {:issue    :expected-missing
             :severity :error
             :missing  (vec missing)
             :message  (str "expected addon(s) not mounted: " (str/join ", " missing))})

      (and (pos? base-n) (< n-disc (* baseline-drop-ratio base-n)))
      (conj {:issue    :baseline-drop
             :severity :error
             :expected base-n
             :found    n-disc
             :lost     (vec lost)
             :message  (str "discovered " n-disc " addon manifest(s); the last healthy boot"
                            (when-let [at (:at baseline)] (str " (" at ")"))
                            " discovered " base-n ". Missing: "
                            (str/join ", " (take 12 lost))
                            (when (> (count lost) 12) (str " … +" (- (count lost) 12) " more")))})

      (seq failed)
      (conj {:issue    :mount-failed
             :severity :warn
             :failed   (vec (sort failed))
             :message  (str "discovered but not mounted: " (str/join ", " (sort failed)))}))))

(defn degraded?
  "True when any issue is an :error."
  [issues]
  (boolean (some #(= :error (:severity %)) issues)))

(defn- expected-count
  "The number of addons this boot SHOULD have had, the largest of the
   expectations present; nil when nothing sets one."
  [{:keys [expected-min expected baseline]}]
  (let [cands (cond-> []
                expected-min               (conj expected-min)
                (seq expected)             (conj (count expected))
                (seq (:discovered baseline)) (conj (count (:discovered baseline))))]
    (when (seq cands) (apply max cands))))

(defn- build-snapshot [facts]
  (let [issues (assess facts)]
    (assoc facts
           :issues issues
           :degraded? (degraded? issues)
           :expected-count (expected-count facts))))

;; -----------------------------------------------------------------------------
;; Recording
;; -----------------------------------------------------------------------------

(defn- log-issues! [snap]
  (doseq [{:keys [severity message] :as i} (:issues snap)]
    (let [data (dissoc i :message :severity)]
      (if (= :error severity)
        (log/error "DEGRADED ADDON BOOT:" message data)
        (log/warn "Addon boot:" message data))))
  (when (:degraded? snap)
    (log/error "DEGRADED ADDON BOOT: this is a launch-classpath problem, not a crash."
               "Every tool keeps answering with a reduced command set."
               "Agents are told so in `help`, unknown-command errors and `addon doctor`."
               {:discovered (count (:discovered snap))
                :mounted    (count (:mounted snap))
                :expected   (:expected-count snap)})))

(defn record-roster!
  "Record the loader's roster FACTS (see `assess`), judge it, log each issue
   at WARN/ERROR, and — when the boot is healthy — persist it as the baseline
   at BASELINE-PATH (nil skips persistence). Returns the snapshot."
  [facts baseline-path]
  (let [baseline (when baseline-path (read-baseline baseline-path))
        prior    (select-keys @state [:memory])
        snap     (build-snapshot (merge prior facts
                                        {:baseline baseline
                                         :baseline-path baseline-path
                                         :recorded-at (str (java.time.Instant/now))}))]
    (reset! state snap)
    (log-issues! snap)
    (when (and baseline-path (not (:degraded? snap)) (seq (:discovered snap)))
      (write-baseline! baseline-path snap))
    snap))

(defn record-memory!
  "Record the memory backend's facts {:backend :deferred-to :store-set?} after
   extensions loaded, re-judge, and log what the memory facts added."
  [memory]
  (let [before (set (map :issue (:issues @state)))
        snap   (swap! state (fn [s] (build-snapshot (assoc (or s {}) :memory memory))))
        fresh  (remove #(contains? before (:issue %)) (:issues snap))]
    (log-issues! (assoc snap :issues (vec fresh)
                        :degraded? (and (:degraded? snap) (degraded? fresh))))
    snap))

(defn snapshot
  "The recorded boot health, or nil before the loader ran."
  []
  @state)

(defn reset-state!
  "Test seam."
  []
  (reset! state nil))

;; -----------------------------------------------------------------------------
;; Rendering
;; -----------------------------------------------------------------------------

(defn summary-line
  "\"expected N addons, mounted M (discovered D)\" for SNAP."
  [snap]
  (str "expected " (or (:expected-count snap) "?") " addons, mounted "
       (count (:mounted snap)) " (discovered " (count (:discovered snap)) ")"))

(defn notice
  "One paragraph for a tool response when the boot is degraded; nil otherwise."
  ([] (notice (snapshot)))
  ([snap]
   (when (:degraded? snap)
     (str "⚠ DEGRADED ADDON ROSTER — this server booted with a reduced classpath: "
          (summary-line snap) ". "
          (str/join "; " (keep #(when (= :error (:severity %)) (:message %)) (:issues snap)))
          ". Commands and stores those addons contribute are ABSENT, not unsupported on this machine."
          " Ask `addon doctor` (no addon_id) for the readiness report."
          " The fix is the launch classpath; do not restart a shared coordinator you do not own."))))

(defn with-notice
  "TEXT with the degraded-roster notice appended when there is one."
  [text]
  (if-let [n (notice)]
    (str text "\n\n" n)
    text))

(defn readiness-report
  "Data answer for a readiness probe."
  ([] (readiness-report (snapshot)))
  ([snap]
   (if (nil? snap)
     {:ready? false
      :summary "addon boot has not recorded a roster yet (still booting, or loader never ran)"}
     {:ready?     (not (:degraded? snap))
      :summary    (summary-line snap)
      :expected   (:expected-count snap)
      :discovered (vec (sort (:discovered snap)))
      :mounted    (vec (sort (:mounted snap)))
      :failed     (vec (sort (:failed snap)))
      :memory     (:memory snap)
      :issues     (mapv #(select-keys % [:issue :severity :message]) (:issues snap))
      :baseline   (when-let [b (:baseline snap)]
                    {:path (:baseline-path snap) :at (:at b) :discovered (count (:discovered b))})
      :recorded-at (:recorded-at snap)
      :notice     (notice snap)})))
