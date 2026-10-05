(ns hive-mcp.swarm.lifecycle.restore-liveness
  "Shared boot/cleanup classification of persisted agents against live backing.
   Evidence comes from the boundary; classification is a pure value transform."
  (:require [hive-mcp.tools.agent.helpers :as helpers]
            [hive-system.process.liveness :as liveness]))

(defn missing-from-emacs?
  "Pure: whether a slave id is absent from the Emacs membership cleanup reads."
  [slave-id elisp-ids]
  (not (contains? elisp-ids slave-id)))

(defn classify-row
  "Pure: retire a restored row unless its Emacs id or OS pid has live evidence.
   Missing evidence is not life. Preserve snapshot fields including created-at."
  [row {:keys [elisp-ids live-pids]}]
  (if (or (not (missing-from-emacs? (:slave-id row) elisp-ids))
          (and (:process-pid row) (contains? live-pids (:process-pid row))))
    row
    (assoc row :status :zombie :alive? false)))

(defn probe-evidence
  "Boundary: obtain the same Emacs membership cleanup uses, plus OS pid life.
   Inject the probe function at full-sync's port during restore tests."
  [rows]
  {:elisp-ids (set (map :slave/id (or (helpers/query-elisp-lings) [])))
   :live-pids (into #{} (comp (keep :process-pid)
                              (filter #(= :liveness/alive
                                          (:adt/variant (liveness/check-pid-alive %)))))
                    rows)})
