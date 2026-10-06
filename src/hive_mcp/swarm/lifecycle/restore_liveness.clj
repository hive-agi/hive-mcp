(ns hive-mcp.swarm.lifecycle.restore-liveness
  "Shared boot/cleanup classification of persisted agents against live backing.
   Evidence comes from the boundary; classification is a pure value transform."
  (:require [hive-mcp.tools.agent.helpers :as helpers]
            [hive-system.process.liveness :as liveness]
            [hive-mcp.agent.ling.terminal-registry :as terminals]
            [hive-mcp.agent.ling.headless-registry :as headless-registry]
            [hive-spi.addon.headless :as headless]))

(defn missing-from-emacs?
  "Pure: whether a slave id is absent from the Emacs membership cleanup reads."
  [slave-id elisp-ids]
  (not (contains? elisp-ids slave-id)))

(defn classify-row
  "Pure: positive evidence preserves a row. When Emacs is unknown, registered
   terminal-backed rows are unverified; headless rows still need positive proof."
  [row {:keys [emacs live-pids live-ids terminal-modes]}]
  (let [id (:slave-id row)]
    (cond
      (or (contains? live-ids id)
          (and (:process-pid row) (contains? live-pids (:process-pid row)))
          (contains? (:ids emacs) id)) (assoc row :liveness :verified)
      (and (= :unknown (:state emacs))
           (contains? terminal-modes (:spawn-mode row)))
      (assoc row :liveness :unverified)
      :else (assoc row :status :zombie :alive? false))))

(defn probe-evidence
  "Boundary: query Emacs, OS pids and the registered headless backend's live
   session status. A missing or failed query never certifies liveness."
  [rows]
  (let [elisp (helpers/query-elisp-lings)]
    {:emacs (if (nil? elisp)
              {:state :unknown}
              {:state :known :ids (set (map :slave/id elisp))})
     :terminal-modes (terminals/registered-terminals)
     :live-ids (into #{}
                     (keep (fn [{:keys [slave-id spawn-mode]}]
                             (when-let [backend (headless-registry/get-headless-backend spawn-mode)]
                               (when (try
                                       (contains? #{:running :idle}
                                                  (:slave/status (headless/headless-status backend {:id slave-id} nil)))
                                       (catch Exception _ false))
                                 slave-id))))
                     rows)
     :live-pids (into #{} (comp (keep :process-pid)
                                (filter #(= :liveness/alive
                                            (:adt/variant (liveness/check-pid-alive %)))))
                      rows)}))
