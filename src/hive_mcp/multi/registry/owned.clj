(ns hive-mcp.multi.registry.owned
  "Owner-tagged two-index registry operations. Each caller owns its state atom
   and chooses its primary index and conflict-reporting policy."
  (:require [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn register!
  "Register entry under id. Same-owner writes replace; another owner conflicts.
   Returns :ok | :replaced | :conflict. conflict-key is the keyword naming the
   id in the conflict diagnostic; log-tag is the caller's bracketed log prefix.
   Optional policy accepts :stamp-fn (entry -> entry) and :overridable-owner,
   whose entries may be replaced by another owner. Stamping happens once per
   registration attempt, including conflicts, to preserve caller timing."
  ([state index-key owner id entry log-tag conflict-key]
   (register! state index-key owner id entry log-tag conflict-key nil))
  ([state index-key owner id entry log-tag conflict-key
    {:keys [stamp-fn overridable-owner]}]
   (let [v (assoc (if stamp-fn (stamp-fn entry) entry) :owner owner)]
     (loop []
       (let [before @state
             existing (get-in before [index-key id])
             previous-owner (:owner existing)
             outcome (cond (nil? existing) :ok
                           (= owner previous-owner) :replaced
                           (and overridable-owner (= overridable-owner previous-owner)) :replaced
                           :else :conflict)
             after (case outcome
                     :ok (-> before
                             (assoc-in [index-key id] v)
                             (update-in [:by-owner owner] (fnil conj #{}) id))
                     :replaced (if (= owner previous-owner)
                                 (assoc-in before [index-key id] v)
                                 (-> before
                                     (assoc-in [index-key id] v)
                                     (update-in [:by-owner previous-owner] disj id)
                                     (update-in [:by-owner owner] (fnil conj #{}) id)))
                     before)]
         (if (compare-and-set! state before after)
           (do (when (= outcome :conflict)
                 (log/warn (str log-tag " :multi/registry-conflict")
                           {conflict-key id
                            :existing-owner previous-owner
                            :rejected-owner owner}))
               outcome)
           (recur)))))))

(defn deregister-by-owner!
  "Remove only ids owned by owner; return the removed set."
  [state index-key owner]
  (loop []
    (let [before @state
          ids (get-in before [:by-owner owner] #{})
          after (-> before
                    (update index-key #(apply dissoc % ids))
                    (update :by-owner dissoc owner))]
      (if (compare-and-set! state before after)
        ids
        (recur)))))

(defn lookup [state index-key id]
  (get-in @state [index-key id]))

(defn all [state index-key]
  (get @state index-key))
