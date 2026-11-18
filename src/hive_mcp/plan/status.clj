(ns hive-mcp.plan.status
  "Plan lifecycle state carried in TAGS, plus the readers that prefer real
   fields when a backend has them.

   Why tags: the memory port's entry shape is open, but the Milvus store maps
   entries onto a FIXED column set and silently drops unknown keys. A plan
   status written as `:plan-status` therefore survived in the plan index and
   vanished from the entry itself. Tags are the one field every backend
   persists verbatim, so the authoritative record uses namespaced tags:

     plan-status:<status>       exactly one
     plan-task:<kanban task id> one per converted card
     plan-wave:<step-id>=<n>    one per step

   The `:plan-status` field is still the first read: Chroma and test doubles
   can carry it, and older entries may. Tags are the fallback, and the only
   form `update-entry!` is guaranteed to round-trip everywhere."
  (:require [clojure.string :as str]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def status-prefix "plan-status:")
(def task-prefix "plan-task:")
(def wave-prefix "plan-wave:")

(defn status-tag
  "The tag naming STATUS."
  [status]
  (str status-prefix status))

(defn status-tag?
  [tag]
  (and (string? tag) (str/starts-with? tag status-prefix)))

(defn task-tag?
  [tag]
  (and (string? tag) (str/starts-with? tag task-prefix)))

(defn wave-tag?
  [tag]
  (and (string? tag) (str/starts-with? tag wave-prefix)))

(defn status-of
  "The plan status of ENTRY: the field when present, else its status tag, else
   `draft` (an entry written before the lifecycle existed)."
  [entry]
  (or (:plan-status entry)
      (if-let [tag (some #(when (status-tag? %) %) (:tags entry))]
        (subs tag (count status-prefix))
        "draft")))

(defn task-ids-of
  "Kanban task ids recorded on ENTRY, from the field or its tags."
  [entry]
  (or (not-empty (:task-ids entry))
      (into [] (keep #(when (task-tag? %) (subs % (count task-prefix))))
            (:tags entry))))

(defn waves-of
  "Step-id -> wave-number recorded on ENTRY, from the field or its tags."
  [entry]
  (or (not-empty (:waves entry))
      (into {}
            (keep (fn [tag]
                    (when (wave-tag? tag)
                      (let [[step n] (str/split (subs tag (count wave-prefix)) #"=" 2)]
                        (when (and step n) [step (parse-long n)])))))
            (:tags entry))))

(defn- strip [pred tags]
  (into [] (remove pred) tags))

(defn with-status
  "TAGS with exactly one status tag, set to STATUS."
  [tags status]
  (conj (strip status-tag? tags) (status-tag status)))

(defn with-conversion
  "TAGS carrying the conversion: one task tag per id, one wave tag per step,
   and the status UNCHANGED (the caller sets `active` separately)."
  [tags task-ids waves]
  (-> (strip #(or (task-tag? %) (wave-tag? %)) tags)
      (into (map #(str task-prefix %) task-ids))
      (into (map (fn [[step n]] (str wave-prefix step "=" n)) waves))))

(m/=> status-tag [:=> [:cat :string] :string])
(m/=> status-tag? [:=> [:cat :any] :boolean])
(m/=> task-tag? [:=> [:cat :any] :boolean])
(m/=> wave-tag? [:=> [:cat :any] :boolean])
(m/=> status-of [:=> [:cat :map] :string])
(m/=> task-ids-of [:=> [:cat :map] [:vector :string]])
(m/=> waves-of [:=> [:cat :map] [:map-of :string :int]])
(m/=> with-status [:=> [:cat [:vector :string] :string] [:vector :string]])
(m/=> with-conversion [:=> [:cat [:vector :string] [:vector :string] [:map-of :string :int]]
                           [:vector :string]])
