(ns hive-mcp.multi.registry.script-support
  "Test support shared by the four child-registry facet tests (aliases,
   batchables, tools, verbs). Each registry is a process-global owned index
   that `multi.core-seed` populates at load, so a script must not leave it
   changed: `run-script` snapshots the live index, clears it, runs the
   script, captures the outcome as data, and re-registers every snapshotted
   entry under its own owner through the registry's public `register!`.

   `model` is an independent plain-map model of the owned-registry policy:
   first write wins across owners, same-owner writes replace, an
   `overridable-owner`'s entries yield to any other owner, and
   deregistration removes exactly the owner's ids."
  (:require [clojure.test :refer [is]]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def owners [:addon/a :addon/b "hive.rss" :multi/core])

(def ids ["x" "y" "z"])

(defn op-schema
  "Malli schema of one script step, with ENTRY-SCHEMA for the entry."
  [entry-schema]
  [:or
   [:tuple [:= :register] (into [:enum] owners) (into [:enum] ids) entry-schema]
   [:tuple [:= :deregister] (into [:enum] owners)]])

(defn script-schema [entry-schema]
  [:vector {:max 10} (op-schema entry-schema)])

(def ^:private volatile-keys
  "Clock-stamped keys a registry may add (tools stamps :registered-at)."
  [:registered-at])

(defn- strip [entry] (apply dissoc entry volatile-keys))

(defn run-script
  "Run OPS against registry R, a map of the registry's public fns
   {:register! :deregister-by-owner! :lookup :snapshot :reset-for-test!} and
   its :index key. Answers {:steps [...] :index {...} :by-owner {...}}, with
   clock stamps removed, and leaves the live registry as it found it. The
   registry logs every rejected write; the script silences that below :error."
  [{:keys [register! deregister-by-owner! lookup snapshot reset-for-test! index]} ops]
  (log/with-min-level :error
    (let [saved (:data (snapshot))]
      (try
        (reset-for-test!)
        (let [steps (mapv (fn [[op owner id entry]]
                            (case op
                              :register   (register! owner id entry)
                              :deregister (deregister-by-owner! owner)))
                          ops)
              data  (:data (snapshot))]
          {:steps    steps
           :index    (update-vals (get data index) strip)
           :by-owner (into {} (remove (comp empty? val)) (:by-owner data))
           :lookups  (into {} (map (fn [id] [id (some-> (lookup id) strip)])) ids)})
        (finally
          (reset-for-test!)
          (doseq [[id entry] (get saved index)]
            (register! (:owner entry) id (apply dissoc entry :owner volatile-keys))))))))

(defn model
  "Plain-map model of the owned-registry policy over OPS. OVERRIDABLE is the
   owner whose entries any other owner may replace (nil for none)."
  [ops overridable]
  (let [[steps idx by-owner]
        (reduce
         (fn [[steps idx by-owner] [op owner id entry]]
           (case op
             :register
             (let [existing (get idx id)
                   v        (assoc entry :owner owner)
                   prev     (:owner existing)]
               (cond
                 (nil? existing)
                 [(conj steps :ok) (assoc idx id v) (update by-owner owner (fnil conj #{}) id)]
                 (= owner prev)
                 [(conj steps :replaced) (assoc idx id v) by-owner]
                 (and overridable (= prev overridable))
                 [(conj steps :replaced) (assoc idx id v)
                  (-> by-owner (update prev disj id) (update owner (fnil conj #{}) id))]
                 :else
                 [(conj steps :conflict) idx by-owner]))
             :deregister
             (let [gone (get by-owner owner #{})]
               [(conj steps gone) (apply dissoc idx gone) (dissoc by-owner owner)])))
         [[] {} {}]
         ops)]
    {:steps    steps
     :index    idx
     :by-owner (into {} (remove (comp empty? val)) by-owner)
     :lookups  (into {} (map (fn [id] [id (get idx id)])) ids)}))

(defn restores-the-live-registry
  "Assertion: running a script leaves R's live index unchanged."
  [{:keys [snapshot index] :as r} ops]
  (let [before (update-vals (get (:data (snapshot)) index) strip)]
    (run-script r ops)
    (is (= before (update-vals (get (:data (snapshot)) index) strip))
        "the live registry is restored after a script")))
