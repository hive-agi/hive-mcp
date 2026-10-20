(ns hive-mcp.extensions.departures
  "Ledger of contributed commands that have left the contribution store.

   A retraction records, per tool, every command it removed with the addon that
   owned it and when it left: {tool-name {command {:addon id :at-ms t}}}. A
   later contribution of the same command leaves the entry in place; the ledger
   is read only when dispatch misses a root, so a stale entry for a command
   that came back is never consulted.

   What it answers: whether a root the tree does not hold left it (an addon
   remount or unload) or never existed."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private ledger (atom {}))

(defn- command-key
  "COMMAND as the lower-cased string the ledger is keyed by."
  [command]
  (str/lower-case (if (keyword? command) (name command) (str command))))

(defn record!
  "Record that ADDON-ID's COMMANDS left TOOL-NAME at AT-MS (default: now).
   Replaces any earlier entry for the same command. Returns nil."
  ([tool-name addon-id commands]
   (record! tool-name addon-id commands (System/currentTimeMillis)))
  ([tool-name addon-id commands at-ms]
   (when (seq commands)
     (swap! ledger update tool-name merge
            (into {} (map (fn [c] [(command-key c) {:addon addon-id :at-ms at-ms}]))
                  commands)))
   nil))

(defn record-owned!
  "Record as departed, in every tool of CONTRIBUTIONS ({tool {command spec}}),
   the commands whose spec :addon is ADDON-ID. Call it BEFORE the retraction,
   so a dispatch that misses the root always finds its departure. Returns the
   tool names that held such a command."
  [contributions addon-id]
  (let [now (System/currentTimeMillis)]
    (into []
          (keep (fn [[tool cmds]]
                  (when-let [owned (seq (keep (fn [[c spec]] (when (= addon-id (:addon spec)) c))
                                              cmds))]
                    (record! tool addon-id (vec owned) now)
                    tool)))
          contributions)))

(defn departure
  "{:addon id :at-ms t} for COMMAND's last departure from TOOL-NAME, or nil
   when it never left."
  [tool-name command]
  (get-in @ledger [tool-name (command-key command)]))

(defn clear!
  "Forget every departure. Returns nil."
  []
  (reset! ledger {})
  nil)

(def Departure
  "One departure: the addon that owned the command, and when it left."
  [:map [:addon :any] [:at-ms :int]])

(def Command
  "A command name as a string or keyword."
  [:or :string :keyword])

(m/=> record-owned!
      [:=> [:cat [:map-of :string [:maybe [:map-of :string :map]]] :any] [:vector :string]])

(m/=> record!
      [:function
       [:=> [:cat :string :any [:sequential Command]] :nil]
       [:=> [:cat :string :any [:sequential Command] :int] :nil]])

(m/=> departure [:=> [:cat :string Command] [:maybe Departure]])

(m/=> clear! [:=> [:cat] :nil])
