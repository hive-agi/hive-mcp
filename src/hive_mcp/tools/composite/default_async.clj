(ns hive-mcp.tools.composite.default-async
  "Which contributed commands a consolidated tool queues by default."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def CommandName
  "A command or verb name, read with `name`."
  [:or :keyword :string :symbol])

(def AddonCommands
  "Command name -> contribution spec. A spec's :default-async is true (the
   command itself) or a collection of its verbs; anything else queues nothing."
  [:maybe [:map-of CommandName
           [:maybe [:map [:default-async {:optional true}
                          [:or :boolean :nil [:sequential CommandName] [:set CommandName]]]]]]])

(def FoldableToolDef
  "A tool definition as `fold` reads and answers it: its default-async
   command set, when it has one, is any collection."
  [:maybe [:map [:default-async-commands {:optional true}
                 [:maybe [:or [:set :any] [:sequential :any]]]]]])

(defn contributed-commands
  "The command keywords ADDON-CMDS (command name -> contribution spec) declare
   queued by default, spelled as `wrap-handler-default-async-for-commands`
   matches them. A spec's :default-async is true (the command itself) or a
   collection of its verbs (each `<command> <verb>`)."
  [addon-cmds]
  (into #{}
        (mapcat (fn [[cmd {:keys [default-async]}]]
                  (cond
                    (true? default-async) [(keyword (name cmd))]
                    (coll? default-async) (map #(keyword (str (name cmd) " " (name %))) default-async))))
        addon-cmds))

(m/=> contributed-commands [:=> [:cat AddonCommands] [:set :keyword]])

(defn fold
  "TOOL-DEF with the commands ADDON-CMDS declare queued by default joined to
   its :default-async-commands. Unchanged when none declares any."
  [tool-def addon-cmds]
  (let [cmds (contributed-commands addon-cmds)]
    (cond-> tool-def
      (seq cmds) (update :default-async-commands (fnil into #{}) cmds))))

(m/=> fold [:=> [:cat FoldableToolDef AddonCommands] FoldableToolDef])
