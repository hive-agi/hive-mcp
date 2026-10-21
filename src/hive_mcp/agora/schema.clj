(ns hive-mcp.agora.schema
  "DataScript schema and CRUD for Agora multi-ling dialogue system.

   Compat shim: moved to hive-agent.swarm.agora.schema in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.agora.schema" sym))

(def dialogue-statuses @(impl 'dialogue-statuses))

(def turn-signals @(impl 'turn-signals))

(def schema @(impl 'schema))

;; Value schemas for the shim contracts. They transcribe the docstrings of
;; hive-agent.swarm.agora.schema, where the behaviour lives.

(def DialogueId
  "A dialogue identifier. Unknown or nil ids are answered (nil, empty), not
   refused."
  [:maybe :string])

(def TurnId
  "A turn identifier."
  [:maybe :string])

(def DialogueStatus
  "`dialogue-statuses`."
  [:enum :active :consensus :timeout :aborted])

(def TurnSignal
  "`turn-signals`: the five signals a recorded turn may carry."
  [:enum :propose :counter :no-change :approve :defer])

(def Conn
  "A DataScript connection: an atom holding the database value."
  [:fn {:gen/schema :nil :gen/fmap (fn [_] (atom {}))}
   (fn [x] (instance? clojure.lang.IAtom x))])

(def TxReport
  "A DataScript transaction report (a record, so a map)."
  [:map [:db-after :any] [:tx-data [:sequential :any]]])

(def Epoch
  "Epoch milliseconds."
  [:int {:min 0}])

(def Uuid
  "A random UUID rendered as a string."
  [:string {:min 36 :max 36 :gen/schema :uuid :gen/fmap str}])

(def Dialogue
  "A dialogue as `get-dialogue` reads it back."
  [:map
   [:id :string]
   [:name [:maybe :string]]
   [:participants [:vector :string]]
   [:status DialogueStatus]
   [:config [:maybe :map]]
   [:created [:maybe Epoch]]])

(def DialogueRow
  "One row of `list-dialogues`."
  [:map
   [:id :string]
   [:name :string]
   [:status DialogueStatus]
   [:created Epoch]])

(def Turn
  "A turn as `get-turns` reads it back. `get-turn` adds the reply and task
   references."
  [:map
   [:id :string]
   [:dialogue-id :string]
   [:sender :string]
   [:receiver :string]
   [:message :string]
   [:signal TurnSignal]
   [:timestamp Epoch]
   [:turn-number [:int {:min 1}]]
   [:in-reply-to {:optional true} [:maybe :string]]
   [:task-ref {:optional true} :any]])

(def SignalCounts
  "Per-signal turn counts; every signal is present, zero when unused."
  [:map
   [:propose [:int {:min 0}]]
   [:counter [:int {:min 0}]]
   [:approve [:int {:min 0}]]
   [:no-change [:int {:min 0}]]
   [:defer [:int {:min 0}]]])

(def CreateDialogueOpts
  "`create-dialogue!` options. At least one participant is required."
  [:map
   [:id {:optional true} [:maybe :string]]
   [:name {:optional true} [:maybe :string]]
   [:participants [:or [:sequential {:min 1} :string] [:set {:min 1} :string]]]
   [:config {:optional true} [:maybe :map]]])

(def TurnOpts
  "`add-turn!` options. The signal defaults to :propose."
  [:map
   [:sender :string]
   [:receiver :string]
   [:message :string]
   [:signal {:optional true} TurnSignal]
   [:in-reply-to {:optional true} [:maybe :string]]
   [:task-ref {:optional true} :any]])

(defn create-conn
  {:arglists '([])}
  [& args]
  (apply (impl 'create-conn) args))

(m/=> create-conn [:=> [:cat] Conn])

(defn get-conn
  {:arglists '([])}
  [& args]
  (apply (impl 'get-conn) args))

(m/=> get-conn [:=> [:cat] Conn])

(defn reset-conn!
  {:arglists '([])}
  [& args]
  (apply (impl 'reset-conn!) args))

;; The body ends in a log call, so the reset answers nil.
(m/=> reset-conn! [:=> [:cat] :nil])

(defn ensure-conn
  {:arglists '([])}
  [& args]
  (apply (impl 'ensure-conn) args))

(m/=> ensure-conn [:=> [:cat] Conn])

(defn now
  {:arglists '([])}
  [& args]
  (apply (impl 'now) args))

(m/=> now [:=> [:cat] Epoch])

(defn gen-id
  {:arglists '([])}
  [& args]
  (apply (impl 'gen-id) args))

(m/=> gen-id [:=> [:cat] Uuid])

(defn config->edn
  {:arglists '([config])}
  [& args]
  (apply (impl 'config->edn) args))

(m/=> config->edn [:=> [:cat [:maybe :map]] :string])

(defn edn->config
  {:arglists '([edn-str])}
  [& args]
  (apply (impl 'edn->config) args))

;; The stored text is always written by `config->edn`, so it reads back as a
;; map; nil stays nil.
(m/=> edn->config [:=> [:cat [:maybe :string]] [:maybe :map]])

(defn create-dialogue!
  {:arglists '([{:keys [id name participants config], :or {config {:threshold 0.8, :timeout-ms 300000}}}])}
  [& args]
  (apply (impl 'create-dialogue!) args))

(m/=> create-dialogue! [:=> [:cat CreateDialogueOpts] [:map [:id :string] [:tx-report TxReport]]])

(defn get-dialogue
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-dialogue) args))

(m/=> get-dialogue [:=> [:cat DialogueId] [:maybe Dialogue]])

(defn update-dialogue-status!
  {:arglists '([dialogue-id new-status])}
  [& args]
  (apply (impl 'update-dialogue-status!) args))

(m/=> update-dialogue-status! [:=> [:cat DialogueId DialogueStatus] [:maybe TxReport]])

(defn- required-impl
  "Like `impl`, for a var hive-agent added after the shim's other vars: an
   older hive-agent raises :swarm/addon-outdated instead of a call on nil."
  [sym]
  (delegate/resolve-required "hive-agent.swarm.agora.schema" sym))

(defn add-participant!
  "Add SLAVE-ID to dialogue DIALOGUE-ID's participants in the agora store.
   Answers the transaction report, or nil when no such dialogue exists.
   Without hive-agent the call raises :swarm/addon-missing; with a hive-agent
   that predates the verb it raises :swarm/addon-outdated."
  {:arglists '([dialogue-id slave-id])}
  [& args]
  (apply (required-impl 'add-participant!) args))

(m/=> add-participant! [:=> [:cat DialogueId :string] [:maybe TxReport]])

(defn remove-participant!
  "Remove SLAVE-ID from dialogue DIALOGUE-ID's participants in the agora
   store. Answers the transaction report, or nil when no such dialogue
   exists. Raises like `add-participant!` without a recent enough hive-agent."
  {:arglists '([dialogue-id slave-id])}
  [& args]
  (apply (required-impl 'remove-participant!) args))

(m/=> remove-participant! [:=> [:cat DialogueId :string] [:maybe TxReport]])

(defn list-dialogues
  {:arglists '([] [status])}
  [& args]
  (apply (impl 'list-dialogues) args))

(m/=> list-dialogues
      [:function
       [:=> [:cat] [:sequential DialogueRow]]
       [:=> [:cat [:maybe :keyword]] [:sequential DialogueRow]]])

(defn add-turn!
  {:arglists '([dialogue-id {:keys [sender receiver message signal in-reply-to task-ref], :or {signal :propose}}])}
  [& args]
  (apply (impl 'add-turn!) args))

(m/=> add-turn!
      [:=> [:cat DialogueId TurnOpts]
       [:maybe [:map [:id :string] [:turn-number [:int {:min 1}]] [:tx-report TxReport]]]])

(defn get-turn
  {:arglists '([turn-id])}
  [& args]
  (apply (impl 'get-turn) args))

(m/=> get-turn [:=> [:cat TurnId] [:maybe Turn]])

(defn get-turns
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'get-turns) args))

(m/=> get-turns [:=> [:cat DialogueId] [:sequential Turn]])

(defn get-turns-by-sender
  {:arglists '([dialogue-id sender-id])}
  [& args]
  (apply (impl 'get-turns-by-sender) args))

(m/=> get-turns-by-sender [:=> [:cat DialogueId [:maybe :string]] [:sequential Turn]])

(defn count-signals
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'count-signals) args))

(m/=> count-signals [:=> [:cat DialogueId] SignalCounts])

(defn check-consensus
  {:arglists '([dialogue-id])}
  [& args]
  (apply (impl 'check-consensus) args))

(m/=> check-consensus
      [:=> [:cat DialogueId]
       [:map
        [:reached? :boolean]
        [:approval-ratio :double]
        [:threshold number?]
        [:signal-counts SignalCounts]]])
