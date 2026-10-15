(ns hive-mcp.agent.runtime-schema
  "Value-object schemas for the agent runtime leaves: ring buffer, session
   state ADT, spawn-mode ADT, tool/agent registry, transcript entries.

   Leaf namespace, malli only. Every schema here GENERATES (mg/sample); the
   atom-valued ones carry :gen/schema + :gen/fmap so a test can derive an
   instance from the schema alone.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; ---------------------------------------------------------------------------
;; Ring buffer
;; ---------------------------------------------------------------------------

(def BufferState
  "The map held by a ring-buffer atom."
  [:map
   [:lines [:vector :any]]
   [:capacity [:int {:min 1}]]
   [:total-lines-seen :int]
   [:dropped :int]
   [:timestamps {:optional true} [:vector :int]]])

(def RingBuffer
  "A ring-buffer atom holding a BufferState. The generator builds a
   CONSISTENT buffer (lines and sorted timestamps of equal length)."
  [:fn {:gen/schema [:vector {:max 12} [:tuple :string [:int {:min 0 :max 1000000}]]]
        :gen/fmap   (fn [pairs]
                      (atom {:lines            (mapv first pairs)
                             :timestamps       (vec (sort (map second pairs)))
                             :capacity         (max 1 (count pairs))
                             :total-lines-seen (count pairs)
                             :dropped          0}))}
   (fn [a] (and (instance? clojure.lang.IDeref a) (map? @a)))])

(def LineEntry
  "One line with its append timestamp."
  [:map [:text :any] [:ts :int]])

(def BufferStats
  [:map
   [:current-lines :int]
   [:capacity :int]
   [:total-lines-seen :int]
   [:dropped :int]])

;; ---------------------------------------------------------------------------
;; ADTs
;; ---------------------------------------------------------------------------

(def SessionStateKw
  [:enum :session/idle :session/running :session/done :session/errored :session/aborted])

(def SessionState
  "AgentSessionState ADT value."
  [:map
   [:adt/type [:= :AgentSessionState]]
   [:adt/variant SessionStateKw]])

(def SpawnModeKw
  [:enum :claude :vterm :headless :agent-sdk])

(def SpawnMode
  "SpawnMode ADT value."
  [:map
   [:adt/type [:= :SpawnMode]]
   [:adt/variant SpawnModeKw]])

;; ---------------------------------------------------------------------------
;; Registry
;; ---------------------------------------------------------------------------

(def ToolDef
  "A tool as held by the delegation registry."
  [:map [:name :any]])

;; ---------------------------------------------------------------------------
;; Transcripts
;; ---------------------------------------------------------------------------

(def Entry
  "A transcript entry in either the JSONL or the hive-agent Datalevin shape.
   Open: the accessors read whichever keys exist."
  [:maybe [:map-of :any :any]])

(def ToolCall
  "A tool call map in either shape (stored, or OpenAI style)."
  [:maybe [:map-of :any :any]])

(def Segment
  [:map [:role [:maybe :string]] [:tool [:maybe :any]] [:text :string]])

(def Needle
  "Result of compile-needle: a regex/substring needle, or an error."
  [:or
   [:map [:kind [:enum :regex :substring]] [:pattern [:fn {:gen/elements [#"x" #"(?i)ab"]} #(instance? java.util.regex.Pattern %)]]]
   [:map [:error :string]]])
