(ns hive-mcp.context.budget
  "Context budget delegation stubs.

   Delegates to extension if available. Returns noop defaults otherwise.

   Extension points are resolved via the extensions registry at startup.
   When no extensions are registered, all functions gracefully degrade
   to pass-through results."
  (:require [hive-mcp.extensions.delegate :refer [delegate-or-noop]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Extension Delegation Helpers
;; =============================================================================

;; =============================================================================
;; Constants
;; =============================================================================

(def ^:const default-total-budget
  "Default token budget for context augmentation."
  4000)

;; =============================================================================
;; Public API — delegates to extension or returns noop
;; =============================================================================

(defn estimate-tokens
  "Estimate token count for text.
   Delegates to extension if available."
  [text]
  (delegate-or-noop :cb/a 0 [text]))

(defn truncate-to-budget
  "Truncate text to fit within a token budget.
   Delegates to extension if available."
  [text token-budget]
  (delegate-or-noop :cb/b
                    {:content (or text "") :tokens 0 :truncated? false}
                    [text token-budget]))

(defn allocate-ling-context
  "Allocate token budget across ling context sections.
   Delegates to extension if available."
  [{:keys [total-budget preset axioms context history] :as input}]
  (delegate-or-noop :cb/c
                    {:preset   (or preset "")
                     :axioms   (or axioms "")
                     :context  (or context "")
                     :history  (or history "")
                     :metadata {:total-budget (or total-budget 0)
                                :total-tokens 0
                                :remaining    (or total-budget 0)}}
                    [input]))

(defn allocate-unified-entries
  "Allocate token budget across entry vectors.
   Delegates to extension if available."
  [{:keys [total-budget conventions decisions snippets domain] :as input}]
  (delegate-or-noop :cb/e
                    {:conventions      (or conventions [])
                     :decisions        (or decisions [])
                     :snippets         (or snippets [])
                     :domain           (or domain [])
                     :budget-used      0
                     :budget-remaining (or total-budget 0)
                     :metadata         {:total-budget (or total-budget 0)
                                        :total-tokens 0
                                        :remaining    (or total-budget 0)
                                        :counts       {:conventions (count (or conventions []))
                                                       :decisions   (count (or decisions []))
                                                       :snippets    (count (or snippets []))
                                                       :domain      (count (or domain []))}}}
                    [input]))

(def Tokens
  "A token count."
  [:int {:min 0}])

(def BudgetMetadata
  "The accounting block both allocators attach."
  [:map
   [:total-budget :int]
   [:total-tokens :int]
   [:remaining :int]])

(def TruncatedText
  "What `truncate-to-budget` answers."
  [:map
   [:content :string]
   [:tokens :int]
   [:truncated? :boolean]])

(def LingContextInput
  "Sections `allocate-ling-context` splits a budget across."
  [:map
   [:total-budget {:optional true} [:maybe :int]]
   [:preset {:optional true} [:maybe :string]]
   [:axioms {:optional true} [:maybe :string]]
   [:context {:optional true} [:maybe :string]]
   [:history {:optional true} [:maybe :string]]])

(def LingContext
  "The allocated ling context."
  [:map
   [:preset :string]
   [:axioms :string]
   [:context :string]
   [:history :string]
   [:metadata BudgetMetadata]])

(def UnifiedEntriesInput
  "Entry vectors `allocate-unified-entries` splits a budget across."
  [:map
   [:total-budget {:optional true} [:maybe :int]]
   [:conventions {:optional true} [:maybe [:sequential :any]]]
   [:decisions {:optional true} [:maybe [:sequential :any]]]
   [:snippets {:optional true} [:maybe [:sequential :any]]]
   [:domain {:optional true} [:maybe [:sequential :any]]]])

(def UnifiedEntries
  "The allocated entries."
  [:map
   [:conventions [:sequential :any]]
   [:decisions [:sequential :any]]
   [:snippets [:sequential :any]]
   [:domain [:sequential :any]]
   [:budget-used :int]
   [:budget-remaining :int]
   [:metadata [:map
               [:total-budget :int]
               [:total-tokens :int]
               [:remaining :int]
               [:counts [:map-of :keyword :int]]]]])

(m/=> estimate-tokens [:=> [:cat [:maybe :string]] Tokens])

(m/=> truncate-to-budget [:=> [:cat [:maybe :string] :int] TruncatedText])

(m/=> allocate-ling-context [:=> [:cat LingContextInput] LingContext])

(m/=> allocate-unified-entries [:=> [:cat UnifiedEntriesInput] UnifiedEntries])
