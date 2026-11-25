(ns hive-mcp.tools.kanban.diff.schema
  "Malli value objects for `kanban diff` (RUN-LEDGER-5).

   DiffRequest  the two tag sets, coerced and non-empty.
   CardKey      what a card is matched on across runs: its scenario:<x> and
                persona:<p> tags (convention 20261125100357-6730324c).
   SideCell     every card one run holds under one key: ids and verdict.
   SideIndex    one run, keyed.
   DiffRow      one key across both runs, classified.
   Summary      rows counted per class, plus the total.
   DiffReport   what `kanban diff` answers.

   Generators are bounded so the synthesized suites stay fast.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Tag [:string {:min 1 :gen/max 24}])

(def Tags
  "A side's tag set: at least one tag; every card of the side carries all."
  [:vector {:min 1 :gen/max 3} Tag])

(def DiffRequest
  [:map {:closed true}
   [:tags-a Tags]
   [:tags-b Tags]])

(def Verdict
  "The value of a card's verdict:<v> tag (pass, fail-product, ...). When the
   cards under one key carry several verdicts they are joined with |."
  [:string {:min 1 :gen/max 16}])

(def DiffClass
  "flipped      both runs hold a verdict and they differ
   same         both runs hold the same verdict
   only-a       the key is in run A only
   only-b       the key is in run B only
   unverdicted  the key is in both runs and at least one holds no verdict"
  [:enum "flipped" "same" "only-a" "only-b" "unverdicted"])

(def classes
  "Every DiffClass, in the order rows are reported."
  ["flipped" "only-a" "only-b" "unverdicted" "same"])

(def KeyText [:string {:min 1 :gen/max 40}])

(def Part [:maybe [:string {:gen/max 12}]])

(def CardKey
  [:map {:closed true}
   [:key      KeyText]
   [:scenario Part]
   [:persona  Part]])

(def Ids [:vector {:gen/max 4} [:string {:gen/max 12}]])

(def SideCell
  [:map {:closed true}
   [:key      KeyText]
   [:scenario Part]
   [:persona  Part]
   [:ids      Ids]
   [:verdict  [:maybe Verdict]]])

(def MaybeCell [:maybe SideCell])

(def SideIndex [:map-of {:gen/max 4} KeyText SideCell])

(def DiffRow
  [:map {:closed true}
   [:key       KeyText]
   [:scenario  Part]
   [:persona   Part]
   [:class     DiffClass]
   [:verdict_a [:maybe Verdict]]
   [:verdict_b [:maybe Verdict]]
   [:ids_a     Ids]
   [:ids_b     Ids]])

(def Rows [:vector {:gen/max 6} DiffRow])

(def Count [:int {:min 0 :gen/max 50}])

(def Summary
  [:map {:closed true}
   [:flipped     Count]
   [:same        Count]
   [:only-a      Count]
   [:only-b      Count]
   [:unverdicted Count]
   [:total       Count]])

(def DiffReport
  [:map {:closed true}
   [:tags_a  Tags]
   [:tags_b  Tags]
   [:summary Summary]
   [:rows    Rows]])

(def RequestResult
  "`request`: the coerced DiffRequest, or the message a caller can act on."
  [:or
   [:map {:closed true} [:ok DiffRequest]]
   [:map {:closed true} [:error [:string {:min 1}]]]])
