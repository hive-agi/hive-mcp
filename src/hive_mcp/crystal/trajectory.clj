(ns hive-mcp.crystal.trajectory
  "The `trajectory` catchup block (SYNTHESIS-HISTORY slice (d)): the last N
   (default 3) wrap summaries of the project, newest first, one line each,
   built on `hive-mcp.crystal.history/history`.

     Promote   trajectory-lines, budget-lines    pure: History -> bounded lines
     Section   trajectory-section                pure: the block value or nil
     Gather    gather-trajectory                 port -> section, never throws
     Block     block, register!                  hive-spi.catchup.registry

   It lives with the memory domain, not in tools.catchup: the kernel catchup
   never names a crystal namespace, it composes whatever block is registered.
   `hive-mcp.tools.consolidated.session` registers it on load.

   Budgeted by a hard char cap over the rendered lines, and fail-soft: no
   store, no KG, a read error or no summary all yield nil, which the host
   renders as no section at all, never as an error."
  (:require [clojure.string :as str]
            [hive-mcp.crystal.history :as history]
            [hive-spi.catchup.registry :as blocks]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-n
  "Summaries the block shows when the context names no count."
  3)

(def char-cap
  "Hard ceiling on the summed length of the block's lines."
  480)

(def ^:private hint
  "Newest wrap summaries first, one line each. Full chain: session history; one body: memory get :id <id>.")

(def Row
  [:map [:id :string] [:line :string]])

(def Section
  "The block value."
  [:map
   [:trajectory [:vector :string]]
   [:ids [:vector :string]]
   [:hint :string]
   [:truncated {:optional true} :boolean]])

(defn trajectory-lines
  "[{:id :line}] for the entries of HISTORY that carry a digest, in order,
   each line `<date>  <digest>`. A summary missing from the store or holding
   no text has nothing to show and is skipped."
  [{:keys [entries]}]
  (into []
        (keep (fn [{:keys [id date digest missing]}]
                (when (and (not missing) (string? id) (string? digest) (not (str/blank? digest)))
                  {:id id :line (str (or date "????-??-??") "  " digest)})))
        entries))

(m/=> trajectory-lines
      [:=> [:cat [:maybe [:map [:entries {:optional true} [:maybe [:sequential history/HistoryEntry]]]]]]
       [:vector Row]])

(defn budget-lines
  "The longest prefix of ROWS whose lines sum to at most CAP chars. When even
   the first line is over CAP it is cut to CAP chars (ending in an ellipsis),
   so a non-empty input never yields an empty block. :truncated reports
   whether anything was dropped or cut."
  [rows cap]
  (let [kept (loop [acc [] used 0 rs (seq rows)]
               (if-let [r (first rs)]
                 (let [n (count (:line r))]
                   (if (<= (+ used n) cap)
                     (recur (conj acc r) (+ used n) (next rs))
                     acc))
                 acc))]
    (cond
      (seq kept) {:rows kept :truncated (< (count kept) (count rows))}
      (seq rows) {:rows [(update (first rows) :line #(str (subs % 0 (max 0 (dec cap))) "…"))]
                  :truncated true}
      :else      {:rows [] :truncated false})))

(m/=> budget-lines
      [:=> [:cat [:sequential Row] [:int {:min 1}]]
       [:map [:rows [:vector Row]] [:truncated :boolean]]])

(defn trajectory-section
  "The block value for HISTORY, or nil when there is nothing to show (no
   summary, a read error with no entries, or no entry with text)."
  [hist]
  (let [{:keys [rows truncated]} (budget-lines (trajectory-lines hist) char-cap)]
    (when (seq rows)
      (cond-> {:trajectory (mapv :line rows)
               :ids        (mapv :id rows)
               :hint       hint}
        truncated (assoc :truncated true)))))

(m/=> trajectory-section [:=> [:cat [:maybe :map]] [:maybe Section]])

(defn gather-trajectory
  "The trajectory section of PROJECT-ID through PORT (an ISummaryHistory),
   at most N summaries (default `default-n`). nil when PORT or PROJECT-ID is
   absent or anything fails: this block never errors a catchup."
  [port project-id n]
  (when (and (some? port) (string? project-id) (not (str/blank? project-id)))
    (try
      (trajectory-section (history/history port project-id (or n default-n)))
      (catch Throwable _history-unavailable-means-no-block nil))))

(m/=> gather-trajectory [:=> [:cat :any [:maybe :string] [:maybe :int]] [:maybe Section]])

(defn block-value
  "The :block/fn body: the trajectory of the context's :project-id over the
   live store and KG (`history/store-history`); nil when either is down."
  [ctx]
  (try
    (gather-trajectory (history/store-history) (:project-id ctx) (:trajectory-n ctx))
    (catch Throwable _history-unavailable-means-no-block nil)))

(m/=> block-value [:=> [:cat [:maybe :map]] [:maybe Section]])

(def block
  "The catchup block contribution. Cached per project; a write tagged
   session-summary (a new wrap) drops the entry."
  {:block/id    :trajectory
   :block/fn    block-value
   :block/order 20
   :block/cache {:key-by [:project-id] :drop-tags #{"session-summary"}}})

(defn register!
  "Install the trajectory block in the catchup block registry. Returns it."
  []
  (blocks/register-block! block))

(m/=> register! [:=> [:cat] [:map [:block/id [:= :trajectory]] [:block/order :int]]])
