;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.channel.drain-telemetry
  "Offer/delivery ledger for the memory piggyback drain.

   `record!` accumulates, per memory entry id, how many drains OFFERED the entry
   (ranked it into the order but left it outside the char budget) and how many
   DELIVERED it (it rode a batch). `shelf-report` joins that against a caller
   supplied `lookup` of the entry's stored access + feedback counters and
   returns one row per id.

   The ledger is pure in-memory accumulation on the drain path — no IO, no store
   read. `store-lookup` is the only IO here and it runs at report time.

   Bounded: 4000 ids, 12h TTL, LRU."
  (:refer-clojure :exclude [reset!])
  (:require [hive-dsl.bounded-atom :refer [bounded-atom bput! bget bclear!
                                           register-sweepable!]]
            [hive-spi.memory.ports :as ports]
            [hive-spi.memory.registry :as mp]
            [taoensso.timbre :as log]
            [hive-mcp.channel.drain-metrics :as dm]
            [malli.core :as m]))

(defonce ^{:doc "Map of memory entry id -> {:offers :delivered :last-seq}."}
  ledger
  (bounded-atom {:max-entries 4000
                 :ttl-ms 43200000
                 :eviction-policy :lru}))
(register-sweepable! ledger :drain-telemetry)

(def ^:const max-drains
  "How many drains of commitment metrics to retain. Bounded because this
   accumulates on the drain path, which runs on every tool call."
  200)

(defonce ^:private drains (atom []))

(def LedgerRow
  "One ledger value: how often an entry was offered and delivered, and the
   drain sequence number that last touched it."
  [:map
   [:offers [:int {:min 0}]]
   [:delivered [:int {:min 0}]]
   [:last-seq {:optional true} :any]])

(defn record!
  "Fold one drain outcome into the ledger.

   `offered-ids` are ids ranked into the order but not taken this drain;
   `delivered-ids` are ids that rode the batch. Both may be nil. `seq-num` is
   the drain sequence number, stored as :last-seq.

   When `batch` is supplied it is the PROJECTED batch as it goes on the wire,
   and its Context Codec metrics are appended to the bounded drain log. Passing
   the unprojected entries would report on a payload nobody receives.

   Returns nil."
  [{:keys [offered-ids delivered-ids seq-num batch]}]
  (letfn [(bump! [k id]
            (when id
              (let [prior (or (bget ledger id) {:offers 0 :delivered 0})]
                (bput! ledger id (-> prior
                                     (update k (fnil inc 0))
                                     (assoc :last-seq seq-num))))))]
    (run! (partial bump! :offers) offered-ids)
    (run! (partial bump! :delivered) delivered-ids)
    (when (seq batch)
      (swap! drains (fn [ds]
                      (->> (conj ds (assoc (dm/metrics batch) :seq-num seq-num))
                           (take-last max-drains)
                           vec))))
    nil))

(m/=> record! [:=> [:cat [:map
                          [:offered-ids {:optional true} [:maybe [:sequential :any]]]
                          [:delivered-ids {:optional true} [:maybe [:sequential :any]]]
                          [:seq-num {:optional true} :any]
                          [:batch {:optional true} [:maybe [:sequential :map]]]]]
                :nil])

(defn commitment-log
  "The retained per-drain Context Codec metrics, oldest first."
  []
  @drains)

(m/=> commitment-log [:=> [:cat] [:vector :map]])

(defn commitment-summary
  "Aggregate of the retained drains. `:worst-critical-atom-recall` is the
   alarm: anything below 1.0 means a safety boundary went out as a pointer,
   which the projection is supposed to make impossible.

   nil for :worst-critical-atom-recall when no drain carried a critical atom."
  []
  (let [ds @drains
        cars (keep :critical-atom-recall ds)
        wars (keep :weighted-atom-recall ds)
        recs (keep :round-trip-recoverability ds)
        avg (fn [xs] (when (seq xs) (/ (reduce + 0.0 xs) (count xs))))]
    {:drains (count ds)
     :worst-critical-atom-recall (when (seq cars) (apply min cars))
     :mean-weighted-atom-recall (avg wars)
     :worst-round-trip-recoverability (when (seq recs) (apply min recs))
     :mean-commitment-density (avg (keep :commitment-density ds))
     :withheld-by-class (apply merge-with + {} (map :withheld-by-class ds))}))

(m/=> commitment-summary [:=> [:cat] [:map
                                      [:drains [:int {:min 0}]]
                                      [:worst-critical-atom-recall [:maybe number?]]
                                      [:mean-weighted-atom-recall [:maybe number?]]
                                      [:worst-round-trip-recoverability [:maybe number?]]
                                      [:mean-commitment-density [:maybe number?]]
                                      [:withheld-by-class [:map-of :any number?]]]])

(defn snapshot
  "Ledger as a plain map of id -> {:offers :delivered :last-seq}."
  []
  (reduce-kv (fn [m id entry] (assoc m id (:data entry)))
             {}
             @(:atom ledger)))

(m/=> snapshot [:=> [:cat] [:map-of :any LedgerRow]])

(defn reset!
  "Clear the ledger and the drain log. For testing and for starting a fresh
   measurement window."
  []
  (bclear! ledger)
  (swap! drains empty)
  nil)

(m/=> reset! [:=> [:cat] :nil])

(defn store-lookup
  "Store-backed `lookup` for `shelf-report`: id -> {:access-count :helpful-count
   :unhelpful-count}. Returns a fn yielding nil for every id when no store is
   configured. Never throws."
  []
  (if-not (mp/store-set?)
    (constantly nil)
    (let [store (mp/get-store)]
      (fn [id]
        (try
          (when-let [e (ports/get-entry store id)]
            (select-keys e [:access-count :helpful-count :unhelpful-count]))
          (catch Throwable t
            (log/debug t "drain-telemetry: lookup failed for" id)
            nil))))))

(m/=> store-lookup [:=> [:cat] [:=> [:cat :any] [:maybe [:map
                                                        [:access-count {:optional true} [:maybe :int]]
                                                        [:helpful-count {:optional true} [:maybe :int]]
                                                        [:unhelpful-count {:optional true} [:maybe :int]]]]]])

(defn shelf-report
  "One row per ledgered id, joined against `lookup`, worst shelf first.

   Row keys: :id :offers :delivered :last-seq :access-count :helpful-count
   :unhelpful-count :shelf-score. `shelf-score` is offers + delivered weighted
   against access — high means the entry keeps being pushed and never read.

   Sorted by :shelf-score descending. Pure given `lookup`."
  [lookup]
  (->> (snapshot)
       (map (fn [[id {:keys [offers delivered last-seq]}]]
              (let [{:keys [access-count helpful-count unhelpful-count]} (lookup id)
                    o (or offers 0)
                    d (or delivered 0)
                    a (or access-count 0)]
                {:id id
                 :offers o
                 :delivered d
                 :last-seq last-seq
                 :access-count a
                 :helpful-count (or helpful-count 0)
                 :unhelpful-count (or unhelpful-count 0)
                 :shelf-score (double (- (+ o d) (* 2 a)))})))
       (sort-by (juxt (comp - :shelf-score) :id))
       vec))

(m/=> shelf-report [:=> [:cat ifn?] [:vector [:map
                                              [:id :any]
                                              [:offers [:int {:min 0}]]
                                              [:delivered [:int {:min 0}]]
                                              [:last-seq :any]
                                              [:access-count :int]
                                              [:helpful-count :int]
                                              [:unhelpful-count :int]
                                              [:shelf-score :double]]]])

(defn summary
  "Aggregate counters over the ledger: entry count, total offers, total
   deliveries, and the offer:delivery ratio (nil when nothing was delivered)."
  []
  (let [rows (vals (snapshot))
        offers (reduce + 0 (keep :offers rows))
        delivered (reduce + 0 (keep :delivered rows))]
    {:entries (count rows)
     :offers offers
     :delivered delivered
     :offer-per-delivery (when (pos? delivered) (double (/ offers delivered)))}))

(m/=> summary [:=> [:cat] [:map
                           [:entries [:int {:min 0}]]
                           [:offers [:int {:min 0}]]
                           [:delivered [:int {:min 0}]]
                           [:offer-per-delivery [:maybe :double]]]])
