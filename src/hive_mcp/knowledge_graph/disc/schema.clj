(ns hive-mcp.knowledge-graph.disc.schema
  "Malli value objects for disc (file-state) entities, shared by the function
   contracts of the disc.* namespaces.

   Inputs are permissive where the bodies are: every disc attribute is
   optional because the scoring fns default each one, and a nil disc reads as
   an empty one. Outputs are strict."
  (:require [hive-mcp.knowledge-graph.schema :as kg-schema]
            [malli.util :as mu]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Date
  "A java.util.Date: disc timestamps are stored as, and cast to, Date."
  [:and inst? [:fn {:error/message "should be a java.util.Date"}
               #(instance? java.util.Date %)]])

(def Instant
  "A java.time.Instant."
  [:fn {:error/message "should be a java.time.Instant"
        :gen/schema [:int {:min 0 :max 4102444800000}]
        :gen/fmap #(java.time.Instant/ofEpochMilli %)}
   #(instance? java.time.Instant %)])

(def PositiveNumber
  "A Beta-distribution parameter: a positive finite double, or a positive
   integer.

   BUG (recorded, not fixed): with INTEGER parameters `beta-lower-bound`
   computes `(* sum sum (+ sum 1))` in long arithmetic and throws
   ArithmeticException (long overflow) once alpha + beta exceeds about 2.09e6,
   e.g. {:disc/certainty-alpha 1 :disc/certainty-beta 2097151}. Stored
   parameters are doubles; the integer arm is bounded to the range the body
   survives."
  [:or
   [:and [:double {:gen/infinite? false :gen/NaN? false}]
    [:> 0.0]
    [:fn {:error/message "should be finite"} (fn [x] (Double/isFinite x))]]
   [:int {:min 1 :max 1000000}]])

(def Score
  "A staleness or certainty score in [0, 1]."
  [:and number? [:>= 0] [:<= 1]])

(def Path
  "A non-empty file path."
  [:string {:min 1}])

(def Count
  "A non-negative tally."
  [:int {:min 0}])

(def Disc
  "A disc entity as pulled from the :carto slot, or as handed to the pure
   volatility fns. Every attribute is optional: the scoring fns default each
   one."
  [:map
   [:db/id {:optional true} :int]
   [:disc/path {:optional true} :string]
   [:disc/content-hash {:optional true} :string]
   [:disc/analyzed-at {:optional true} Date]
   [:disc/git-commit {:optional true} :string]
   [:disc/project-id {:optional true} :string]
   [:disc/certainty-alpha {:optional true} PositiveNumber]
   [:disc/certainty-beta {:optional true} PositiveNumber]
   [:disc/volatility-class {:optional true} :keyword]
   [:disc/last-observation {:optional true} Date]
   [:disc/last-read-at {:optional true} Date]
   [:disc/read-count {:optional true} Count]])

(def DiscWithCertainty
  "A disc whose Beta parameters are present, as update-certainty and
   apply-time-decay return it."
  (mu/merge Disc [:map
                  [:disc/certainty-alpha PositiveNumber]
                  [:disc/certainty-beta PositiveNumber]]))

(def VolatilityClass
  "The volatility classes classify-volatility returns."
  kg-schema/DiscVolatilityClass)

(def Sha256Hex
  "A lowercase hex SHA-256 digest."
  [:re {:gen/fmap (fn [bs] (apply str (map #(format "%02x" (bit-and % 0xff)) bs)))
        :gen/schema [:vector {:min 32 :max 32} [:int {:min 0 :max 255}]]}
   #"^[0-9a-f]{64}$"])

(def HashResult
  "What file-content-hash returns: the digest of an existing file, or the
   absence marker."
  [:or
   [:map [:hash Sha256Hex] [:exists? [:= true]]]
   [:map [:exists? [:= false]]]])

(def StalenessReport
  "Diagnostic staleness breakdown for one disc."
  [:map
   [:score Score]
   [:days-since-read [:maybe :int]]
   [:hash-mismatch? :boolean]
   [:never-analyzed? :boolean]])

(def StalenessWarning
  "A warning for one stale path."
  [:map [:path :string] [:staleness Score] [:message :string]])

(def PathColl
  "A collection of paths, as a vector, list or set."
  [:maybe [:or [:sequential :any] [:set :any]]])

(def ProjectOpts
  "The trailing `:project-id` option of a `& {:keys [project-id]}` fn: absent,
   as a key/value pair, or as the single map Clojure 1.11 also accepts."
  [:alt
   [:cat]
   [:cat [:= :project-id] [:maybe :string]]
   [:cat [:maybe [:map [:project-id {:optional true} [:maybe :string]]]]]])

(def Entry
  "The staleness slice of a memory entry."
  [:map
   [:id {:optional true} [:maybe :string]]
   [:staleness-alpha {:optional true} PositiveNumber]
   [:staleness-beta {:optional true} PositiveNumber]
   [:staleness-source {:optional true} [:maybe [:or :string :keyword]]]
   [:staleness-depth {:optional true} [:maybe :int]]
   [:grounded-from {:optional true} [:maybe :string]]])

(def File
  "A java.io.File."
  [:fn {:error/message "should be a java.io.File"
        :gen/schema [:string {:min 1}]
        :gen/fmap #(java.io.File. (str "/tmp/" %))}
   #(instance? java.io.File %)])

(def DecayedDisc
  "What apply-time-decay returns: the disc with a decayed beta and a fresh
   observation time. The beta is any number, not a positive one: a
   last-observation in the future makes the elapsed time, and so the decay,
   negative."
  (mu/merge Disc [:map
                  [:disc/certainty-beta number?]
                  [:disc/last-observation Date]]))

(def EntryReport
  "The staleness report of one memory entry."
  [:map
   [:id [:maybe :string]]
   [:score Score]
   [:alpha PositiveNumber]
   [:beta PositiveNumber]
   [:source [:maybe [:or :string :keyword]]]
   [:depth [:maybe :int]]
   [:grounded-from [:maybe :string]]])

(def DiscSpec
  "What add-disc! takes: a path plus the optional attributes it defaults."
  [:map
   [:path [:maybe :string]]
   [:content-hash {:optional true} [:maybe :string]]
   [:analyzed-at {:optional true} [:maybe Date]]
   [:git-commit {:optional true} [:maybe :string]]
   [:project-id {:optional true} [:maybe :string]]])

(def DiscUpdates
  "Attribute updates merged into a disc entity."
  [:maybe [:map-of :keyword :any]])

(def PathUpdatePairs
  "What batch-update-discs! takes: [path updates] pairs, as a vector, list or
   set, or a {path updates} map. Malformed pairs are dropped by the body."
  [:maybe [:or
           [:sequential [:maybe [:sequential :any]]]
           [:set [:maybe [:sequential :any]]]
           [:map-of :any :any]]])

(def BatchUpdateResult
  "What batch-update-discs! returns."
  [:map [:updated Count] [:tx-data-size Count]])

(def DecayResult
  "What apply-time-decay-to-all-discs! returns."
  [:map [:updated Count] [:skipped Count] [:errors Count]])

(def PropagationResult
  "What propagate-staleness! returns, also on its rescued error path."
  [:map [:propagated Count] [:skipped Count] [:errors Count] [:grounded Count]])

(def StalenessSource
  "The event that made a disc stale; the body takes its `name`."
  [:maybe [:or :keyword :string :symbol]])

(def TopStaleOpts
  "The trailing options of top-stale-files, as key/value pairs or one map."
  [:alt
   [:* [:alt
        [:cat [:= :n] :int]
        [:cat [:= :project-id] [:maybe :string]]
        [:cat [:= :threshold] number?]]]
   [:cat [:maybe [:map
                  [:n {:optional true} :int]
                  [:project-id {:optional true} [:maybe :string]]
                  [:threshold {:optional true} number?]]]]])

(def PathStalenessReport
  "A StalenessReport tagged with the disc path it scores."
  (mu/assoc StalenessReport :path :string))

(def KgFirstOpts
  "The optional opts map of kg-first-context."
  [:maybe [:map [:staleness-threshold {:optional true} [:maybe number?]]]])

(def KgFirstContext
  "What kg-first-context returns: fresh paths with their KG facts, and the
   paths that must be read."
  [:map
   [:kg-known [:map-of :string :map]]
   [:needs-read [:vector :string]]
   [:stale [:vector :string]]
   [:summary [:map
              [:total Count]
              [:known Count]
              [:needs-read Count]
              [:stale Count]]]])
