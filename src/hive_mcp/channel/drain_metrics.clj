(ns hive-mcp.channel.drain-metrics
  "Context Codec metrics over one drain batch.

   An ATOM here is one buffered memory entry, classed by
   `hive-mcp.channel.commitment/classify`. An atom is WHOLE when its content
   rode the wire and POINTERISED when only its address did.

   Four numbers, each answering a different question:

     critical-atom-recall     did every safety boundary arrive whole
     weighted-atom-recall     how much binding weight arrived whole
     commitment-density       commitments per 1000 wire chars
     round-trip-recoverability  can everything withheld still be fetched

   Recall and recoverability are deliberately separate. A pointerised entry
   scores 0 on recall and 1 on recoverability: its content is absent but its
   address is not, and conflating the two is how a lossy channel reports
   itself as lossless.

   Pure, no IO. Plan: memory 20260916002132-51330a59, step-3."
  (:require [hive-mcp.channel.commitment :as commit]
            [malli.core :as m]))

(def Atom
  "One buffered entry reduced to what a metric needs."
  [:map
   [:id [:maybe :string]]
   [:class commit/CommitmentClass]
   [:whole? :boolean]
   [:recoverable? :boolean]])

(def Metrics
  [:map
   [:atoms [:int {:min 0}]]
   [:critical-atom-recall [:maybe [:double {:min 0.0 :max 1.0}]]]
   [:weighted-atom-recall [:maybe [:double {:min 0.0 :max 1.0}]]]
   [:commitment-density [:double {:min 0.0}]]
   [:round-trip-recoverability [:maybe [:double {:min 0.0 :max 1.0}]]]
   [:by-class [:map-of commit/CommitmentClass [:int {:min 0}]]]
   [:withheld-by-class [:map-of commit/CommitmentClass [:int {:min 0}]]]])

(defn whole?
  "True when `entry` carried its content, not merely its address."
  [entry]
  (not (:ref entry)))

(defn recoverable?
  "True when `entry` can be turned back into its full content: it either is
   full already, or it carries the id a pull needs."
  [entry]
  (or (whole? entry) (some? (:id entry))))

(defn atom-of
  "Reduce a buffered entry to its metric shape."
  [entry]
  {:id (:id entry)
   :class (commit/classify (:T entry))
   :whole? (whole? entry)
   :recoverable? (recoverable? entry)})

(defn- mean
  "Mean of `xs`, or nil when there is nothing to average. An empty population
   has no recall, which is not the same as a recall of zero."
  [xs]
  (when (seq xs)
    (/ (double (reduce + xs)) (count xs))))

(defn critical-atom-recall
  "Fraction of CRITICAL atoms that arrived whole. nil when the batch carries
   none. Must read 1.0 whenever a critical atom is present."
  [atoms]
  (mean (for [a atoms :when (commit/critical-classes (:class a))]
          (if (:whole? a) 1.0 0.0))))

(defn weighted-atom-recall
  "Recall weighted by the binding strength of each atom's class."
  [atoms]
  (let [ws (map #(commit/class-weight (:class %)) atoms)
        total (reduce + 0.0 ws)]
    (when (pos? total)
      (/ (reduce + 0.0 (map (fn [a w] (if (:whole? a) w 0.0)) atoms ws))
         total))))

(defn commitment-density
  "Commitments per 1000 chars of `wire-chars`. 0.0 for an empty payload."
  [atoms wire-chars]
  (if (pos? wire-chars)
    (* 1000.0 (/ (double (count atoms)) wire-chars))
    0.0))

(defn round-trip-recoverability
  "Fraction of atoms whose content is either present or still fetchable."
  [atoms]
  (mean (for [a atoms] (if (:recoverable? a) 1.0 0.0))))

(defn metrics
  "Every metric for one drain `batch`, plus the class histograms. `wire-chars`
   defaults to the batch's printed length."
  ([batch] (metrics batch (count (pr-str batch))))
  ([batch wire-chars]
   (let [atoms (mapv atom-of batch)]
     {:atoms (count atoms)
      :critical-atom-recall (critical-atom-recall atoms)
      :weighted-atom-recall (weighted-atom-recall atoms)
      :commitment-density (commitment-density atoms wire-chars)
      :round-trip-recoverability (round-trip-recoverability atoms)
      :by-class (frequencies (map :class atoms))
      :withheld-by-class (frequencies (map :class (remove :whole? atoms)))})))

(m/=> whole? [:=> [:cat [:maybe :map]] :boolean])
(m/=> recoverable? [:=> [:cat [:maybe :map]] :boolean])
(m/=> atom-of [:=> [:cat [:maybe :map]] Atom])
(m/=> critical-atom-recall [:=> [:cat [:sequential Atom]] [:maybe :double]])
(m/=> weighted-atom-recall [:=> [:cat [:sequential Atom]] [:maybe :double]])
(m/=> round-trip-recoverability [:=> [:cat [:sequential Atom]] [:maybe :double]])
(m/=> metrics [:function
               [:=> [:cat [:sequential [:maybe :map]]] Metrics]
               [:=> [:cat [:sequential [:maybe :map]] :int] Metrics]])
