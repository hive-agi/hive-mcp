(ns hive-mcp.channel.commitment
  "Context Codec commitment classes for hive memory types.

   `classify` is TOTAL: a type with no entry in `type->class` resolves to
   `default-class`, which is deliberately not the weakest class. `critical?`
   names the Critical Atom Recall population, and agrees with
   `drain-rank/floor-types` by test rather than by construction.

   Pure, no IO. The live type registry is never read here; covering it is a
   test obligation discharged by hive-mcp.channel.commitment-test.

   Plan: memory 20260916002132-51330a59, step-1."
  (:require [hive-mcp.channel.drain-rank :as rank]
            [malli.core :as m]))

(def classes
  "Commitment classes, ordered most binding first."
  [:safety-boundary :constraint :decision :goal :preference
   :artifact :evidence :tool-result])

(def CommitmentClass
  "A commitment class."
  (into [:enum] classes))

(def TypeToken
  "A memory type as it arrives on the wire: keyword, string or symbol."
  [:maybe [:or :keyword :string :symbol]])

(def ^:private order
  (zipmap classes (range)))

(def default-class
  "Class a type with no explicit classification resolves to."
  :constraint)

(def type->class
  "Hive memory type -> commitment class."
  {:axiom           :safety-boundary

   :principle       :constraint
   :convention      :constraint
   :rule            :constraint
   :guideline       :constraint
   :warning         :constraint

   :decision        :decision

   :axiom-candidate :goal
   :plan            :goal
   :todo            :goal
   :question        :goal

   :feedback        :preference

   :snippet         :artifact
   :recipe          :artifact
   :workflow        :artifact
   :doc             :artifact

   :note            :evidence
   :knowledge       :evidence
   :lesson          :evidence
   :pattern         :evidence
   :answer          :evidence
   :error           :evidence

   :ingestion       :tool-result})

(defn classify
  "Commitment class of memory type `t`. Total: a type absent from
   `type->class` resolves to `default-class`."
  [t]
  (get type->class (rank/kw t) default-class))

(defn classified?
  "True when `t` carries an explicit entry in `type->class`, false when
   `classify` would fall back to `default-class`."
  [t]
  (contains? type->class (rank/kw t)))

(def critical-classes
  "Classes whose atoms Critical Atom Recall must score at 1.0. Widening this
   set is a behaviour change, not a tuning knob: it is asserted equal to
   `drain-rank/floor-types` by test."
  #{:safety-boundary})

(defn critical?
  "True when `t` is a critical atom: one whose Critical Atom Recall must
   read 1.0."
  [t]
  (contains? critical-classes (classify t)))

(defn class-weight
  "Weighted Atom Recall weight of commitment class `cls`: 1.0 for the most
   binding class, descending in equal steps to 1/(count classes) for the
   least. nil when `cls` is not a commitment class."
  [cls]
  (when-let [i (order cls)]
    (/ (double (- (count classes) i)) (count classes))))

(defn weight
  "Weighted Atom Recall weight of memory type `t`."
  [t]
  (class-weight (classify t)))

(m/=> classify [:=> [:cat TypeToken] CommitmentClass])
(m/=> classified? [:=> [:cat TypeToken] :boolean])
(m/=> critical? [:=> [:cat TypeToken] :boolean])
(m/=> class-weight [:=> [:cat :any] [:maybe [:double {:min 0.0 :max 1.0}]]])
(m/=> weight [:=> [:cat TypeToken] [:double {:min 0.0 :max 1.0}]])
