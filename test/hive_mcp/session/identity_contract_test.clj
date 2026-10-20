(ns hive-mcp.session.identity-contract-test
  "Contract facets for the session identity algebra. Worlds, refs and rows
   are drawn over a small id alphabet, so generated slave chains, parent maps
   and adoption candidates collide often enough to reach every rule: a
   coordinator's subtree, a ling's own rows, adoption and foreign rows."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.session.identity :as sid]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;;; ---------------------------------------------------------------------------
;;; Test-domain schemas over a colliding alphabet
;;; ---------------------------------------------------------------------------

(def ^:private Id [:enum "a" "b" "c" "d"])
(def ^:private Sess [:enum "s-a" "s-b" "s-c" "s-d"])
(def ^:private Proj [:enum "hive" "other"])

(def ^:private Slave
  [:map
   [:slave/id Id]
   [:slave/parent-id [:maybe Id]]
   [:slave/depth [:enum 0 1 2]]
   [:slave/session-id [:maybe Sess]]
   [:slave/project-id [:maybe Proj]]])

(def ^:private Coord
  [:map
   [:coordinator/id Id]
   [:coordinator/session-id Sess]
   [:coordinator/project Proj]
   [:coordinator/status [:enum :active :terminated]]])

(def ^:private TWorld
  [:map
   [:slaves [:map-of Id Slave]]
   [:coordinators [:map-of Id Coord]]])

(def ^:private TCaller
  [:map
   [:agent-id [:maybe [:or Id [:= "editor"]]]]
   [:project-id [:maybe Proj]]
   [:session-id [:maybe [:or Sess [:= ""]]]]])

(def ^:private chain
  "s-d -> s-c -> s-b -> s-a: every ordered pair of the alphabet is related."
  {"s-b" "s-a" "s-c" "s-b" "s-d" "s-c"})

(def ^:private TParentOf
  ;; mostly the chain, so ancestry holds for a good share of generated pairs;
  ;; sometimes a random (possibly cyclic) map
  [:or [:= {} chain] [:= {} chain] [:= {} chain] [:map-of Sess Sess]])

(def ^:private TRef
  [:map
   [:session/id [:maybe Sess]]
   [:session/kind sid/SessionKind]
   [:session/project-id [:maybe Proj]]])

(def ^:private TCandidate
  [:maybe
   [:map
    [:session-id [:maybe Sess]]
    [:kind [:enum :adhoc :ling]]
    [:project-id [:maybe Proj]]
    [:parent-id [:maybe Sess]]]])

;;; ---------------------------------------------------------------------------
;;; Oracles — restate the docstrings without calling the subject
;;; ---------------------------------------------------------------------------

(defn- walk-up
  "Ancestors of x under step, nearest first, cycle-safe and bounded."
  [step x]
  (loop [id (step x) seen #{x} acc []]
    (if (or (nil? id) (contains? seen id) (>= (count acc) sid/max-depth))
      acc
      (recur (step id) (conj seen id) (conj acc id)))))

(defn- blank->nil [s] (when (and (string? s) (not (clojure.string/blank? s))) s))

(defn- oracle-owns? [parent-of ref row]
  (let [self (:session/id ref)]
    (boolean (and self row
                  (or (= self row)
                      (and (= :coordinator (:session/kind ref))
                           (some #{self} (walk-up parent-of row))))))))

(defn- oracle-adoptable? [ref {:keys [session-id kind project-id parent-id]}]
  (boolean (and (= :coordinator (:session/kind ref))
                (= :adhoc kind)
                session-id
                (not= session-id (:session/id ref))
                (some? (:session/project-id ref))
                (= project-id (:session/project-id ref))
                (or (nil? parent-id) (= parent-id (:session/id ref))))))

;;; ---------------------------------------------------------------------------
;;; Construction
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema session-ref-normalizes
  #'sid/session-ref
  {:in [:cat [:map
              [:id [:maybe [:enum "s-a" "" "  "]]]
              [:kind [:maybe [:enum :coordinator :ling :adhoc :bogus]]]
              [:parent-id [:maybe [:enum "s-b" ""]]]
              [:depth [:maybe [:enum 0 3]]]
              [:project-id [:maybe [:enum "hive" " "]]]
              [:agent-id [:maybe [:enum "a" ""]]]]]
   :out sid/NormalizedRef
   :rel (fn [[{:keys [id kind parent-id depth project-id agent-id]}] out]
          (let [k (if (#{:coordinator :ling :adhoc} kind) kind :adhoc)]
            (= out {:session/id         (blank->nil id)
                    :session/kind       k
                    :session/parent-id  (when (not= :coordinator k) (blank->nil parent-id))
                    :session/depth      (cond (= :coordinator k) 0 (int? depth) depth :else nil)
                    :session/project-id (blank->nil project-id)
                    :session/agent-id   (blank->nil agent-id)})))
   :golden-path "test/golden/hive-mcp/session/session-ref.edn"
   :num-tests 200})

(deftrifecta-from-schema valid-needs-an-id-and-a-known-kind
  #'sid/valid?
  {:in [:cat [:or :nil :int TRef sid/NormalizedRef]]
   :out :boolean
   :rel (fn [[ref] out]
          (= out (boolean (and (map? ref)
                               (blank->nil (:session/id ref))
                               (#{:coordinator :ling :adhoc} (:session/kind ref))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 200})

;;; ---------------------------------------------------------------------------
;;; Resolution
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema root-slave-is-the-depth-zero-ancestor
  #'sid/root-slave
  {:in [:cat TWorld [:maybe Id]]
   :out [:maybe sid/SlaveRow]
   :rel (fn [[world aid] out]
          (let [slaves (:slaves world)
                chain (map slaves (walk-up #(get-in slaves [% :slave/parent-id]) aid))
                chain (take-while some? chain)]
            (= out (first (filter #(= 0 (:slave/depth %)) chain)))))
   :num-tests 200})

(def ^:private ResolveCase
  ;; two cases in three ask as an agent the world holds a slave row for, so
  ;; the coordinator and ling arms are reached as often as the adhoc one
  [:cat {:gen/schema [:cat TWorld TCaller [:enum :row :row :any]]
         :gen/fmap (fn [[world caller pick]]
                     (let [ids (keys (:slaves world))]
                       (if (and (= :row pick) (seq ids))
                         [world (assoc caller :agent-id (first ids))]
                         [world caller])))}
   TWorld TCaller])

(deftrifecta-from-schema resolve-ref-kind-follows-the-slave-row
  #'sid/resolve-ref
  {:in ResolveCase
   :out sid/NormalizedRef
   :rel (fn [[world {:keys [agent-id session-id]}] out]
          (let [slave (get-in world [:slaves agent-id])]
            (cond
              (and slave (= 0 (:slave/depth slave)))
              (and (= :coordinator (:session/kind out)) (= 0 (:session/depth out)))
              slave
              (and (= :ling (:session/kind out))
                   (= (:slave/depth slave) (:session/depth out)))
              :else
              (and (= :adhoc (:session/kind out))
                   (= (blank->nil (or session-id agent-id)) (:session/id out))))))
   :classify (fn [_ out] (:session/kind out))
   :classify-domain sid/SessionKind
   :num-tests 300})

;;; ---------------------------------------------------------------------------
;;; Ownership
;;; ---------------------------------------------------------------------------

(deftrifecta-from-schema ancestors-walk-the-parent-map
  #'sid/ancestor-session-ids
  {:in [:cat TParentOf [:maybe Sess]]
   :out [:vector :string]
   :rel (fn [[p s] out] (= out (walk-up p s)))
   :num-tests 200})

(def ^:private NSess
  ;; nil a quarter of the time, so the nil guards are reached without
  ;; starving the related pairs
  [:or Sess Sess Sess :nil])

(deftrifecta-from-schema descendant-is-strict-ancestry
  #'sid/descendant-of?
  {:in [:cat TParentOf NSess NSess]
   :out :boolean
   :rel (fn [[p s a] out]
          (= out (boolean (and s a (some #{a} (walk-up p s))))))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   ;; a related pair is about one case in six; a fixed floor keeps the
   ;; true arm required without tying it to the default ratio
   :classify-floor 20
   :num-tests 400})

(def ^:private OwnerRef
  ;; a coordinator three times in four, so the subtree arm is exercised
  [:map
   [:session/id NSess]
   [:session/kind [:enum :coordinator :coordinator :coordinator :ling :adhoc]]
   [:session/project-id [:maybe Proj]]])

(deftrifecta-from-schema owns-is-self-or-coordinator-subtree
  #'sid/owns?
  {:in [:cat TParentOf OwnerRef NSess]
   :out :boolean
   :rel (fn [[p ref row] out] (= out (oracle-owns? p ref row)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :classify-floor 20
   :num-tests 400})

(def ^:private AdoptionCase
  ;; half the cases are aligned: a coordinator ref with a project, and an
  ;; adhoc candidate in that project with no parent, so the adoptable arm is
  ;; reached as well as each refusal
  [:cat {:gen/schema [:cat TRef TCandidate :boolean]
         :gen/fmap (fn [[ref cand align?]]
                     (if-not align?
                       [ref cand]
                       (let [ref (assoc ref :session/kind :coordinator
                                        :session/project-id (or (:session/project-id ref) "hive"))]
                         [ref (assoc (or cand {}) :kind :adhoc :parent-id nil
                                     :session-id (or (:session-id cand) "s-d")
                                     :project-id (:session/project-id ref))])))}
   TRef TCandidate])

(deftrifecta-from-schema adoption-follows-the-hcr-rules
  #'sid/adoptable?
  {:in AdoptionCase
   :out :boolean
   :rel (fn [[ref cand] out] (= out (oracle-adoptable? ref cand)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   :num-tests 400})

;;; ---------------------------------------------------------------------------
;;; Partition / harvest
;;; ---------------------------------------------------------------------------

(def ^:private Row
  [:map
   [:session [:maybe Sess]]
   [:kind [:maybe [:enum :adhoc]]]
   [:project [:maybe Proj]]
   [:parent [:maybe Sess]]])

(defn- row->candidate [row]
  (when (= :adhoc (:kind row))
    {:session-id (:session row) :kind :adhoc
     :project-id (:project row) :parent-id (:parent row)}))

(defn- opts [p] {:parent-of p :row->session :session :row->candidate row->candidate})

(deftrifecta-from-schema partition-is-total-and-ordered
  #'sid/partition-rows
  {:in [:cat [:map [:parent-of TParentOf]
              [:row->session [:= {} :session]]
              [:row->candidate [:= {} row->candidate]]]
        TRef [:vector Row]]
   :out sid/Partition
   :rel (fn [[o ref rows] {:keys [own adopted foreign]}]
          (let [p (:parent-of o)
                own? #(oracle-owns? p ref (:session %))]
            (and (= own (filterv own? rows))
                 (= adopted (filterv #(and (not (own? %))
                                           (oracle-adoptable? ref (row->candidate %)))
                                     rows))
                 (= (+ (count own) (count adopted) (count foreign)) (count rows)))))
   :num-tests 200})

(deftrifecta-from-schema harvestable-is-own-then-adopted
  #'sid/harvestable
  {:in [:cat [:map [:parent-of TParentOf]
              [:row->session [:= {} :session]]
              [:row->candidate [:= {} row->candidate]]]
        TRef [:vector Row]]
   :out [:vector :any]
   :rel (fn [[o ref rows] out]
          (let [p (:parent-of o)
                own? #(oracle-owns? p ref (:session %))]
            (= out (into (filterv own? rows)
                         (filter #(and (not (own? %))
                                       (oracle-adoptable? ref (row->candidate %))))
                         rows))))
   :num-tests 200})

(def ^:private coord-ref {:session/id "s-a" :session/kind :coordinator :session/project-id "hive"})

(golden/deftest-golden partition-every-bucket-golden
  "test/golden/hive-mcp/session/partition-rows.edn"
  (sid/partition-rows (opts {"s-b" "s-a" "s-c" "s-b"}) coord-ref
                      [{:session "s-a"}
                       {:session "s-c"}
                       {:session "s-d"}
                       {:session nil}
                       {:session "s-x" :kind :adhoc :project "hive"}
                       {:session "s-y" :kind :adhoc :project "hive" :parent "s-a"}
                       {:session "s-z" :kind :adhoc :project "hive" :parent "s-d"}
                       {:session "s-w" :kind :adhoc :project "other"}]))

(golden/deftest-golden adoption-rules-golden
  "test/golden/hive-mcp/session/adoptable.edn"
  (let [loose {:session-id "s-x" :kind :adhoc :project-id "hive" :parent-id nil}]
    (into (sorted-map)
          {:loose           (sid/adoptable? coord-ref loose)
           :already-mine    (sid/adoptable? coord-ref (assoc loose :parent-id "s-a"))
           :claimed         (sid/adoptable? coord-ref (assoc loose :parent-id "s-b"))
           :other-project   (sid/adoptable? coord-ref (assoc loose :project-id "other"))
           :ling-session    (sid/adoptable? coord-ref (assoc loose :kind :ling))
           :itself          (sid/adoptable? coord-ref (assoc loose :session-id "s-a"))
           :as-ling         (sid/adoptable? (assoc coord-ref :session/kind :ling) loose)
           :no-ref-project  (sid/adoptable? (dissoc coord-ref :session/project-id) loose)
           :nil-candidate   (sid/adoptable? coord-ref nil)})))

;;; ---------------------------------------------------------------------------
;;; Coverage gate for session/
;;; ---------------------------------------------------------------------------

(coverage/deftest-contract-coverage session-contracts
  {:paths ["src/hive_mcp/session"]})

(deftest session-gate-is-not-vacuous
  (let [report (coverage/coverage {:paths ["src/hive_mcp/session"]})]
    (is (= 17 (count (:universe report))))
    (is (= 17 (count (:covered report))))))

(deftest every-session-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.session.identity hive-mcp.session.current]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (testing (str ns-sym "/" fn-sym)
      (is (= 3 (count (mg/sample side {:size 3 :seed 7})))))))
