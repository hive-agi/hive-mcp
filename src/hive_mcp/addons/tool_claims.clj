(ns hive-mcp.addons.tool-claims
  "Who holds each addon tool NAME: the pure resolution of addon tool
   contributions against the host's core tools and against each other.

   Addon tools are installed under the names the addon returns, unprefixed.
   This namespace decides, per name N, which contribution (if any) is
   installed. Contributions are taken in REGISTRATION order; rules apply in
   this order:

   1. CLAIM. An addon that both provides N and lists N in `excluded-tools`
      claims N. The first claimant holds N, over a core tool of that name and
      over every other addon. Any other provider of N is refused: `:excluded`,
      or `:contested-claim` when it claimed N as well.
   2. EXCLUSION WITHOUT A CLAIM. An addon listing N in `excluded-tools`
      without providing N refuses every other addon's N (`:excluded`). It
      never removes a core tool.
   3. CORE. A core tool named N stays, and every addon providing N is refused
      (`:shadows-core`). One exception, the legacy supertool form: a `:native`
      addon whose N is `:consolidated`, where the core N is `:consolidated`
      too, holds N as a `:legacy-consolidated` claim.
   4. FIRST PROVIDER. Otherwise the first provider holds N; later providers
      are refused (`:duplicate`). A name repeated inside one addon's own
      tools keeps its first definition.

   Values in, values out: nothing here reads a registry, touches the
   extension registry or logs."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects
;; =============================================================================

(def ToolDef
  "An MCP tool definition as an addon returns it. Only :name is read here."
  [:map [:name :string]])

(def Contribution
  "One active addon's tool contribution."
  [:map
   [:addon-id :any]
   [:addon-type {:optional true} [:maybe :keyword]]
   [:tools [:sequential ToolDef]]
   [:excluded {:optional true} [:maybe [:set :string]]]])

(def CoreTool
  "A host core tool, reduced to what the rules read."
  [:map
   [:name :string]
   [:consolidated {:optional true} [:maybe :boolean]]])

(def RefusalReason
  [:enum :shadows-core :duplicate :excluded :contested-claim])

(def Refusal
  "An addon tool that is NOT installed. :holder is :core, or the addon id
   whose contribution holds the name."
  [:map
   [:addon-id :any]
   [:tool :string]
   [:reason RefusalReason]
   [:holder :any]])

(def Claim
  "A name an addon holds by claim rather than by being first. :over is :core
   when a core tool of that name was displaced, :addon otherwise."
  [:map
   [:addon-id :any]
   [:tool :string]
   [:how [:enum :declared :legacy-consolidated]]
   [:over [:enum :core :addon]]])

(def Resolution
  [:map
   [:installed [:vector ToolDef]]
   [:refused [:vector Refusal]]
   [:claims [:vector Claim]]])

;; =============================================================================
;; Promote — per-name decision
;; =============================================================================

(defn- providers-of
  "Contributions providing tool `n`, in contribution order, each paired with
   its first tool-def for `n`."
  [contributions n]
  (keep (fn [c]
          (when-let [t (some #(when (= n (:name %)) %) (:tools c))]
            {:contribution c :tool t}))
        contributions))

(defn- excludes? [contribution n]
  (contains? (or (:excluded contribution) #{}) n))

(defn- legacy-supertool?
  "The supertool form that predates claims: a native addon's consolidated
   tool standing in for a consolidated core root of the same name."
  [{:keys [contribution tool]} core-tool]
  (and (= :native (:addon-type contribution))
       (true? (:consolidated tool))
       (true? (:consolidated core-tool))))

(defn- refuse [provider reason holder]
  {:addon-id (get-in provider [:contribution :addon-id])
   :tool     (get-in provider [:tool :name])
   :reason   reason
   :holder   holder})

(defn- decide
  "Decision for one tool name: {:holder provider-or-nil :refused [...]
   :claim claim-or-nil}."
  [contributions core-by-name n]
  (let [providers  (providers-of contributions n)
        core-tool  (get core-by-name n)
        id-of      #(get-in % [:contribution :addon-id])
        claimants  (filter #(excludes? (:contribution %) n) providers)
        excluder   (first (filter #(excludes? % n) contributions))]
    (cond
      (seq claimants)
      (let [holder (first claimants)
            hid    (id-of holder)
            claim? (set (map id-of claimants))]
        {:holder  holder
         :claim   {:addon-id hid :tool n :how :declared
                   :over (if core-tool :core :addon)}
         :refused (->> providers
                       (remove #(= hid (id-of %)))
                       (mapv #(refuse % (if (claim? (id-of %)) :contested-claim :excluded) hid)))})

      excluder
      {:holder  nil
       :refused (mapv #(refuse % (if core-tool :shadows-core :excluded)
                               (if core-tool :core (:addon-id excluder)))
                      providers)}

      core-tool
      (if-let [legacy (first (filter #(legacy-supertool? % core-tool) providers))]
        {:holder  legacy
         :claim   {:addon-id (id-of legacy) :tool n :how :legacy-consolidated :over :core}
         :refused (->> providers
                       (remove #(= (id-of legacy) (id-of %)))
                       (mapv #(refuse % :shadows-core :core)))}
        {:holder nil :refused (mapv #(refuse % :shadows-core :core) providers)})

      :else
      (let [holder (first providers)]
        {:holder  holder
         :refused (mapv #(refuse % :duplicate (id-of holder)) (rest providers))}))))

(defn- distinct-by-name
  "`tools` with every repeated :name after the first dropped, order kept."
  [tools]
  (->> tools
       (reduce (fn [[seen acc] t]
                 (if (contains? seen (:name t))
                   [seen acc]
                   [(conj seen (:name t)) (conj acc t)]))
               [#{} []])
       second))

(defn- distinct-names
  "Every tool name contributed, in first-appearance order."
  [contributions]
  (distinct (mapcat #(map :name (:tools %)) contributions)))

(defn- in-addon-duplicates
  "A name listed twice in ONE addon's tools: every repeat after the first is
   refused against the addon itself."
  [contributions]
  (vec (for [{:keys [addon-id tools]} contributions
             [n cnt] (frequencies (map :name tools))
             :when (> cnt 1)
             _ (range (dec cnt))]
         {:addon-id addon-id :tool n :reason :duplicate :holder addon-id})))

;; =============================================================================
;; Public
;; =============================================================================

(defn resolve-claims
  "Resolve `contributions` (active addons, REGISTRATION order) against
   `core-tools` (the host's own tool defs). Returns a Resolution:

     :installed  tool-defs to install, each tagged :addon-source, in
                 contribution order
     :refused    Refusals, one per tool-def not installed
     :claims     Claims, one per name held by claim"
  [core-tools contributions]
  (let [core-by-name (into {} (map (juxt :name identity)) core-tools)
        decisions    (map #(decide contributions core-by-name %) (distinct-names contributions))
        holders      (into #{}
                           (keep (fn [{:keys [holder]}]
                                   (when holder
                                     [(get-in holder [:contribution :addon-id])
                                      (get-in holder [:tool :name])])))
                           decisions)
        installed    (vec (for [{:keys [addon-id tools]} contributions
                                t (distinct-by-name tools)
                                :when (contains? holders [addon-id (:name t)])]
                            (assoc t :addon-source addon-id)))]
    {:installed installed
     :refused   (into (vec (mapcat :refused decisions)) (in-addon-duplicates contributions))
     :claims    (vec (keep :claim decisions))}))

(defn refusals-by-addon
  "Refusals grouped by addon id: {addon-id [Refusal ...]}."
  [resolution]
  (group-by :addon-id (:refused resolution)))

(defn claimed-core-names
  "Names an addon holds OVER a core tool of the same name. The host must DROP
   its own tool for each: the addon's is installed under that name, and
   advertising both would put two tools with one name on the wire."
  [resolution]
  (into #{} (comp (filter #(= :core (:over %))) (map :tool)) (:claims resolution)))

(def valid-resolution?
  "Malli validator for a Resolution."
  (m/validator Resolution))
