(ns hive-mcp.tools.consolidated.workflow.forge-execution-propagation-test
  "A card's :execution travels plan memory -> plan-to-kanban card :context ->
   kanban list row -> forge survey -> spark! -> spawn params. Every hop here
   consumes the previous hop's real output: the plan is indexed in a stub store
   that answers reads the way a JSON-writing backend does, plan-to-kanban and
   kanban list run for real, the belt runs for real through
   build-fsm-resources, and only the spark!/smite! effects are ports.

   Pinned here:
   1. kanban list rows stay slim: no :context.
   2. The spawn params the spawn port receives for the routed card carry its
      provider, model, spawn_mode and presets, and its persona is registered
      under the spawned name before that spawn; the plain card gets none.
   3. The prompt dispatched to the routed ling carries the card description.
   4. 2 and 3 hold for an unscoped strike too.
   5. survey fails closed on a listed card it cannot read back as todo, and
      reads a plan card at most twice.
   6. The forge survey verb reports each task's routing and no card body.
   7. An orchestrator strike, and spark! in orchestrator mode, reports the
      routed card in :failed as :execution/unsupported-mode, dispatches the
      plain card, and spawns nothing when only routed cards remain. The removed
      drone mode is refused outright.
   8. In :mixed mode slots go in survey order and every card goes to a ling."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.addons.headless :as headless]
            [hive-mcp.agent.ling.headless-registry :as headless-reg]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.isolation-methods]
            [hive-mcp.plan.tool :as plan-tool]
            [hive-mcp.test.stub.forge-belt :as stub-belt]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.consolidated.kanban :as c-kanban]
            [hive-mcp.tools.consolidated.workflow :as wf]
            [hive-mcp.tools.consolidated.workflow.forge-cycle :as cycle]
            [hive-mcp.tools.consolidated.workflow.forge-ops :as forge-ops]
            [hive-mcp.tools.consolidated.workflow.spawn :as spawn]
            [hive-mcp.vectordb.facade :as memory]
            [hive-mcp.vectordb.kanban-facade :as kanban-store]
            [hive-mcp.workflows.forge-belt :as belt]
            [hive-spi.memory.registry :as store-registry]
            [hive-test.isolation :as iso]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private dir "/tmp/forge-execution-propagation")

(def ^:private project-id "forge-execution-propagation")

(def ^:private routed-mode :hive-agent)

(def ^:private execution
  {:provider "ollama-compat" :model "qwen2.5:3b-instruct" :spawn-mode "hive-agent"
   :presets ["ling"] :persona {:caps {:decisions 2 :conventions 1 :snippets 0}}})

(def ^:private plan-content
  (str "```edn\n"
       (pr-str {:id "propagation" :title "Execution propagation"
                :steps [{:id "step-1" :title "Routed step" :description "Routed step body"
                         :priority :high :execution execution}
                        {:id "step-2" :title "Plain step" :description "Plain step body"
                         :priority :medium}]})
       "\n```"))

(def ^:private events
  "Ordered log of persona registrations and spark! port calls for one test."
  (atom []))

;; ── Fixtures ────────────────────────────────────────────────────────────────

(defn- store-fixture
  "One decoding stub store under :default and :kanban; the prior registry is restored."
  [f]
  (let [prior (store-registry/registered-stores)
        store (mem-stub/->stub nil {:decode-content? true})]
    (try
      (store-registry/reset-registry!)
      (store-registry/register-store! :default store)
      (store-registry/register-store! :kanban store)
      (f)
      (finally
        (store-registry/reset-registry!)
        (doseq [[k s] prior] (store-registry/register-store! k s))))))

(defn- persona-fixture
  "Recording persona-lens providers; the prior registrations are restored."
  [f]
  (let [ks [:agent/register-persona-lens :agent/unregister-persona-lens]
        prior (into {} (map (fn [k] [k (ext/get-extension k)])) ks)]
    (try
      (reset! events [])
      (ext/register! :agent/register-persona-lens
                     (fn [id lens] (swap! events conj [:register id lens])))
      (ext/register! :agent/unregister-persona-lens
                     (fn [id] (swap! events conj [:unregister id])))
      (f)
      (finally
        (doseq [[k v] prior]
          (if v (ext/register! k v) (ext/deregister! k)))))))

(defn- stub-backend
  "Headless backend that only answers lookups; spawning through it throws."
  []
  (reify
    headless/IHeadlessBackend
    (headless-id [_] routed-mode)
    (headless-spawn! [_ _ _] (throw (ex-info "stub backend never spawns" {})))
    (headless-dispatch! [_ _ _] (throw (ex-info "stub backend never dispatches" {})))
    (headless-status [_ _ _] {:alive? false})
    (headless-kill! [_ _] {:killed? true})
    (headless-interrupt! [_ _] {:success? false})

    headless/IHeadlessCapabilities
    (declared-capabilities [_] #{})))

(defn- headless-fixture
  "Register the routed mode as a backend providing :ollama-compat; the prior entry is restored."
  [f]
  (let [prior (headless-reg/get-headless-backend routed-mode)
        prior-meta (headless-reg/headless-metadata routed-mode)]
    (try
      (headless-reg/register-headless! routed-mode (stub-backend) {:provides #{:ollama-compat}})
      (f)
      (finally
        (headless-reg/deregister-headless! routed-mode)
        (when prior (headless-reg/register-headless! routed-mode prior prior-meta))))))

(use-fixtures :each
  store-fixture
  (iso/with-isolations :kg-conn)
  stub-belt/forge-belt-fixture
  persona-fixture
  headless-fixture)

;; ── Hops ────────────────────────────────────────────────────────────────────

(defn- convert-plan!
  "Hops 0-1: index the plan memory, run the real plan-to-kanban.
   Returns {:plan-id id :cards {step-id card-id}}."
  []
  (let [plan-id (str "plan-" (random-uuid))
        _ (memory/index-memory-entry! {:id plan-id :content plan-content :type "plan"
                                       :tags ["plan"] :project-id project-id})
        result (plan-tool/handle-plan-to-kanban {:plan_id plan-id :directory dir})]
    (is (not (:isError result)) (str "plan-to-kanban failed: " (:text result)))
    {:plan-id plan-id
     :cards (get (json/read-str (:text result)) "step-mapping")}))

(defn- list-rows
  "Hop 2: the real kanban list answer for the todo column."
  []
  (json/read-str (:text (c-kanban/handle-kanban {:command "list" :status "todo" :directory dir}))
                 :key-fn keyword))

(defn- spark-ports
  "Recording spark! ports: every spawn succeeds, every ling is ready, every dispatch succeeds."
  []
  {:spawn-agent-fn (fn [params]
                     (swap! events conj [:spawn params])
                     {:text (json/write-str {:agent-id (:name params) :success true})})
   :await-ready-fn (fn [agent-id _mode] {:ready? true :slave {:slave/id agent-id}})
   :send-prompt-fn (fn [msg]
                     (swap! events conj [:dispatch msg])
                     {:text "{\"success\":true}"})
   :kanban-fn      (fn [cmd] (swap! events conj [:kanban cmd]) {:text "{}"})
   :agents-fn      (constantly [])
   :project-id-fn  (constantly project-id)})

(defn- strike!
  "Hop 3: the real belt over real build-fsm-resources, effects through ports."
  [params]
  (belt/run-single-strike
   (cycle/build-fsm-resources (merge {:directory dir} params)
                              {:spark (spark-ports)
                               :smite {:agents-fn (constantly [])
                                       :project-id-fn (constantly project-id)}})))

;; ── Last-hop assertions ─────────────────────────────────────────────────────

(defn- spawns []
  (keep (fn [[k p]] (when (= :spawn k) p)) @events))

(defn- spawn-for [card-id]
  (first (filter #(= card-id (:kanban_task_id %)) (spawns))))

(defn- position [pred]
  (first (keep-indexed (fn [i e] (when (pred e) i)) @events)))

(defn- prompt-for [agent-id]
  (some (fn [[k m]] (when (and (= :dispatch k) (= agent-id (:agent_id m))) (:prompt m)))
        @events))

(defn- routed-card-reached-spawn [cards]
  (let [routed (spawn-for (get cards "step-1"))
        plain  (spawn-for (get cards "step-2"))]
    (is (= 2 (count (spawns))) "each card is spawned exactly once")
    (testing "the routed card's execution reaches the spawn params"
      (is (= {:provider "ollama-compat" :model "qwen2.5:3b-instruct"
              :spawn_mode "hive-agent" :presets ["ling"]}
             (select-keys routed [:provider :model :spawn_mode :presets]))))
    (testing "the routed card's persona is registered under the spawned name before the spawn"
      (let [registered (position #(= [:register (:name routed) (:persona execution)] %))
            spawned    (position #(= [:spawn routed] %))]
        (is (some? registered))
        (is (and registered spawned (< registered spawned)))))
    (testing "the plain card gets no routing and no persona"
      (is (some? plain))
      (is (not (contains? plain :provider)))
      (is (not (contains? plain :spawn_mode)))
      (is (nil? (position #(and (= :register (first %)) (= (:name plain) (second %)))))))
    (testing "the routed ling's prompt carries the card description"
      (is (str/includes? (str (prompt-for (:name routed))) "Routed step body")))))

;; ── Tests ───────────────────────────────────────────────────────────────────

(deftest routed-card-reaches-spawn-params-through-a-plan-strike
  (let [{:keys [plan-id cards]} (convert-plan!)]
    (testing "hop 2: the list row for the routed card is slim"
      (let [row (first (filter #(= (get cards "step-1") (:id %)) (list-rows)))]
        (is (some? row))
        (is (not (contains? row :context)))))
    (let [r (strike! {:plan_id plan-id})]
      (is (= :ready (get-in r [:survey-result :selection-status])))
      (routed-card-reached-spawn cards))))

(deftest routed-card-reaches-spawn-params-through-an-unscoped-strike
  (let [{:keys [cards]} (convert-plan!)
        r (strike! {})]
    (is (= :ready (get-in r [:survey-result :selection-status])))
    (routed-card-reached-spawn cards)))

(defn- entry-as
  "Store entry for id with its content status replaced."
  [id status]
  (update (kanban-store/get-entry-by-id id) :content assoc :status status))

(deftest survey-fails-closed-on-a-card-it-cannot-read-back
  (let [{:keys [cards]} (convert-plan!)
        routed (get cards "step-1")
        plain  (get cards "step-2")
        faulty (fn [answer]
                 (fn [id] (if (= id routed) (answer id) (kanban-store/get-entry-by-id id))))]
    (doseq [[label entry-fn state]
            [["missing" (faulty (constantly nil)) :missing]
             ["lookup error" (faulty (fn [_] (throw (ex-info "store offline" {})))) :lookup-error]
             ["not a kanban card" (faulty (fn [id] (assoc-in (kanban-store/get-entry-by-id id)
                                                             [:content :task-type] "note")))
              :invalid]
             ["moved to doing" (faulty #(entry-as % "doing")) :not-todo]]]
      (testing label
        (let [s (forge-ops/survey {:directory dir :task-entry-fn entry-fn})]
          (is (= [plain] (mapv :id (:tasks s))) "only the readable todo card is selected")
          (is (= 1 (:count s)))
          (is (= [state] (keep #(when (= routed (:task-id %)) (:state %)) (:blocked s))))
          (is (= 1 (:blocked-count s)))
          (is (= :ready (:selection-status s))))
        (let [s (forge-ops/survey {:directory dir :task-entry-fn entry-fn :task_ids [routed]})]
          (is (empty? (:tasks s)))
          (is (= :blocked (:selection-status s))))))))

(deftest plan-survey-reads-each-card-at-most-twice
  (let [{:keys [plan-id cards]} (convert-plan!)
        reads (atom {})
        counting (fn [id]
                   (swap! reads update id (fnil inc 0))
                   (kanban-store/get-entry-by-id id))
        s (forge-ops/survey {:plan_id plan-id :directory dir :task-entry-fn counting})
        routed (first (filter #(= (get cards "step-1") (:id %)) (:tasks s)))]
    (is (= :ready (:selection-status s)))
    (is (= execution (get-in routed [:context :execution])))
    (is (= 0 (:wave-number routed)))
    (is (= #{(get cards "step-1") (get cards "step-2")} (set (keys @reads))))
    (is (every? #(<= % 2) (vals @reads)) (str "reads per card: " @reads))))

(deftest survey-verb-reports-routing-without-card-bodies
  (let [{:keys [plan-id cards]} (convert-plan!)
        handler (get-in wf/canonical-handlers [:forge :survey])
        res (handler {:plan_id plan-id :directory dir})
        body (json/read-str (:text res) :key-fn keyword)
        by-id (into {} (map (juxt :id identity)) (:tasks body))
        routed (get by-id (get cards "step-1"))
        plain (get by-id (get cards "step-2"))]
    (is (not (:isError res)) (str (:text res)))
    (is (= {:provider "ollama-compat" :model "qwen2.5:3b-instruct"
            :spawn-mode "hive-agent" :presets ["ling"]}
           (:execution routed)))
    (is (true? (:persona? routed)))
    (is (some? plain))
    (is (nil? (:execution plain)))
    (is (not-any? #(contains? % :description) (:tasks body)))
    (is (not-any? #(contains? % :context) (:tasks body)))))

;; ── Modes that cannot honor per-task execution ──────────────────────────────

(defn- rejected
  "The rejection keys spark reports for a routed card in `route`."
  [route]
  {:type :execution/unsupported-mode :route route :spawned false})

(defn- rejection-for
  "The :failed entry of a spark result for card-id, reduced to its rejection keys."
  [spark-result card-id]
  (some-> (first (filter #(= card-id (:task-id %)) (:failed spark-result)))
          (select-keys [:type :route :spawned])))

(defn- kanban-marked
  "Task ids the kanban port was asked to move, in order."
  []
  (keep (fn [[k cmd]] (when (= :kanban k) (:task_id cmd))) @events))

(deftest orchestrator-strike-rejects-only-the-routed-card
  (let [{:keys [cards]} (convert-plan!)
        routed (get cards "step-1")
        plain (get cards "step-2")
        spark (:spark-result (strike! {:spawn_mode "orchestrator"}))
        orchestrator (first (spawns))]
    (testing "one orchestrator is spawned, for the plain card only"
      (is (= 1 (count (spawns))))
      (is (= ["orchestrator" "ling" "mcp-first"] (:presets orchestrator)))
      (is (= [1] (mapv :task-count (:spawned spark))))
      (let [prompt (str (prompt-for (:name orchestrator)))]
        (is (str/includes? prompt plain))
        (is (not (str/includes? prompt routed))))
      (is (= [plain] (kanban-marked))))
    (testing "the routed card is reported as rejected, not bundled"
      (is (= (rejected :orchestrator) (rejection-for spark routed)))
      (is (nil? (rejection-for spark plain)))
      (is (nil? (position #(= :register (first %))))))))

(deftest spark-rejects-routed-cards-in-orchestrator-mode
  (let [{:keys [cards]} (convert-plan!)
        routed (get cards "step-1")
        plain (get cards "step-2")
        tasks (:tasks (forge-ops/survey {:directory dir}))
        routed-task (first (filter #(= routed (:id %)) tasks))]
    (is (= #{routed plain} (set (map :id tasks))))
    (testing "orchestrator over both cards"
      (reset! events [])
      (let [r (spawn/spark! {:directory dir :spawn-mode :orchestrator :tasks tasks} (spark-ports))]
        (is (= (rejected :orchestrator) (rejection-for r routed)))
        (is (nil? (rejection-for r plain)))))
    (testing "orchestrator over the routed card alone spawns nothing"
      (reset! events [])
      (let [r (spawn/spark! {:directory dir :spawn-mode :orchestrator :tasks [routed-task]} (spark-ports))]
        (is (empty? (spawns)))
        (is (= 0 (:count r)))
        (is (= [routed] (mapv :task-id (:failed r))))))
    (testing "the removed drone mode is refused"
      (let [ex (try (spawn/spark! {:directory dir :spawn-mode :drone :tasks tasks} (spark-ports))
                    nil
                    (catch clojure.lang.ExceptionInfo e e))]
        (is (= :execution/unsupported-mode (:type (ex-data ex))))
        (is (empty? (spawns)))))))

;; ── Slot order ──────────────────────────────────────────────────────────────

(deftest mixed-strike-gives-slots-in-survey-order
  (let [{:keys [cards]} (convert-plan!)
        routed (get cards "step-1")
        plain (get cards "step-2")
        r (strike! {:max_slots 1})]
    (is (= [routed plain] (mapv :id (get-in r [:survey-result :tasks])))
        "survey ranks the high-priority routed card first")
    (is (= [routed] (mapv :kanban_task_id (spawns)))
        "the only slot goes to the first-ranked card")))

(deftest mixed-spark-sends-every-card-to-a-ling
  (let [{:keys [cards]} (convert-plan!)
        routed (get cards "step-1")
        plain (get cards "step-2")
        tasks (:tasks (forge-ops/survey {:directory dir}))
        r (spawn/spark! {:directory dir :tasks tasks :max_slots 10} (spark-ports))]
    (is (= #{routed plain} (set (map :kanban_task_id (spawns)))))
    (is (= {:provider "ollama-compat" :model "qwen2.5:3b-instruct" :spawn_mode "hive-agent"}
           (select-keys (spawn-for routed) [:provider :model :spawn_mode])))
    (is (empty? (:failed r)))))
