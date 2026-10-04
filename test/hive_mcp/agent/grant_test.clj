(ns hive-mcp.agent.grant-test
  "Grant wiring: lineage resolution, the spawn decision, the dispatch gate
   and the spawn handler. The grant domain is the real hive-agent one when
   it is on the classpath, else a faithful stub of its contract."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.agent.grant :as grant]
            [hive-mcp.server.routes.middleware :as mw]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.agent.protocol :as proto]
            [hive-mcp.agent.executor :as executor]
            [hive-mcp.agent.registry]
            [hive-mcp.context.request :as proto-ctx]
            [hive-mcp.agent.openrouter :as llm-registry]))

;; =============================================================================
;; Domain: real when available, else a stub of the same contract
;; =============================================================================

(defn- set-sub? [held wanted]
  (or (= held :all) (and (not= wanted :all) (every? (fn [w] (some #(or (= % w) (str/starts-with? w (str % ":"))) held)) wanted))))

(def ^:private stub-domain
  {:from-wire (fn [m] (when m (into {} (map (fn [[k v]] [(keyword (name k)) (if (= v "all") :all (if (coll? v) (set v) v))])) m)))
   :->wire    (fn [g] (into {} (map (fn [[k v]] [k (if (set? v) (vec (sort v)) (if (keyword? v) (name v) v))])) g))
   :attenuate (fn [p q] (let [c (merge {:tools :all} p q)]
                          (if (set-sub? (get p :tools :all) (:tools c))
                            {:ok c}
                            {:error :grant/widening :message "wider on tools"})))
   :refusal   (fn [g {:keys [capability tool command]}]
                (case capability
                  :spawn (when (false? (:may-spawn g)) {:message "no spawn" :grant/missing {:capability :may-spawn}})
                  :tool  (let [e (if command (str tool ":" (name command)) tool)]
                           (when-not (set-sub? (get g :tools :all) #{e})
                             {:message (str "Grant does not permit tool " e) :grant/missing {:capability :tool :wanted e}}))))})

(defn- domain [] (or (grant/domain) stub-domain))

(defn- registry [rows] (fn [id] (get rows id)))

;; =============================================================================
;; Lineage
;; =============================================================================

(deftest lineage-resolution
  (let [rows {"coordinator:s1" {:slave/id "coordinator:s1" :slave/depth 0}
              "ling-a" {:slave/id "ling-a" :slave/depth 1 :slave/parent {:slave/id "coordinator:s1"}
                        :slave/grant {:tools ["memory"]}}
              "ling-b" {:slave/id "ling-b" :slave/depth 2 :slave/parent {:slave/id "ling-a"}}
              "loop-x" {:slave/id "loop-x" :slave/parent "loop-y"}
              "loop-y" {:slave/id "loop-y" :slave/parent "loop-x" :slave/grant {:tools ["kanban"]}}}
        get-slave (registry rows)]
    (testing "a session with no grant anywhere is unrestricted (nil)"
      (is (nil? (grant/recorded-grant get-slave "coordinator:s1")))
      (is (nil? (grant/recorded-grant get-slave "unknown")))
      (is (nil? (grant/recorded-grant get-slave nil))))
    (testing "a grant is inherited down the lineage, instance suffix stripped"
      (is (= {:tools ["memory"]} (grant/recorded-grant get-slave "ling-a")))
      (is (= {:tools ["memory"]} (grant/recorded-grant get-slave "ling-b:bb-123"))))
    (testing "cycles terminate"
      (is (= {:tools ["kanban"]} (grant/recorded-grant get-slave "loop-x"))))
    (testing "depth"
      (is (= 2 (grant/depth-of get-slave "ling-b")))
      (is (= 0 (grant/depth-of get-slave "coordinator:s1")))
      (is (= 0 (grant/depth-of get-slave "nobody"))))))

;; =============================================================================
;; Spawn decision
;; =============================================================================

(deftest child-grant-decisions
  (let [dom (domain)]
    (testing "no grant anywhere and none asked: nothing recorded (behaviour unchanged)"
      (is (= {:grant nil} (grant/child-grant nil nil nil 1)))
      (is (= {:grant nil} (grant/child-grant dom nil nil 1))))
    (testing "share: no request inherits the parent's grant"
      (let [{g :grant} (grant/child-grant dom {:tools ["memory"]} nil 2)]
        (is (= ["memory"] (:tools g)))))
    (testing "limit: a narrower request is recorded"
      (let [{g :grant} (grant/child-grant dom nil {"tools" ["memory:search"]} 1)]
        (is (= ["memory:search"] (:tools g)))))
    (testing "widen: refused, naming the dimension"
      (let [{:keys [refused]} (grant/child-grant dom {:tools ["memory"]} {"tools" ["agent"]} 2)]
        (is (string? refused))
        (is (str/includes? refused "tools"))))
    (testing "no domain loaded but a grant is involved: fail closed"
      (is (string? (:refused (grant/child-grant nil {:tools ["memory"]} nil 2))))
      (is (string? (:refused (grant/child-grant nil nil {"tools" ["memory"]} 1)))))))

;; =============================================================================
;; Dispatch gate
;; =============================================================================

(deftest call-refusal-gate
  (let [dom (domain)
        get-slave (registry {"ling-a" {:slave/id "ling-a" :slave/depth 1
                                       :slave/grant {:tools ["memory:search" "kanban"]}}})]
    (is (nil? (grant/call-refusal dom get-slave "coordinator:s1" "agent" "spawn"))
        "no recorded grant: unrestricted")
    (is (nil? (grant/call-refusal dom get-slave "ling-a" "memory" "search")))
    (is (nil? (grant/call-refusal dom get-slave "ling-a" "kanban" "list")))
    (is (nil? (grant/call-refusal dom get-slave "ling-a" "hivemind" "ask"))
        "the ask path is never gated")
    (let [no (grant/call-refusal dom get-slave "ling-a:inst" "memory" "add")]
      (is (str/includes? no "memory:add"))
      (is (str/includes? no "hivemind ask")))
    (is (string? (grant/call-refusal nil get-slave "ling-a" "memory" "search"))
        "grant recorded but no domain: fail closed")))

(deftest ask-path-through-swarm-tool
  ;; Measured live 2026-10-03: a ling holding {:tools ["memory:search"]} was
  ;; refused `swarm` + `hivemind ask`, because the table only spelled the
  ;; consolidated `hivemind:ask`, which no ling holds.
  (let [get-slave (registry {"ling-a" {:slave/id "ling-a" :slave/depth 1
                                       :slave/grant {:tools ["memory:search"]}}})
        refusal   (fn [tool command] (grant/call-refusal stub-domain get-slave "ling-a:inst" tool command))]
    (testing "the ask path is permitted on every route a caller reaches it by"
      (doseq [command ["hivemind ask" "hivemind messages" "hivemind respond"]]
        (is (nil? (refusal "swarm" command)) (str "swarm:" command)))
      (doseq [command ["ask" "messages" "respond"]]
        (is (nil? (refusal "hivemind" command)) (str "hivemind:" command))))
    (testing "the table is derived, one entry per route x command"
      (is (= #{"hivemind:ask" "hivemind:messages" "hivemind:respond"
               "swarm:hivemind ask" "swarm:hivemind messages" "swarm:hivemind respond"}
             grant/always-permitted)))
    (testing "the rest of the grant still binds"
      (is (str/includes? (refusal "swarm" "agent spawn") "swarm:agent spawn"))
      (is (str/includes? (refusal "swarm" "hivemind shout") "swarm:hivemind shout"))
      (is (str/includes? (refusal "memory" "add") "memory:add")))
    (testing "no grant recorded: unrestricted, as before"
      (is (nil? (grant/call-refusal stub-domain get-slave "coordinator:s1" "swarm" "agent spawn"))))))

(deftest middleware-gate
  (let [called (atom 0)
        h (mw/wrap-handler-grant (fn [_] (swap! called inc) [{:type "text" :text "ran"}]) "memory")]
    (binding [mw/*grant-get-slave* (registry {"ling-a" {:slave/id "ling-a" :slave/grant {:tools ["kanban"]}}})
              mw/*grant-domain* domain]
      (testing "unrestricted caller passes"
        (is (= "ran" (:text (first (h {:_caller_id "coordinator:s1" :command "add"}))))))
      (testing "limited caller refused before the handler runs"
        (let [[c] (h {:_caller_id "ling-a:bb" :command "add"})]
          (is (:isError c))
          (is (str/includes? (:text c) "memory:add"))
          (is (str/includes? (:text c) "hivemind ask"))))
      (testing "the model-written agent_id is never the identity the gate keys on"
        (let [[c] (h {:_caller_id "ling-a:bb" :agent_id "coordinator:s1" :command "add"})]
          (is (:isError c) "naming another agent does not lift the caller's grant"))
        (let [[c] (h {:agent_id "ling-a" :command "add"})]
          (is (= "ran" (:text c)) "agent_id alone does not make a call gated as that agent")))
      (is (= 2 @called)))))

(deftest executor-gate
  (let [called (atom 0)
        rows {"ling-a" {:slave/id "ling-a" :slave/grant {:tools ["kanban"]}}}
        dom (domain)]
    (with-redefs [hive-mcp.agent.registry/get-tool
                  (fn [_] {:handler (fn [_] (swap! called inc) "ran")})
                  grant/registry-get-slave (registry rows)
                  grant/domain (constantly dom)]
      (testing "no bound caller: ungated"
        (is (:success (executor/execute-tool "memory" {:command "add"}))))
      (testing "the bound caller's grant gates a delegated call"
        (let [res (proto-ctx/with-request-context {:caller-id "ling-a"}
                    (executor/execute-tool "memory" {:command "add" :agent_id "coordinator:s1"}))]
          (is (false? (:success res)))
          (is (str/includes? (:error res) "hivemind ask")))
        (is (:success (proto-ctx/with-request-context {:caller-id "ling-a"}
                        (executor/execute-tool "kanban" {:command "list"})))))
      (is (= 2 @called)))))

;; =============================================================================
;; Spawn handler
;; =============================================================================

(defn- spawn-with [rows params]
  (let [captured (atom nil)]
    (with-redefs [proto/spawn! (fn [_ling opts] (reset! captured opts) "ling-new")
                  llm-registry/resolve-provider-model (fn [_] {:provider :openrouter :model "test/model"})
                  spawn/provider-preflight-refusal (constantly nil)]
      (binding [spawn/*get-slave* (registry rows)
                spawn/*grant-domain* domain
                spawn/*editor-reachable?* (constantly true)]
        (let [res (spawn/handle-spawn (merge {:type "ling" :cwd "/tmp" :spawn_mode "headless" :project_id "grant-test"} params))]
          {:res res :opts @captured})))))

(defn- body [res]
  (let [t (:text res)]
    (is (not (:isError res)) (str "spawn failed: " t))
    (when-not (:isError res) (json/read-str t :key-fn keyword))))

(deftest spawn-handler-grant
  (testing "no grant: the spawn opts and response carry none (unchanged behaviour)"
    (let [{:keys [res opts]} (spawn-with {} {:_caller_id "coordinator:s1"})]
      (is (nil? (:grant opts)))
      (is (nil? (:grant (body res))))))
  (testing "a requested grant is passed to the registry write and returned"
    (let [{:keys [res opts]} (spawn-with {} {:_caller_id "coordinator:s1" :grant {"tools" ["memory"]}})]
      (is (= ["memory"] (get-in opts [:grant :tools])))
      (is (= ["memory"] (get-in (body res) [:grant :tools])))))
  (testing "a widening request is refused and nothing is spawned"
    (let [rows {"ling-a" {:slave/id "ling-a" :slave/depth 1 :slave/grant {:tools ["memory"]}}}
          {:keys [res opts]} (spawn-with rows {:_caller_id "ling-a:bb" :grant {"tools" ["agent"]}})]
      (is (nil? opts))
      (is (:isError res))
      (is (str/includes? (:text res) "tools")))))

(deftest spawn-handler-grant-as-string
  ;; Measured live 2026-10-02: a client whose cached schema predates `grant`
  ;; sends it as text, and the spawn died on "java.lang.Character cannot be
  ;; cast to java.util.Map$Entry".
  (testing "a JSON-object string is parsed and honoured like the map"
    (let [{:keys [res opts]} (spawn-with {} {:_caller_id "coordinator:s1"
                                             :grant "{\"tools\": [\"memory\"], \"may_spawn\": false}"})]
      (is (= ["memory"] (get-in opts [:grant :tools])))
      (is (= ["memory"] (get-in (body res) [:grant :tools])))))
  (testing "a blank string shares, as no grant does"
    (let [{:keys [opts]} (spawn-with {} {:_caller_id "coordinator:s1" :grant "  "})]
      (is (nil? (:grant opts)))))
  (doseq [[what bad] [["not JSON" "tools=memory"]
                      ["a JSON array" "[\"memory\"]"]
                      ["a JSON scalar" "42"]
                      ["a number" 42]
                      ["a vector" ["memory"]]]]
    (testing (str "anything else is refused by message, never a ClassCastException: " what)
      (let [{:keys [res opts]} (spawn-with {} {:_caller_id "coordinator:s1" :grant bad})]
        (is (nil? opts) "nothing is spawned")
        (is (:isError res))
        (is (str/includes? (:text res) "`grant` must be a JSON object"))
        (is (not (str/includes? (:text res) "ClassCast")))))))
