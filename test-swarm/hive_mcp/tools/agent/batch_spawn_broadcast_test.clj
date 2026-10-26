(ns hive-mcp.tools.agent.batch-spawn-broadcast-test
  "Children batch-spawned by a coordinator are found by that coordinator's
   `agent broadcast scope=children` (kanban 20261026125418-562e92fe).

   Runs the real path: `agent batch-spawn` -> hive-mcp.batch.cli-adapter ->
   the swarm Batchable runner -> the spawn handler -> the swarm registry,
   with a stub headless backend (no ling process, no LLM), then hive-agent's
   broadcast recipient selection over the same registry. The wave executor
   is swapped for one that runs every op on a fresh thread, the shape the
   :bx/j pool has, so a parent stamped from thread-bound state would be lost.

   Also covers the :agent/spawn-gate seam (kanban 20261026125417-651b66e1):
   with hive-agent's gate registered, an unknown key refuses the whole batch
   before any ling reaches the backend."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.tools.consolidated.agent :as agent]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.swarm.datascript.queries :as queries]
            [hive-mcp.test.stub.headless-backend :as hb]
            [hive-agent.swarm.tools.agent.broadcast :as broadcast]
            [hive-agent.swarm.spawn-gate :as spawn-gate]
            [hive-test.isolation :as iso]
            [hive-mcp.isolation-methods]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (iso/with-isolations :swarm-ds))

(def ^:private caller "coordinator:2238473")

(def ^:private base
  {:type "ling" :cwd "/tmp/batch-spawn-broadcast" :spawn_mode "headless"
   :model "venice:test-model" :task "x"})

(defn- on-fresh-threads
  "A :bx/j stand-in: every op of a wave on a new raw Thread (no conveyed
   bindings of its own)."
  [wave-ops exec-one]
  (let [ps (mapv (fn [op]
                   (let [p (promise)]
                     (doto (Thread. #(deliver p (exec-one op))) (.start))
                     p))
                 wave-ops)]
    (mapv deref ps)))

(defn- with-extension [k f body]
  (let [prior (ext/get-extension k)]
    (try
      (if f (ext/register! k f) (ext/deregister! k))
      (body)
      (finally
        (if prior (ext/register! k prior) (ext/deregister! k))))))

(defn- spawned-ids
  "The agent ids a batch-spawn response reports, in op order."
  [resp]
  (let [body (json/read-str (:text resp) :key-fn keyword)]
    (mapv (fn [{:keys [result]}]
            (let [r (if (sequential? result) (first result) result)]
              (:agent-id (json/read-str (:text r) :key-fn keyword))))
          (:results body))))

(defn- broadcast-as
  "hive-agent's broadcast over the live registry; dispatch is recorded, not run."
  [caller-id]
  (let [sent (atom [])
        host {:all-slaves queries/get-all-slaves
              :dispatch   (fn [id _prompt]
                            (swap! sent conj id)
                            {:type "text" :text (json/write-str {:task-id (str "task-" id)})})}
        resp (broadcast/handle-broadcast* host {:prompt "status?" :scope "children"
                                                :_caller_id caller-id})]
    [resp @sent]))

(deftest batch-spawned-children-are-broadcast-children-of-their-coordinator
  (let [b (hb/->backend :test-headless)]
    (hb/with-backend :test-headless b
      (with-extension :bx/j on-fresh-threads
        (fn []
          (let [resp (agent/handle-agent
                      {:command "batch-spawn" :_caller_id caller
                       :operations [(assoc base :name "bsb-1")
                                    (assoc base :name "bsb-2")
                                    (assoc base :name "bsb-3")]})
                ids (spawned-ids resp)]
            (is (not (:isError resp)) (:text resp))
            (is (= 3 (count (hb/calls-of b :spawn!))) "every op reached the backend")
            (is (= 3 (count (remove nil? ids))) (:text resp))
            (testing "each child's registry row names the coordinator as parent"
              (doseq [id ids]
                (is (= caller (let [p (:slave/parent (queries/get-slave id))]
                                (if (map? p) (:slave/id p) p)))
                    (pr-str (queries/get-slave id)))))
            (testing "broadcast scope=children as that coordinator reaches all of them"
              (let [[out sent] (broadcast-as caller)]
                (is (not (:isError out)) (:text out))
                (is (= (set ids) (set sent)))))
            (testing "another coordinator's children scope does not see them"
              (let [[out sent] (broadcast-as "coordinator:999")]
                (is (:isError out))
                (is (empty? sent))
                (is (str/includes? (:text out) (str caller " (3)")))))))))))

(deftest the-spawn-gate-refuses-a-batch-before-any-ling-spawns
  (let [b (hb/->backend :test-headless)]
    (hb/with-backend :test-headless b
      (with-extension :agent/spawn-gate spawn-gate/gate
        (fn []
          (testing "an unknown key in one operation: nothing reaches the backend"
            (let [resp (agent/handle-agent
                        {:command "batch-spawn" :_caller_id caller
                         :operations [(assoc base :name "gate-1")
                                      (assoc base :name "gate-2" :spawn-mode "headless")]})]
              (is (:isError resp))
              (is (str/includes? (:text resp) "operations[1] `spawn-mode` (did you mean `spawn_mode`?)"))
              (is (empty? (hb/calls-of b :spawn!)))))
          (testing "a single spawn with an unknown key is refused too"
            (let [resp (agent/handle-agent (assoc base :command "spawn" :name "gate-3"
                                                  :_caller_id caller :dry_run true))]
              (is (:isError resp))
              (is (str/includes? (:text resp) "`dry_run`"))
              (is (empty? (hb/calls-of b :spawn!)))))
          (testing "a clean batch, host keys included, still spawns"
            (let [resp (agent/handle-agent
                        {:command "batch-spawn" :_caller_id caller :_caller_cwd "/tmp"
                         :directory "/tmp" :parallel true
                         :operations [(assoc base :name "gate-4") (assoc base :name "gate-5")]})]
              (is (not (:isError resp)) (:text resp))
              (is (= 2 (count (hb/calls-of b :spawn!)))))))))))

(deftest with-no-gate-contributed-core-spawns-as-before
  (let [b (hb/->backend :test-headless)]
    (hb/with-backend :test-headless b
      (with-extension :agent/spawn-gate nil
        (fn []
          (let [resp (agent/handle-agent (assoc base :command "spawn" :name "nogate-1"
                                                :_caller_id caller))]
            (is (not (:isError resp)) (:text resp))
            (is (= 1 (count (hb/calls-of b :spawn!))))))))))

(deftest the-gate-accepts-every-spawn-param-the-agent-tool-advertises
  ;; drift guard: a spawn param added to the schema but not to hive-agent's
  ;; accepted set would be refused as unknown.
  (let [props    (get-in agent/tool-def [:inputSchema :properties])
        tagged   (keep (fn [[k {:keys [description]}]]
                         (when (str/starts-with? (str description) "[spawn]") k))
                       props)
        op-desc  (get-in props ["operations" :description])
        listed   (some-> (re-find #"\{([^}]*)\}" op-desc) second
                         (str/split #",\s*"))
        expected (set (concat tagged listed ["type" "name" "cwd" "presets" "model"
                                             "provider" "task" "prompt" "spawn_mode"
                                             "parent" "kanban_task_id"]))]
    (is (seq tagged))
    (is (seq listed))
    (is (empty? (remove spawn-gate/spawn-keys expected))
        (str "advertised spawn params the gate would refuse: "
             (vec (remove spawn-gate/spawn-keys expected))))))
