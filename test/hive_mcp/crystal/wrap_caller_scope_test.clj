(ns hive-mcp.crystal.wrap-caller-scope-test
  "Kanban 20261002152638-34671747 (WRAP-SCOPE), measured 2026-10-02 from a
   ClojureWasm coordinator whose whoami answered coordinator:2914886.

   Defect 1: `workflow wrap` with no agent_id answered agent-id `coordinator`
   and crystallized as the bare shared role, because the wrap path resolved
   the caller with (or agent_id current-agent-id env \"coordinator\") and never
   read the transport's caller id, which is what whoami reads.

   Defect 2: the synthesis written under scope:project:ClojureWasm described
   other projects' work. The harvest read every author's hivemind shouts, the
   process-wide recall buffer and every unscoped created id, so whoever wrapped
   summarized the whole box.

   Every test here enters at a handler boundary with a request context for
   coordinator:AAA in project A while coordinator:BBB records activity in
   project B."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.data.json :as json]
            [hive-mcp.agent.context :as ctx]
            [hive-mcp.channel.piggyback :as piggyback]
            [hive-mcp.crystal.hooks :as hooks]
            [hive-mcp.crystal.recall :as recall]
            [hive-mcp.crystal.synthesis :as synthesis]
            [hive-mcp.crystal.harvest.collect :as collect]
            [hive-mcp.swarm.datascript :as ds]
            [hive-mcp.tools.consolidated.session :as session]
            [hive-mcp.tools.crystal :as crystal]
            [hive-mcp.tools.memory.scope :as scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private dir-a (str (System/getProperty "java.io.tmpdir") "/wrap-scope-proj-a"))
(def ^:private dir-b (str (System/getProperty "java.io.tmpdir") "/wrap-scope-proj-b"))

(defn- project-of [d]
  (cond (= d dir-a) "proj-a"
        (= d dir-b) "proj-b"
        :else "global"))

(defn- reset-buffers! []
  (piggyback/clear-backbone-buffer!)
  (recall/flush-recall-buffer!)
  (recall/flush-created-ids!))

(use-fixtures :each
  (fn [t]
    (.mkdirs (java.io.File. ^String dir-a))
    (.mkdirs (java.io.File. ^String dir-b))
    (reset-buffers!)
    (try (t) (finally (reset-buffers!)))))

(defn- as-caller
  "Run f inside the request context a transport-stamped call would bind."
  [caller-id directory f]
  (ctx/with-request-context {:agent-id nil
                             :caller-id caller-id
                             :project-id (project-of directory)
                             :directory directory}
    (f)))

(defn- shout! [agent-id project-id msg]
  (piggyback/buffer-backbone-event! {:agent-id agent-id
                                     :event-type :completed
                                     :message msg
                                     :timestamp (System/currentTimeMillis)
                                     :project-id project-id}))

(defn- parse [r] (json/read-str (:text r) :key-fn keyword))

;; =============================================================================
;; Defect 1: identity
;; =============================================================================

(deftest wrap-resolves-the-caller-the-way-whoami-does
  (with-redefs [scope/get-current-project-id (fn ([] "global") ([d] (project-of d)))]
    (testing "whoami answers the caller id: the reference the wrap must match"
      (is (= "coordinator:AAA"
             (:agent-id (parse (as-caller "coordinator:AAA" dir-a
                                          #(session/handle-whoami {:directory dir-a})))))))

    (testing "session wrap with no agent_id reports and crystallizes as the caller"
      (let [seen (promise)]
        (with-redefs [crystal/handle-wrap-crystallize
                      (fn [params] (deliver seen params) {:type "text" :text "{}"})]
          (let [r (parse (as-caller "coordinator:AAA" dir-a
                                    #(session/handle-wrap {:directory dir-a})))]
            (is (= "coordinator:AAA" (:agent-id r))
                "the response must name the session, not the shared role")
            (is (= "coordinator:AAA" (:agent_id (deref seen 2000 nil)))
                "the background crystallization must receive the resolved id")))))

    (testing "the bare coordinator role passed explicitly resolves to the caller"
      (let [seen (promise)]
        (with-redefs [crystal/handle-wrap-crystallize
                      (fn [params] (deliver seen params) {:type "text" :text "{}"})]
          (let [r (parse (as-caller "coordinator:AAA" dir-a
                                    #(session/handle-wrap {:directory dir-a
                                                           :agent_id "coordinator"})))]
            (is (= "coordinator:AAA" (:agent-id r)))
            (is (= "coordinator:AAA" (:agent_id (deref seen 2000 nil))))))))

    (testing "a specific agent id still wins"
      (let [done (promise)] (with-redefs [crystal/handle-wrap-crystallize (fn [p] (deliver done p) {:type "text", :text "{}"})] (is (= "ling-7"
               (:agent-id (parse (as-caller "coordinator:AAA" dir-a
                                            #(session/handle-wrap {:directory dir-a
                                                                   :agent_id "ling-7"})))))) (is (= "ling-7" (:agent_id (deref done 2000 nil))) "joined here so the background wrap cannot outlive this redef"))))

    (testing "handle-wrap-crystallize (session complete, :wrap-crystallize effect) harvests as the caller"
      (let [seen (atom nil)]
        (with-redefs [collect/harvest-all (fn [opts] (reset! seen opts) {:summary {}})]
          (as-caller "coordinator:AAA" dir-a
                     #(crystal/handle-wrap-crystallize {:directory dir-a}))
          (is (= "coordinator:AAA" (:agent-id @seen)))
          (as-caller "coordinator:AAA" dir-a
                     #(crystal/handle-wrap-crystallize {:directory dir-a
                                                        :agent_id "coordinator"}))
          (is (= "coordinator:AAA" (:agent-id @seen))))))))

;; =============================================================================
;; Defect 2: scope
;; =============================================================================

(defn- record-two-sessions!
  "coordinator:AAA works in project A with one ling; coordinator:BBB works in
   project B and also shouts into global and into A. Recorded through the real
   buffers, the way live requests record them."
  []
  (shout! "coordinator:AAA" "proj-a" "A: round-6 cljw work")
  (shout! "ling-a1" "proj-a" "A's ling: zig build green")
  (shout! "coordinator:BBB" "proj-b" "B: Ed25519 spawn identity")
  (shout! "coordinator:BBB" "global" "B: branding rollout")
  (shout! "coordinator:BBB" "proj-a" "B wandered into A")
  (as-caller "coordinator:AAA" dir-a
             #(hooks/on-memory-accessed {:entry-ids ["mem-read-by-a"] :source "query"}))
  (as-caller "coordinator:BBB" dir-b
             #(hooks/on-memory-accessed {:entry-ids ["mem-read-by-b"] :source "query"}))
  (recall/register-created-id! "mem-made-by-b" "proj-b" "note")
  (recall/register-created-id! "mem-unscoped" nil "note"))

(def ^:private slaves
  [{:slave/id "ling-a1" :slave/parent-id "coordinator:AAA"}
   {:slave/id "ling-b1" :slave/parent-id "coordinator:BBB"}])

(deftest wrap-harvest-holds-only-the-callers-project-and-lineage
  (let [captured (atom nil)]
    (with-redefs [scope/get-current-project-id (fn ([] "global") ([d] (project-of d)))
                  ds/get-all-slaves (fn [& _] slaves)
                  synthesis/synthesize (fn [h] (reset! captured h) {:summary-id "sum-1"})]
      (record-two-sessions!)
      (recall/register-created-id! "mem-made-by-a" "proj-a" "note")
      (let [r (parse (as-caller "coordinator:AAA" dir-a
                                #(crystal/handle-wrap-crystallize {:directory dir-a})))
            h @captured
            authors (set (map :a (:hivemind-messages h)))]
        (is (= "sum-1" (:summary-id r)))
        (testing "hivemind: the caller and its own lings only"
          (is (= #{"coordinator:AAA" "ling-a1"} authors))
          (is (not-any? #(re-find #"Ed25519|branding|wandered" (str (:m %)))
                        (:hivemind-messages h))))
        (testing "memory reads: only those made in the caller's project by its lineage"
          (is (= #{"mem-read-by-a"} (set (keys (:recalls h)))))
          (is (= ["mem-read-by-a"] (vec (:memory-ids-accessed h)))))
        (testing "memory writes: only entries scoped to the caller's project"
          (is (= #{"mem-made-by-a"} (set (map :id (:memory-ids-created h))))))
        (testing "the harvest names its scope for the synthesizer's own filter"
          (is (= "proj-a" (:project-id h)))
          (is (= #{"coordinator:AAA" "ling-a1"} (set (:coordination-agent-ids h)))))))))

(deftest wrap-writes-nothing-when-only-someone-elses-activity-exists
  (let [called (atom false)]
    (with-redefs [scope/get-current-project-id (fn ([] "global") ([d] (project-of d)))
                  ds/get-all-slaves (fn [& _] slaves)
                  synthesis/synthesize (fn [_] (reset! called true) {:summary-id "leak"})]
      (record-two-sessions!)
      (let [r (parse (as-caller "coordinator:CCC" dir-a
                                #(crystal/handle-wrap-crystallize {:directory dir-a})))]
        (is (false? @called)
            "a wrap with no activity of its own must not summarize another session")
        (is (true? (:skipped r)))
        (is (= "no-activity" (:reason r)))
        (is (= {:project-id "proj-a" :agent-id "coordinator:CCC"} (:scope r))
            "the skip says whose activity it looked for")))))

(deftest lineage-agent-ids-walks-parents-and-refuses-the-bare-role
  (is (= #{"c:1" "l-1" "l-2"}
         (collect/lineage-agent-ids "c:1" [{:slave/id "l-1" :slave/parent-id "c:1"}
                                           {:slave/id "l-2" :slave/parent {:slave/id "l-1"}}
                                           {:slave/id "l-x" :slave/parent-id "c:2"}])))
  (is (= #{"c:1"} (collect/lineage-agent-ids "c:1" [{:slave/id "c:1" :slave/parent-id "c:1"}]))
      "a self-parent does not loop")
  (is (nil? (collect/lineage-agent-ids "coordinator" []))
      "the shared role names no single session, so it owns no shouts")
  (is (nil? (collect/lineage-agent-ids nil []))))
