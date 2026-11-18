(ns hive-mcp.await.resumers.hive-agent-run-test
  "The :hive-agent-run resumer runs the same port conformance cases as the
   registry's stub, with the spawn path replaced by a recording double; it
   resumes a parked run through the registry on a manual fire, and it treats
   a deferred or refused spawn as a FAILED resume instead of a success (the
   silent-loss shape review B3 found)."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumer-conformance :as conformance]
            [hive-mcp.await.resumers.hive-agent-run :as har]
            [hive-mcp.await.sources.manual :as manual]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private spawns (atom []))

(def ^:private spawn-answer (atom (fn [_p] {:type "text" :text "{}"})))

(def owner {:kind :hive-agent-run :run-id "run-abc" :cwd "/tmp" :ling-id "ling-7"
            :model "sonnet" :parent "coord-1" :spawned-by "coord-1"
            :kanban-task-id "card-1" :sandbox true :presets ["ling"]
            :token-budget 40000 :time-budget-ms 900000 :max-budget-usd 2.5
            :grant {:tools ["bash"]}})

(use-fixtures :each
  (fn [t]
    (reset! spawns [])
    (reset! spawn-answer (fn [_p] {:type "text" :text "{}"}))
    (reset! har/spawn-fn (fn [p] (swap! spawns conj p) (@spawn-answer p)))
    (try (t) (finally (reset! har/spawn-fn nil)))))

(defn- drain-spawns!
  "Recorded spawns as resume requests (the prompt the run resumes with)."
  []
  (let [[old _] (reset-vals! spawns [])]
    (mapv (fn [p] {:prompt (get-in p [:resume :prompt]) :spawn p}) old)))

(defn- temp-path []
  (let [d (.toFile (java.nio.file.Files/createTempDirectory
                    "await-har" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (str (io/file d "registry.edn"))))

(deftest real-resumer-conforms
  (conformance/run-cases! owner drain-spawns!))

(deftest spawn-params-resume-the-chat-point-and-inherit-authority
  (let [p (har/spawn-params owner "01234567-89ab" "go")]
    (testing "the run is continued at its chat point, under the same identity"
      (is (= {:type "ling" :name "ling-7-w-01234567-1" :cwd "/tmp" :spawn_mode "hive-agent"
              :resume {:run_id "run-abc" :prompt "go"} :model "sonnet" :parent "coord-1"
              :spawned_by "coord-1" :kanban_task_id "card-1" :sandbox true
              :presets ["ling"] :token_budget 40000 :time_budget_ms 900000
              :max_budget_usd 2.5 :grant {:tools ["bash"]}}
             p)))
    (testing "a re-driven attempt gets a different name, so it cannot collide"
      (is (not= (:name p) (:name (har/spawn-params owner "01234567-89ab" "go" 2)))))))

(deftest owner-without-a-cwd-is-refused
  (is (= :await/invalid-owner
         (:error (ex-data (try (ports/open-resumer {:kind :hive-agent-run :run-id "r"})
                               (catch clojure.lang.ExceptionInfo e e)))))))

(deftest owner-without-a-run-id-is-unbound-not-invalid
  (testing "an unarmed park has no run id yet; the refusal says so and names arming"
    (let [e (try (ports/open-resumer {:kind :hive-agent-run :cwd "/tmp"})
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :await/owner-unbound (:error (ex-data e))))
      (is (.contains ^String (.getMessage e) "Arm")))))

(deftest parked-run-resumes-on-fire-through-registry
  (let [reg (registry/make-registry (temp-path))]
    (try
      (registry/init! reg)
      (registry/create! reg {:id "parked-1" :owner (dissoc owner :run-id)
                             :prompt "Review the CI verdict."
                             :subscription {:type :manual :filter {:sha "abc"}}})
      (manual/fire! {:sha "zzz"} "parked-1")
      (is (empty? @spawns) "a non-matching fire does nothing")
      (manual/fire! {:sha "abc" :verdict "green"} "parked-1")
      (is (empty? @spawns) "a matching fire while pending only records the wake")
      (registry/arm! reg "parked-1" "run-abc")
      (let [end (+ (System/currentTimeMillis) 4000)]
        (loop []
          (when (and (empty? @spawns) (< (System/currentTimeMillis) end))
            (Thread/sleep 20) (recur))))
      (manual/fire! {:sha "abc" :verdict "green"} "parked-1")
      (testing "exactly one successor resumes the chat point with the payload"
        (is (= 1 (count @spawns)))
        (let [p (first @spawns)]
          (is (= "run-abc" (get-in p [:resume :run_id])) "arm bound the parked run id")
          (is (.startsWith ^String (get-in p [:resume :prompt]) "Review the CI verdict."))
          (is (.contains ^String (get-in p [:resume :prompt]) "\"green\""))
          (is (= "ling-7-w-parked-1-1" (:name p)))))
      (is (= {:ok? true :value {:spawned "ling-7-w-parked-1-1" :run-id "run-abc" :attempt 1}
              :attempt 1}
             (dissoc (registry/resume-result reg "parked-1") :give-up?)))
      (finally (registry/shutdown! reg)))))

(deftest deferred-spawn-is-a-failed-resume-not-a-success
  (testing "{:success false :deferred true} (heap pressure) must not look like a resume"
    (reset! spawn-answer (fn [_p] {:success false :deferred true}))
    (let [reg (registry/make-registry (temp-path) {:max-attempts 2})]
      (try
        (registry/init! reg)
        (registry/create! reg {:id "def-1" :owner owner
                               :subscription {:type :manual}})
        (registry/arm! reg "def-1" "run-abc")
        (manual/fire! {} "def-1")
        (let [end (+ (System/currentTimeMillis) 6000)]
          (loop []
            (when (and (not (true? (:give-up? (registry/resume-result reg "def-1"))))
                       (< (System/currentTimeMillis) end))
              (Thread/sleep 20) (recur))))
        (is (true? (:give-up? (registry/resume-result reg "def-1")))
            "a deferred spawn gives up after :max-attempts, visibly")
        (is (= :waking (:status (registry/lookup reg "def-1")))
            "the awaiter is never marked woken on a failed resume")
        (is (false? (:ok? (registry/resume-result reg "def-1"))))
        (finally (registry/shutdown! reg))))))

(deftest refused-spawn-is-recorded-with-its-reason
  (reset! spawn-answer (fn [_p] {:isError true :content [{:text "spawn refused: no slots"}]}))
  (let [reg (registry/make-registry (temp-path) {:max-attempts 1})]
    (try
      (registry/init! reg)
      (registry/create! reg {:id "ref-1" :owner owner :subscription {:type :manual}})
      (registry/arm! reg "ref-1" "run-abc")
      (manual/fire! {} "ref-1")
      (let [end (+ (System/currentTimeMillis) 3000)]
        (loop []
          (when (and (nil? (registry/resume-result reg "ref-1"))
                     (< (System/currentTimeMillis) end))
            (Thread/sleep 20) (recur))))
      (is (.contains ^String (str (:error (registry/resume-result reg "ref-1"))) "spawn refused"))
      (finally (registry/shutdown! reg)))))

(deftest missing-cwd-is-refused-at-resume
  (let [reg (registry/make-registry (temp-path) {:max-attempts 1})]
    (try
      (registry/init! reg)
      (registry/create! reg {:id "cwd-1" :owner (assoc owner :cwd "/no/such/worktree")
                             :subscription {:type :manual}})
      (registry/arm! reg "cwd-1" "run-abc")
      (manual/fire! {} "cwd-1")
      (let [end (+ (System/currentTimeMillis) 3000)]
        (loop []
          (when (and (nil? (registry/resume-result reg "cwd-1"))
                     (< (System/currentTimeMillis) end))
            (Thread/sleep 20) (recur))))
      (is (.contains ^String (str (:error (registry/resume-result reg "cwd-1")))
                     "cwd no longer exists"))
      (is (empty? @spawns) "nothing is spawned into a directory that is gone")
      (finally (registry/shutdown! reg)))))
