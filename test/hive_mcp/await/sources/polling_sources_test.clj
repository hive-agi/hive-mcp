(ns hive-mcp.await.sources.polling-sources-test
  "The polling await sources (card RUN-LEDGER-4): :sql-predicate (the
   priority one), :k8s-log-regex and :prom-predicate.

   Every boundary is a stub injected the way production injects it: the
   scheduler, clock, SQL runner, secret resolver, process runner and HTTP
   client through each namespace's dynamic port var, bound with `binding`
   around create!; the journal through the registry's :journal option. No
   with-redefs. The stub scheduler runs a tick only when the test says so,
   so every poll is deterministic."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumers.session]
            [hive-mcp.await.sources.boundary :as b]
            [hive-mcp.await.sources.k8s-log-regex :as k8s]
            [hive-mcp.await.sources.poll :as poll]
            [hive-mcp.await.sources.prom-predicate :as prom]
            [hive-mcp.await.sources.sql-predicate :as sql]
            [hive-mcp.tools.consolidated.await :as await-tool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Stubs
;; =============================================================================

(defn- manual-scheduler
  "An IPollScheduler that records each schedule; (tick!) runs every live
   one once. Cancelled schedules never run again."
  []
  (let [jobs (atom {}) n (atom 0)]
    {:jobs jobs
     :tick! (fn [] (doseq [[_ f] @jobs] (f)))
     :port (reify b/IPollScheduler
             (schedule-repeating! [_ f interval-ms]
               (let [id (swap! n inc)]
                 (swap! jobs assoc id f)
                 (swap! jobs vary-meta assoc id interval-ms)
                 (fn [] (swap! jobs dissoc id)))))}))

(defn- clock [t] (reify b/IClock (now-ms [_] @t)))

(defn- sql-runner
  "An ISqlRunner answering (answers) in turn (the last repeats); every
   request is recorded."
  [requests answers]
  (let [i (atom -1)]
    (reify b/ISqlRunner
      (run-select! [_ req]
        (swap! requests conj req)
        (let [a (nth answers (min (swap! i inc) (dec (count answers))))]
          (if (instance? Throwable a) (throw a) a))))))

(def secrets
  (reify b/ISecretResolver
    (resolve-secret! [_ ref]
      (if (= ref "pass:sisf/staging-pg-url")
        "postgres://u:s3cret@db:5432/sisf"
        (throw (ex-info "missing" {:error :await/secret-missing}))))))

(defn- journal [entries]
  (reify ports/IWakeJournal
    (append-wake! [_ e] (swap! entries conj e) e)))

(defn- temp-path []
  (str (io/file (.toFile (java.nio.file.Files/createTempDirectory
                          "await-poll" (make-array java.nio.file.attribute.FileAttribute 0)))
                "registry.edn")))

(defn- wait-until [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) end)) v (do (Thread/sleep 10) (recur)))))))

(defmacro ^:private with-reg [[reg entries t] & body]
  `(let [~entries (atom [])
         ~t (atom 1000000)
         ~reg (registry/make-registry (temp-path) {:journal (journal ~entries)
                                                   :now-fn (fn [] @~t)})]
     (registry/init! ~reg)
     (try ~@body (finally (registry/shutdown! ~reg)))))

(def reference-spec
  {:connection_ref "pass:sisf/staging-pg-url"
   :query "select 1 from contributor.contributors where id=$1 and address_complement=$2"
   :params [42 "Apto 3"]
   :predicate {:kind "exists"}})

(defn- sql-request [id & {:as extra}]
  (merge {:id id :owner {:kind :journal :label "landed?"}
          :subscription {:type :sql-predicate :poll-ms 300000
                         :spec (poll/kw-keys reference-spec)}
          :timeout-ms (* 2 60 60 1000)}
         extra))

;; =============================================================================
;; sql-predicate
;; =============================================================================

(deftest the-sources-are-registered
  (is (every? (set (ports/known-sources)) [:manual :sql-predicate :k8s-log-regex :prom-predicate])))

(deftest sql-predicate-reference-case-wakes-when-the-row-lands
  (with-reg [reg entries t]
    (let [{:keys [port tick!]} (manual-scheduler)
          reqs (atom [])]
      (binding [poll/*scheduler* port
                poll/*clock* (clock t)
                sql/*runner* (sql-runner reqs [{:rows [] :truncated? false}
                                               {:rows [{:?column? 1}] :truncated? false}])
                sql/*secrets* secrets]
        (is (= :armed (:status (registry/create! reg (sql-request "q1"))))))
      (testing "first poll: no row yet, nothing wakes"
        (tick!)
        (is (= :armed (:status (registry/lookup reg "q1")))))
      (testing "second poll: the row exists, the awaiter wakes with the observation"
        (tick!)
        (is (wait-until 2000 #(= :woken (:status (registry/lookup reg "q1")))))
        (let [ev (get-in (registry/lookup reg "q1") [:wake :event])]
          (is (= :sql-predicate (:type ev)))
          (is (= true (get-in ev [:data :observed])))
          (is (= 2 (get-in ev [:data :poll])))))
      (testing "the query was bound, read-only bounded, and the secret stayed out"
        (is (= {:url "postgres://u:s3cret@db:5432/sisf"
                :sql "select 1 from contributor.contributors where id=? and address_complement=?"
                :params [42 "Apto 3"] :row-cap 100 :statement-timeout-ms 5000}
               (first @reqs)))
        (is (not (str/includes? (slurp (:path reg)) "s3cret")) "no credential is persisted"))
      (testing "the source stopped: a later tick queries nothing"
        (let [n (count @reqs)] (tick!) (is (= n (count @reqs)))))
      (is (wait-until 2000 #(seq @entries)) "the registry journaled the wake"))))

(deftest sql-predicate-true-on-the-first-poll-wakes
  (testing "a predicate already true at the first poll (emitted on the scheduler right after open-source)"
    (with-reg [reg _entries t]
      (let [reqs (atom [])
            sched (reify b/IPollScheduler
                    ;; runs the first tick synchronously, inside open-source
                    (schedule-repeating! [_ f _] (f) (fn [] nil)))]
        (binding [poll/*scheduler* sched
                  poll/*clock* (clock t)
                  sql/*runner* (sql-runner reqs [{:rows [{:n 1}] :truncated? false}])
                  sql/*secrets* secrets]
          (registry/create! reg (sql-request "first")))
        (is (wait-until 2000 #(= :woken (:status (registry/lookup reg "first")))))
        (is (= 1 (count @reqs)))))))

(deftest sql-predicate-count-and-scalar
  (let [parsed (fn [p] (sql/parse {:spec (assoc (poll/kw-keys reference-spec) :predicate p)}))
        rows   {:rows [{:n 3 :s "done"} {:n 4}] :truncated? false}]
    (is (:match? (sql/evaluate (parsed {:kind "count" :op ">=" :value 2}) rows)))
    (is (not (:match? (sql/evaluate (parsed {:kind "count" :op ">=" :value 3}) rows))))
    (is (:match? (sql/evaluate (parsed {:kind "scalar" :op ">" :value 2}) rows)) "first column of the first row")
    (is (:match? (sql/evaluate (parsed {:kind "scalar" :op "=" :value "done" :column "s"}) rows)))
    (is (not (:match? (sql/evaluate (parsed {:kind "scalar" :op "=" :value 1}) {:rows []}))) "no row never holds")))

(deftest sql-predicate-refuses-before-recording
  (with-reg [reg _entries _t]
    (doseq [[why req] {"no deadline"      (dissoc (sql-request "r1") :timeout-ms)
                       "poll too fast"    (assoc-in (sql-request "r2") [:subscription :poll-ms] 1000)
                       "inline url"       (assoc-in (sql-request "r3") [:subscription :spec :connection-ref]
                                                    "postgres://u:pw@db/sisf")
                       "a write"          (assoc-in (sql-request "r4") [:subscription :spec :query]
                                                    "delete from contributor.contributors where id=$1 and x=$2")
                       "deadline too far" (assoc (sql-request "r5") :timeout-ms (* 72 3600 1000))
                       "row cap too big"  (assoc-in (sql-request "r6") [:subscription :spec :row-cap] 100000)}]
      (let [e (try (registry/create! reg req) nil (catch clojure.lang.ExceptionInfo e (ex-data e)))]
        (is (= :await/invalid-subscription (:error e)) why)
        (is (nil? (registry/lookup reg (:id req))) (str why ": nothing recorded"))))))

(deftest sql-predicate-failing-poll-goes-on-and-cancel-stops
  (with-reg [reg _entries t]
    (let [{:keys [port tick! jobs]} (manual-scheduler)
          reqs (atom [])]
      (binding [poll/*scheduler* port
                poll/*clock* (clock t)
                sql/*runner* (sql-runner reqs [(ex-info "db down" {}) {:rows [] :truncated? false}])
                sql/*secrets* secrets]
        (registry/create! reg (sql-request "c1")))
      (tick!) (tick!)
      (is (= 2 (count @reqs)) "a failed poll is retried at the next interval")
      (is (= :armed (:status (registry/lookup reg "c1"))))
      (registry/cancel! reg "c1")
      (is (empty? @jobs) "cancel stops the schedule")
      (tick!)
      (is (= 2 (count @reqs))))))

(deftest polling-stops-at-the-deadline-and-the-registry-times-out
  (with-reg [reg entries t]
    (let [{:keys [port tick! jobs]} (manual-scheduler)
          reqs (atom [])]
      (binding [poll/*scheduler* port
                poll/*clock* (clock t)
                sql/*runner* (sql-runner reqs [{:rows [] :truncated? false}])
                sql/*secrets* secrets]
        (registry/create! reg (sql-request "d1" :timeout-ms 60000)))
      (tick!)
      (swap! t + 60000)
      (tick!)
      (is (= 1 (count @reqs)) "no query once the deadline is reached")
      (is (empty? @jobs))
      (registry/tick! reg)
      (is (wait-until 2000 #(= :timeout (get-in (registry/lookup reg "d1") [:wake :reason])))))))

;; =============================================================================
;; k8s-log-regex
;; =============================================================================

(def k8s-sub
  {:type :k8s-log-regex
   :spec {:namespace "sisf" :selector "app=sisf-sync" :container "sync"
          :context "blw" :regex "Scheduled sync completed"}})

(deftest k8s-argv-is-a-bounded-poll-with-no-shell
  (let [p (k8s/parse k8s-sub)]
    (is (= ["kubectl" "--context" "blw" "logs" "-n" "sisf" "-l" "app=sisf-sync" "--prefix"
            "--max-log-requests" "10" "-c" "sync" "--timestamps"
            "--since-time=1970-01-01T00:16:40Z" "--tail=2000"]
           (k8s/argv p (k8s/since-time p 1000000))))))

(deftest k8s-refuses-bad-names
  (doseq [spec [{:namespace "--all" :pod "x" :regex "a"}
                {:namespace "sisf" :regex "a"}
                {:namespace "sisf" :pod "a" :selector "b=c" :regex "a"}
                {:namespace "sisf" :pod "a" :regex "("}]]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"k8s-log-regex"
                          (k8s/parse {:spec spec})) (pr-str spec))))

(deftest k8s-wakes-on-the-first-matching-line-and-carries-it
  (with-reg [reg _entries t]
    (let [{:keys [port tick!]} (manual-scheduler)
          calls (atom [])
          outs  (atom [{:exit 1 :out "" :err "pod not ready"}
                       {:exit 0 :out "2026-11-25T10:00:00Z [sync] starting\n" :err ""}
                       {:exit 0 :out (str "2026-11-25T10:00:01Z [sync] starting\n"
                                          "2026-11-25T10:05:00Z [sync] Scheduled sync completed rows=12\n"
                                          "2026-11-25T10:06:00Z [sync] Scheduled sync completed rows=0\n")
                        :err ""}])
          proc  (reify b/IProcessRunner
                  (run-process! [_ argv opts]
                    (swap! calls conj [argv opts])
                    (let [o (first @outs)] (when (next @outs) (swap! outs rest)) o)))]
      (binding [poll/*scheduler* port poll/*clock* (clock t) k8s/*process* proc]
        (registry/create! reg {:id "k1" :owner {:kind :journal} :timeout-ms 3600000
                               :subscription k8s-sub}))
      (tick!) (tick!)
      (is (= :armed (:status (registry/lookup reg "k1"))) "a failed poll and a non-match do not wake")
      (tick!)
      (is (wait-until 2000 #(= :woken (:status (registry/lookup reg "k1")))))
      (is (= "2026-11-25T10:05:00Z [sync] Scheduled sync completed rows=12"
             (get-in (registry/lookup reg "k1") [:wake :event :data :line])))
      (is (= {:timeout-ms k8s/kubectl-timeout-ms :max-bytes k8s/max-output-bytes}
             (second (first @calls)))))))

;; =============================================================================
;; prom-predicate
;; =============================================================================

(defn- prom-body [result-type result]
  (str "{\"status\":\"success\",\"data\":{\"resultType\":\"" result-type "\",\"result\":" result "}}"))

(deftest prom-evaluates-vectors-and-scalars
  (let [p (prom/parse {:spec {:query "up" :op ">=" :threshold 1}})]
    (is (:match? (prom/evaluate p {:status 200 :body (prom-body "vector" "[{\"metric\":{\"job\":\"a\"},\"value\":[1,\"1\"]}]")})))
    (is (not (:match? (prom/evaluate p {:status 200 :body (prom-body "vector" "[]")}))) "no series is not a match")
    (is (:match? (prom/evaluate p {:status 200 :body (prom-body "scalar" "[1,\"2.5\"]")})))
    (is (not (:match? (prom/evaluate (assoc p :match :all)
                                     {:status 200 :body (prom-body "vector" "[{\"metric\":{},\"value\":[1,\"1\"]},{\"metric\":{},\"value\":[1,\"0\"]}]")}))))
    (is (thrown? clojure.lang.ExceptionInfo (prom/evaluate p {:status 503 :body ""})))))

(deftest prom-wakes-through-the-registry
  (with-reg [reg _entries t]
    (let [{:keys [port tick!]} (manual-scheduler)
          gets (atom [])
          http (reify b/IHttpGet
                 (http-get! [_ url q _]
                   (swap! gets conj [url q])
                   {:status 200 :body (prom-body "vector" "[{\"metric\":{},\"value\":[1,\"7\"]}]")}))]
      (binding [poll/*scheduler* port poll/*clock* (clock t) prom/*http* http]
        (registry/create! reg {:id "p1" :owner {:kind :journal} :timeout-ms 600000
                               :subscription {:type :prom-predicate
                                              :spec {:query "sum(rate(x[5m]))" :op ">" :threshold 5
                                                     :url "http://prom:9090/"}}}))
      (tick!)
      (is (wait-until 2000 #(= :woken (:status (registry/lookup reg "p1")))))
      (is (= [["http://prom:9090/api/v1/query" {"query" "sum(rate(x[5m]))"}]] @gets))
      (is (= 7 (get-in (registry/lookup reg "p1") [:wake :event :data :observed]))))))

;; =============================================================================
;; The verb
;; =============================================================================

(deftest create-request-carries-spec-and-poll
  (let [req (await-tool/create-request {:owner_kind "journal" :source "sql-predicate"
                                        :poll_ms 300000 :timeout_ms 7200000
                                        :spec reference-spec})]
    (is (= {:type :sql-predicate :filter nil :dedup-key nil :poll-ms 300000
            :spec {:connection-ref "pass:sisf/staging-pg-url"
                   :query (:query reference-spec) :params [42 "Apto 3"]
                   :predicate {:kind "exists"}}}
           (get-in req [:ok :subscription])))))
