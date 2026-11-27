(ns hive-mcp.await.sources.polling-sources-test
  "The polling await sources (card RUN-LEDGER-4): :sql-predicate (the
   priority one), :k8s-log-regex and :prom-predicate.

   Every boundary is a stub injected the way production injects it: the
   SQL adapter through sql-port/*adapter* (the registry seam), the scheduler,
   clock, secret resolver, process runner and HTTP client through each
   namespace's dynamic port var, the operator allowlists through
   policy/*policy*, all bound with `binding` around create!; the journal
   through the registry's :journal option. No with-redefs. The stub
   scheduler runs a tick only when the test says so."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumers.session]
            [hive-mcp.await.sources.boundary :as b]
            [hive-mcp.await.sources.bounded-regex :as brx]
            [hive-mcp.await.sources.k8s-log-regex :as k8s]
            [hive-mcp.await.sources.policy :as policy]
            [hive-mcp.await.sources.poll :as poll]
            [hive-mcp.await.sources.prom-predicate :as prom]
            [hive-mcp.await.sources.secret-refs :as refs]
            [hive-mcp.await.sources.sql-port :as sql-port]
            [hive-mcp.await.sources.sql-predicate :as sql]
            [hive-mcp.tools.consolidated.await :as await-tool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Stubs
;; =============================================================================

(def ref* "k8s:sisf/staging-pg-ro/url")

(def test-policy
  {:sql  {:connection-refs [ref*]}
   :k8s  {:namespaces ["sisf"] :contexts ["blw"]}
   :prom {:targets {"default" "http://prom:9090/"}}})

(defn- manual-scheduler []
  (let [jobs (atom {}) n (atom 0)]
    {:jobs jobs
     :tick! (fn [] (doseq [[_ f] @jobs] (f)))
     :port (reify b/IPollScheduler
             (schedule-repeating! [_ f _]
               (let [id (swap! n inc)]
                 (swap! jobs assoc id f)
                 (fn [] (swap! jobs dissoc id)))))}))

(defn- clock [t] (reify b/IClock (now-ms [_] @t)))

(defn- stub-adapter
  "A SQL adapter double: its dialect check refuses `delete`, binds $n to ?
   in order; its transport answers (answers) in turn, recording requests."
  [requests answers]
  (let [i (atom -1)]
    {:id "stub" :addon "test"
     :check-and-bind (fn [q params]
                       (when (re-find #"(?i)\bdelete\b" q)
                         (throw (ex-info "stub dialect: not a SELECT"
                                         {:error :await/invalid-subscription :source :sql-predicate})))
                       {:sql (str/replace q #"\$\d+" "?") :params (vec params)})
     :run-select! (fn [req]
                    (swap! requests conj req)
                    (let [a (nth answers (min (swap! i inc) (dec (count answers))))]
                      (if (instance? Throwable a) (throw a) a)))}))

(def secrets
  (reify b/ISecretResolver
    (resolve-secret! [_ ref]
      (if (= ref ref*)
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
     (try (binding [policy/*policy* test-policy] ~@body)
          (finally (registry/shutdown! ~reg)))))

(def reference-spec
  {:connection_ref ref*
   :query "select 1 from contributor.contributors where id=$1 and address_complement=$2"
   :params [42 "Apto 3"]
   :predicate {:kind "exists"}})

(defn- sql-request [id & {:as extra}]
  (merge {:id id :owner {:kind :journal :label "landed?"}
          :subscription {:type :sql-predicate :poll-ms 300000
                         :spec (poll/kw-keys reference-spec)}
          :timeout-ms (* 2 60 60 1000)}
         extra))

(defn- refused [reg req]
  (try (registry/create! reg req) nil (catch clojure.lang.ExceptionInfo e (ex-data e))))

;; =============================================================================
;; sql-predicate
;; =============================================================================

(deftest the-sources-are-registered
  (is (every? (set (ports/known-sources)) [:manual :sql-predicate :k8s-log-regex :prom-predicate])))

(deftest sql-predicate-without-an-adapter-is-refused-naming-the-addon
  (testing "core has no SQL adapter of its own: the hive.await-sql addon contributes it"
    (with-reg [reg _entries _t]
      (is (empty? (sql-port/registered)) "no adapter is registered in core's test tier")
      (let [e (refused reg (sql-request "na"))]
        (is (= :await/source-adapter-missing (:error e)))
        (is (= :sql-runner (:port e)))
        (is (= "load addon hive.await-sql" (:hint e))))
      (is (nil? (registry/lookup reg "na")) "nothing recorded")
      (is (not (str/includes? (slurp (:path reg)) "contributor")) "no row on disk"))))

(deftest a-registered-adapter-is-found-and-retracted
  (let [a (stub-adapter (atom []) [{:rows []}])]
    (try
      (sql-port/register-adapter! a)
      (is (= "stub" (:id (sql-port/adapter-for nil))))
      (is (= "stub" (:id (sql-port/adapter-for "stub"))))
      (is (= :await/source-adapter-missing
             (:error (try (sql-port/adapter-for "mysql") (catch Exception e (ex-data e))))))
      (finally (sql-port/unregister-adapter! "stub")))
    (is (empty? (sql-port/registered)))))

(deftest sql-predicate-reference-case-wakes-when-the-row-lands
  (with-reg [reg entries t]
    (let [{:keys [port tick!]} (manual-scheduler)
          reqs (atom [])]
      (binding [poll/*scheduler* port
                poll/*clock* (clock t)
                sql-port/*adapter* (stub-adapter reqs [{:rows [] :truncated? false}
                                                       {:rows [{:address_complement "Apto 3"}] :truncated? false}])
                sql/*secrets* secrets]
        (is (= :armed (:status (registry/create! reg (sql-request "q1"))))))
      (testing "first poll: no row yet"
        (tick!)
        (is (= :armed (:status (registry/lookup reg "q1")))))
      (testing "second poll: the row exists"
        (tick!)
        (is (wait-until 2000 #(= :woken (:status (registry/lookup reg "q1")))))
        (let [data (get-in (registry/lookup reg "q1") [:wake :event :data])]
          (is (= true (:observed data)))
          (is (= 2 (:poll data)))
          (is (not (contains? data :sample)) "rows are not carried unless sample_rows asks")))
      (testing "bound by the adapter's dialect, bounded, the secret resolved per poll"
        (is (= {:secret "postgres://u:s3cret@db:5432/sisf"
                :sql "select 1 from contributor.contributors where id=? and address_complement=?"
                :params [42 "Apto 3"] :row-cap 100 :statement-timeout-ms 5000}
               (first @reqs)))
        (is (not (str/includes? (slurp (:path reg)) "s3cret")))
        (is (not (str/includes? (slurp (:path reg)) "Apto 3\"}")) "no result row persisted"))
      (testing "the source stopped"
        (let [n (count @reqs)] (tick!) (is (= n (count @reqs)))))
      (is (wait-until 2000 #(seq @entries)) "the registry journaled the wake"))))

(deftest sql-predicate-true-on-the-first-poll-wakes
  (with-reg [reg _entries t]
    (let [reqs (atom [])
          sched (reify b/IPollScheduler
                  ;; runs the first tick synchronously, inside open-source
                  (schedule-repeating! [_ f _] (f) (fn [] nil)))]
      (binding [poll/*scheduler* sched
                poll/*clock* (clock t)
                sql-port/*adapter* (stub-adapter reqs [{:rows [{:n 1}] :truncated? false}])
                sql/*secrets* secrets]
        (registry/create! reg (sql-request "first")))
      (is (wait-until 2000 #(= :woken (:status (registry/lookup reg "first")))))
      (is (= 1 (count @reqs))))))

(deftest sql-predicate-count-scalar-and-opt-in-sample
  (binding [policy/*policy* test-policy]
    (let [parsed (fn [p & {:as more}]
                   (sql/parse {:spec (merge (assoc (poll/kw-keys reference-spec) :predicate p) more)}))
          rows   {:rows [{:n 3 :s "done"} {:n 4}] :truncated? false}]
      (is (:match? (sql/evaluate (parsed {:kind "count" :op ">=" :value 2}) rows)))
      (is (not (:match? (sql/evaluate (parsed {:kind "count" :op ">=" :value 3}) rows))))
      (is (:match? (sql/evaluate (parsed {:kind "scalar" :op ">" :value 2}) rows)))
      (is (:match? (sql/evaluate (parsed {:kind "scalar" :op "=" :value "done" :column "s"}) rows)))
      (is (not (:match? (sql/evaluate (parsed {:kind "scalar" :op "=" :value 1}) {:rows []}))))
      (is (= [{:n 3 :s "done"}]
             (:sample (:data (sql/evaluate (parsed {:kind "exists"} :sample-rows 1) rows))))))))

(deftest sql-predicate-refuses-before-recording
  (with-reg [reg _entries _t]
    (binding [sql-port/*adapter* (stub-adapter (atom []) [{:rows []}])]
      (doseq [[why req] {"no deadline"     (dissoc (sql-request "r1") :timeout-ms)
                         "poll too fast"   (assoc-in (sql-request "r2") [:subscription :poll-ms] 1000)
                         "inline DSN"      (assoc-in (sql-request "r3") [:subscription :spec :connection-ref]
                                                     "postgres://u:pw@db/sisf")
                         "pass: scheme"    (assoc-in (sql-request "r3b") [:subscription :spec :connection-ref]
                                                     "pass:sisf/pg")
                         "not allowlisted" (assoc-in (sql-request "r3c") [:subscription :spec :connection-ref]
                                                     "k8s:other/secret/url")
                         "dialect refusal" (assoc-in (sql-request "r4") [:subscription :spec :query]
                                                     "delete from contributor.contributors where id=$1 and x=$2")
                         "deadline far"    (assoc (sql-request "r5") :timeout-ms (* 72 3600 1000))
                         "row cap big"     (assoc-in (sql-request "r6") [:subscription :spec :row-cap] 100000)
                         "unknown key"     (assoc-in (sql-request "r7") [:subscription :spec :url]
                                                     "postgres://u:pw@db/sisf")}]
        (let [e (refused reg req)]
          (is (contains? #{:await/invalid-subscription :await/not-allowed} (:error e)) why)
          (is (nil? (registry/lookup reg (:id req))) (str why ": nothing recorded"))))
      (is (not (str/includes? (slurp (:path reg)) "pw@")) "no refused credential reached the file"))))

(deftest a-vault-ref-needs-the-cloak-addon
  (with-reg [reg _entries _t]
    (binding [sql-port/*adapter* (stub-adapter (atom []) [{:rows []}])
              policy/*policy* (assoc-in test-policy [:sql :connection-refs] ["vault:sisf-pg"])]
      (let [e (refused reg (assoc-in (sql-request "v1") [:subscription :spec :connection-ref] "vault:sisf-pg"))]
        (is (= :await/source-adapter-missing (:error e)))
        (is (= "load addon hive.cloak" (:hint e)))))))

(deftest sql-predicate-failing-poll-goes-on-and-cancel-stops
  (with-reg [reg _entries t]
    (let [{:keys [port tick! jobs]} (manual-scheduler)
          reqs (atom [])]
      (binding [poll/*scheduler* port
                poll/*clock* (clock t)
                sql-port/*adapter* (stub-adapter reqs [(ex-info "db down" {:error :await/sql-failed})
                                                       {:rows [] :truncated? false}])
                sql/*secrets* secrets]
        (registry/create! reg (sql-request "c1")))
      (tick!) (tick!)
      (is (= 2 (count @reqs)))
      (is (= :armed (:status (registry/lookup reg "c1"))))
      (registry/cancel! reg "c1")
      (is (empty? @jobs))
      (tick!)
      (is (= 2 (count @reqs))))))

(deftest polling-stops-at-the-deadline-and-the-registry-times-out
  (with-reg [reg _entries t]
    (let [{:keys [port tick! jobs]} (manual-scheduler)
          reqs (atom [])]
      (binding [poll/*scheduler* port
                poll/*clock* (clock t)
                sql-port/*adapter* (stub-adapter reqs [{:rows [] :truncated? false}])
                sql/*secrets* secrets]
        (registry/create! reg (sql-request "d1" :timeout-ms 60000)))
      (tick!)
      (swap! t + 60000)
      (tick!)
      (is (= 1 (count @reqs)))
      (is (empty? @jobs))
      (registry/tick! reg)
      (is (wait-until 2000 #(= :timeout (get-in (registry/lookup reg "d1") [:wake :reason])))))))

;; =============================================================================
;; secret refs
;; =============================================================================

(deftest secret-refs-name-secrets-and-resolve-bounded
  (is (= {:namespace "sisf" :secret "staging-pg-ro" :key "url"} (refs/parse-k8s-id "sisf/staging-pg-ro/url")))
  (is (nil? (refs/parse-ref "postgres://u:p@h/db")))
  (is (nil? (refs/parse-k8s-id "../x/y")))
  (is (= ["kubectl" "--context" "blw" "get" "secret" "staging-pg-ro" "-n" "sisf" "-o" "jsonpath={.data.url}"]
         (refs/k8s-argv (refs/parse-k8s-id "sisf/staging-pg-ro/url") "blw")))
  (testing "a k8s secret is read through the process port and base64-decoded"
    (binding [refs/*process* (reify b/IProcessRunner
                               (run-process! [_ argv opts]
                                 (is (= 5000 (:timeout-ms opts)))
                                 {:exit 0 :out (.encodeToString (java.util.Base64/getEncoder)
                                                                (.getBytes "postgres://x"))}))]
      (is (= "postgres://x" (refs/resolve-ref ref*)))))
  (testing "a hung resolver fails the poll after the bound, naming no value"
    (refs/register-resolver! "slow" {:resolve (fn [_] (Thread/sleep 60000) "v")})
    (try
      (let [t0 (System/currentTimeMillis)
            e  (try (refs/resolve-ref "slow:x") (catch Exception e (ex-data e)))]
        (is (= :await/secret-timeout (:error e)))
        (is (< (- (System/currentTimeMillis) t0) 8000)))
      (finally (refs/unregister-resolver! "slow")))))

;; =============================================================================
;; k8s-log-regex
;; =============================================================================

(def k8s-sub
  {:type :k8s-log-regex
   :spec {:namespace "sisf" :selector "app=sisf-sync" :container "sync"
          :context "blw" :regex "Scheduled sync completed"}})

(deftest k8s-argv-is-a-bounded-poll-with-no-shell
  (binding [policy/*policy* test-policy]
    (let [p (k8s/parse k8s-sub)]
      (is (= ["kubectl" "--context" "blw" "logs" "-n" "sisf" "-l" "app=sisf-sync" "--prefix"
              "--max-log-requests" "10" "-c" "sync" "--timestamps"
              "--since-time=1970-01-01T00:16:40Z" "--tail=2000"]
             (k8s/argv p (k8s/since-time p 1000000)))))))

(deftest k8s-refuses-bad-names-unlisted-namespaces-and-unbounded-regexes
  (binding [policy/*policy* test-policy]
    (doseq [spec [{:namespace "--all" :pod "x" :regex "a"}
                  {:namespace "sisf" :regex "a"}
                  {:namespace "sisf" :pod "a" :selector "b=c" :regex "a"}
                  {:namespace "sisf" :pod "a" :regex "("}
                  {:namespace "kube-system" :pod "a" :regex "password=\\S+"}
                  {:namespace "sisf" :pod "a" :context "prod" :regex "a"}
                  {:namespace "sisf" :pod "a" :regex "(a+)+$"}
                  {:namespace "sisf" :pod "a" :regex "(x|xx)*y"}
                  {:namespace "sisf" :pod "a" :regex "(a)\\1"}
                  {:namespace "sisf" :pod "a" :regex "a" :verbose true}]]
      (is (thrown? clojure.lang.ExceptionInfo (k8s/parse {:spec spec})) (pr-str spec)))))

(deftest bounded-regex-stops-a-runaway-match
  (is (false? (brx/nested-quantifier? "Scheduled sync completed rows=\\d+")))
  (is (true? (brx/nested-quantifier? "((ab)+c)*")))
  (is (= "rows=12" (brx/find-in (re-pattern "rows=\\d+") "done rows=12" 1000)))
  (let [p (re-pattern "(a|aa)+b")
        s (apply str (repeat 5000 "a"))
        e (try (brx/find-in p s 0) nil
               (catch clojure.lang.ExceptionInfo e (ex-data e)))]
    (is (= :await/regex-timeout (:error e))
        "matching stops once its time budget is spent (the charAt check)")))

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
      (is (= :armed (:status (registry/lookup reg "k1"))))
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
  (binding [policy/*policy* test-policy]
    (let [p (prom/parse {:spec {:query "up" :op ">=" :threshold 1}})]
      (is (:match? (prom/evaluate p {:status 200 :body (prom-body "vector" "[{\"metric\":{\"job\":\"a\"},\"value\":[1,\"1\"]}]")})))
      (is (not (:match? (prom/evaluate p {:status 200 :body (prom-body "vector" "[]")}))))
      (is (:match? (prom/evaluate p {:status 200 :body (prom-body "scalar" "[1,\"2.5\"]")})))
      (is (not (:match? (prom/evaluate (assoc p :match :all)
                                       {:status 200 :body (prom-body "vector" "[{\"metric\":{},\"value\":[1,\"1\"]},{\"metric\":{},\"value\":[1,\"0\"]}]")}))))
      (testing "a failed query reports status and errorType, never the response text"
        (let [m (try (prom/evaluate p {:status 200 :body "{\"status\":\"error\",\"errorType\":\"bad_data\",\"error\":\"internal secret\"}"})
                     (catch Exception e (.getMessage e)))]
          (is (str/includes? m "bad_data"))
          (is (not (str/includes? m "internal secret"))))))))

(deftest prom-refuses-a-caller-url-and-an-unknown-target
  (binding [policy/*policy* test-policy]
    (is (thrown? clojure.lang.ExceptionInfo
                 (prom/parse {:spec {:query "up" :op ">" :threshold 0 :url "http://169.254.169.254/"}})))
    (is (= :await/not-allowed
           (:error (try (prom/parse {:spec {:query "up" :op ">" :threshold 0 :target "kube-api"}})
                        (catch Exception e (ex-data e))))))))

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
                                              :spec {:query "sum(rate(x[5m]))" :op ">" :threshold 5}}}))
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
            :spec {:connection-ref ref*
                   :query (:query reference-spec) :params [42 "Apto 3"]
                   :predicate {:kind "exists"}}}
           (get-in req [:ok :subscription])))))
