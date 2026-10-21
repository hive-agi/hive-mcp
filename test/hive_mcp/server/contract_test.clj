(ns hive-mcp.server.contract-test
  "Contract facets for the files of src/hive_mcp/server that batch m19 owns
   (core, init, lifecycle, log-sink, permissions, registration, transport):
   the pure rules as schema-driven trifectas, and goldens over the boundary
   fns that need no running server. No port is bound and no Integrant system
   is started; the hide-rule registry is restored around every test."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.server.core :as core]
            [hive-mcp.server.init :as init]
            [hive-mcp.server.log-sink :as sink]
            [hive-mcp.server.permissions :as perm]
            [hive-mcp.server.registration :as reg]
            [hive-schemas.coverage :as coverage]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-hide-rules
  "Every test sees exactly the boot rule set, and leaves it so."
  [t]
  (let [before (reg/hide-rule-ids)]
    (try (t)
         (finally
           (doseq [id (reg/hide-rule-ids) :when (not (contains? before id))]
             (reg/unregister-hide-rule! id))
           (reg/register-hide-rule! :deprecated #'reg/deprecated-tool?)))))

(use-fixtures :each own-hide-rules)

;; =============================================================================
;; Gate
;; =============================================================================

(def ^:private batch-paths
  ["src/hive_mcp/server/core.clj" "src/hive_mcp/server/init.clj"
   "src/hive_mcp/server/lifecycle.clj" "src/hive_mcp/server/log_sink.clj"
   "src/hive_mcp/server/permissions.clj" "src/hive_mcp/server/registration.clj"
   "src/hive_mcp/server/transport.clj"])

(coverage/deftest-contract-coverage server-contracts
  {:paths batch-paths
   :exempt {'hive-mcp.server.core/-main
            "The JVM entry point: it starts the system and then blocks on the
             keepalive until shutdown, ending in System/exit. It never returns,
             so it has no result to promise, and no test may call it."}})

(deftest the-gate-is-not-vacuous
  (is (= 81  (count (:universe (coverage/coverage {:paths batch-paths}))))))

(deftest every-server-contract-generates-both-sides
  (doseq [ns-sym '[hive-mcp.server.core hive-mcp.server.init hive-mcp.server.lifecycle
                   hive-mcp.server.log-sink hive-mcp.server.permissions
                   hive-mcp.server.registration hive-mcp.server.transport]
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

;; =============================================================================
;; core: profile precedence and overlays
;; =============================================================================

(def ^:private Prof [:maybe [:enum "desktop" "k8s-headless" "k8s-minimal" :custom]])

(deftrifecta-from-schema profile-from-takes-cli-then-env-then-config
  #'core/profile-from
  {:in [:cat Prof Prof Prof]
   :out :keyword
   :rel (fn [[c e f] out] (= out (keyword (or c e f "desktop"))))
   :classify (fn [[c e f] _] (cond c :cli e :env f :config :else :default))
   :classify-domain #{:cli :env :config :default}
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/server/profile-from.edn"
   :num-tests 200})

(def ^:private Overlay
  [:map-of [:enum :a :b :c] [:maybe [:map-of [:enum :x :y] :int]]])

(deftrifecta-from-schema apply-overlay-merges-and-nil-removes
  #'core/apply-overlay
  {:in [:cat Overlay Overlay]
   :out [:map-of :any :any]
   :rel (fn [[base overlay] out]
          (every? (fn [k]
                    (cond (and (contains? overlay k) (nil? (get overlay k))) (not (contains? out k))
                          (contains? overlay k) (contains? out k)
                          :else (= (get base k) (get out k))))
                  (into (set (keys base)) (keys overlay))))
   :golden-path "test/golden/hive-mcp/server/apply-overlay.edn"
   :num-tests 200})

(deftest-golden profile-overlays-read-from-the-classpath
  "test/golden/hive-mcp/server/profile-config.edn"
  {:missing-profile (core/read-profile-config :no-such-profile)
   :base-keys (vec (sort (map str (keys (core/read-base-config)))))
   :stop-without-system (do (when-not @core/system (core/stop!)) :not-running)})

;; =============================================================================
;; permissions: tier detection (pure; escalation beyond :tier-1 asks a human)
;; =============================================================================

(def ^:private Tool [:enum "file_write" "file_edit" "bash" "magit_push" "memory" "clojure_edit"])
(def ^:private Args
  [:maybe [:map
           ["file_path" {:optional true} [:enum "test/foo_test.clj" "src/core.clj" "README.md" "lib/x.clj" "a/generated/b.clj"]]
           ["command" {:optional true} [:enum "ls" "rm -rf x" "DROP TABLE t" "echo"]]]])

(defn- oracle-tier [tool args]
  (let [path (get args "file_path")
        cmd (get args "command" "")
        writes #{"file_write" "file_edit" "clojure_edit"}]
    (cond (contains? #{"bash" "magit_push"} tool) :tier-3
          (and (writes tool) path (re-find #"_test\.clj$|^test/|/generated/" path)) :tier-1
          (and (writes tool) path (re-find #"^src/|^lib/" path)) :tier-2
          :else (do cmd :allowed))))

(deftrifecta-from-schema get-tier-follows-the-matrix
  #'perm/get-tier
  {:in [:cat Tool Args]
   :out perm/Tier
   :rel (fn [[t a] out] (= out (oracle-tier t a)))
   :classify (fn [_ out] out)
   :classify-domain perm/Tier
   :classify-floor 5
   :golden-path "test/golden/hive-mcp/server/get-tier.edn"
   :num-tests 300})

(deftrifecta-from-schema requires-escalation-is-not-allowed
  #'perm/requires-escalation?
  {:in [:cat Tool Args]
   :out :boolean
   :rel (fn [[t a] out] (= out (not= :allowed (oracle-tier t a))))
   :golden-path "test/golden/hive-mcp/server/requires-escalation.edn"
   :num-tests 200})

(deftrifecta-from-schema dangerous-tool-is-tier-3-membership
  #'perm/dangerous-tool?
  {:in [:cat [:enum "bash" "magit_commit" "eval_elisp" "swarm_kill" "file_edit" "memory" nil]]
   :out :boolean
   :rel (fn [[t] out] (= out (contains? #{"bash" "magit_commit" "eval_elisp" "swarm_kill"} t)))
   :golden-path "test/golden/hive-mcp/server/dangerous-tool.edn"
   :num-tests 100})

(deftest-golden escalate-auto-approves-without-asking
  "test/golden/hive-mcp/server/escalate.edn"
  {:allowed (perm/escalate! "ling-1" "memory" {})
   :tier-1 (perm/escalate! "ling-1" "file_edit" {"file_path" "test/a_test.clj"})
   :approved? [(perm/approved? {:approved true}) (perm/approved? {:approved false}) (perm/approved? nil)]})

;; =============================================================================
;; registration: the tools/list projection
;; =============================================================================

(def ^:private GenEntry
  [:map [:tool [:map [:name [:enum "a" "b" "c" "zeta" "memory"]]
                [:deprecated {:optional true} [:maybe :boolean]]]]])

(def ^:private BootRules
  "The boot rule set, owned by this test: the subjects below never read the
   process-global registry, which other tests populate."
  {:deprecated #'reg/deprecated-tool?})

;; The subjects close over BootRules so a generated case (and so the golden)
;; holds only plain data: the entries in, the tool-defs out. The rules map
;; carries a var, which EDN cannot read back.
(defn- listed-under-boot-rules [entries] (reg/listed-tools BootRules entries))

(defn- visible-under-boot-rules [entries] (reg/visible-tools BootRules entries))

(deftrifecta-from-schema listed-tools-hides-deprecated-and-sorts
  #'listed-under-boot-rules
  {:in [:cat [:maybe [:vector {:max 5} GenEntry]]]
   :out [:vector reg/ToolDef]
   :rel (fn [[entries] out]
          (and (= out (sort-by :name out))
               (= (frequencies (map :name out))
                  (frequencies (map :name (remove :deprecated (map :tool entries)))))))
   :golden-path "test/golden/hive-mcp/server/listed-tools.edn"
   :num-tests 200})

(deftrifecta-from-schema visible-tools-keeps-table-order
  #'visible-under-boot-rules
  {:in [:cat [:maybe [:vector {:max 5} GenEntry]]]
   :out [:vector reg/ToolDef]
   :rel (fn [[entries] out] (= out (vec (remove :deprecated (map :tool entries)))))
   :golden-path "test/golden/hive-mcp/server/visible-tools.edn"
   :num-tests 200})

(deftrifecta-from-schema deprecated-tool-reads-the-flag
  #'reg/deprecated-tool?
  {:in [:cat [:maybe [:map [:name :string] [:deprecated {:optional true} [:maybe :boolean]]]]]
   :out :boolean
   :rel (fn [[t] out] (= out (boolean (:deprecated t))))
   :num-tests 100
   :mutation false}) ; a boolean output yields no schema-derived mutant the
                     ; property does not already pin

(deftest hide-rules-register-and-unregister
  (is (= :m19-test (reg/register-hide-rule! :m19-test (comp #{"a"} :name))))
  (is (contains? (reg/hide-rule-ids) :m19-test))
  (is (reg/hidden? {:m19-test (comp #{"a"} :name)} {:name "a"}))
  (is (false? (reg/hidden? {:m19-test (comp #{"a"} :name)} {:name "b"})))
  (is (= {:tools [{:name "b"}]}
         (reg/receive-tools-list nil {:tools (atom {"a" {:tool {:name "a"}} "b" {:tool {:name "b"}}})} nil)))
  (is (= :m19-test (reg/unregister-hide-rule! :m19-test)))
  (is (not (contains? (reg/hide-rule-ids) :m19-test)))
  (is (true? (reg/install-tools-list!)))
  (is (true? (reg/tools-list-installed?))))

;; =============================================================================
;; log-sink: admission without a drain thread
;; =============================================================================

(deftest-golden sink-admission-is-bounded
  "test/golden/hive-mcp/server/sink-admission.edn"
  (let [s (sink/make-sink (sink/err-stream-writer) {:start? false :capacity 2})
        offers (mapv #(sink/offer! s %) ["a" "b" "c"])
        appended ((sink/appender-fn s) {:output_ (delay "d")})
        boot ((sink/boot-appender-fn s) {:level :info :?ns-str "elsewhere" :vargs []})]
    {:offers offers :appended appended :boot boot
     :depth (sink/depth s) :dropped (sink/dropped s) :stopped (sink/stop! s)
     :thread (:thread s) :capacity (:capacity s)}))

(deftest boot-line-admits-only-valid-typed-events
  (let [ev (sink/boot-event :hive.carto :addon 12.5 :ok)
        line {:level :info :?ns-str "hive-mcp.server.core" :vargs [ev]}]
    (is (true? (sink/boot-line? line)))
    (is (not (sink/boot-line? (assoc line :level :warn))))
    (is (not (sink/boot-line? (assoc line :vargs [(sink/boot-event "API_KEY=x" :addon 1.0 :ok)]))))
    (is (not (sink/boot-line? (assoc line :vargs [(sink/boot-event :a :addon -1.0 :ok)]))))
    (is (instance? java.io.File (sink/file-path)))
    (is (str/ends-with? (str (sink/file-path)) "coordinator.log"))))

;; =============================================================================
;; init: pure helpers
;; =============================================================================

(def ^:private watch-dirs-in
  "Explicit roots only, so no case depends on the machine's classpath."
  [:cat [:maybe [:vector {:max 3} [:enum "src" "dev" "/abs/src" "/opt/x"]]]
   [:maybe [:vector {:max 2} [:enum "/srv/proj/src" "/repo/src"]]]])

(deftrifecta-from-schema watch-dirs-are-absolute
  #'init/watch-dirs
  {:in watch-dirs-in
   :out [:vector [:string {:min 1}]]
   :rel (fn [[configured roots] out]
          (and (every? #(str/starts-with? % "/") out)
               (= (count out) (count (or (seq configured) (seq roots) ["src"])))))
   :num-tests 200})

(defn- cwd-relative
  "`dir` with the JVM working directory replaced by <cwd>. Only the no-root
   fallback resolves against the working directory; every other case is a
   fixed absolute path already, so the golden reads the same on any machine."
  [dir]
  (let [cwd (System/getProperty "user.dir")]
    (if (str/starts-with? dir cwd)
      (str "<cwd>" (subs dir (count cwd)))
      dir)))

(def ^:private watch-dirs-cases
  (hive-schemas.test/seeded-cases watch-dirs-in 0 8))

(deftest watch-dirs-golden-cases-are-absolute
  (doseq [in (vals watch-dirs-cases)]
    (is (every? #(str/starts-with? % "/") (apply init/watch-dirs in))
        (str "relative watch dir for " (pr-str in)))))

(deftest-golden watch-dirs-are-absolute-golden
  "test/golden/hive-mcp/server/watch-dirs.edn"
  (into (sorted-map)
        (map (fn [[label in]]
               [label {:in in :out (mapv cwd-relative (apply init/watch-dirs in))}]))
        watch-dirs-cases))

(deftest hot-reload-event-other-than-success-is-ignored
  (is (nil? (init/on-hot-reload-event! {:type :reload-failed})))
  (is (nil? (init/on-hot-reload-event! nil))))

(deftest hot-reload-listener-registers-through-the-port
  (let [seen (atom nil)]
    (is (= :mcp-auto-heal (init/register-hot-reload-listener! (fn [id f] (reset! seen [id (ifn? f)])))))
    (is (= [:mcp-auto-heal true] @seen))))
