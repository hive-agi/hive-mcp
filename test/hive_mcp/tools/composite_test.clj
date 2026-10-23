(ns hive-mcp.tools.composite-test
  "lazy-resolve-schema-props resolves every shape a subdomain advertises its
   params in, and the swarm root folds every subdomain's params through it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.set :as set]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.tools.consolidated.swarm :as swarm]
            [hive-mcp.extensions.registry :as ext]
            [clojure.string :as str]))

(def tool-def-map
  {:inputSchema {:properties {"from-map" {:type "string"}}}})

(def tools-vec
  [{:inputSchema {:properties {"from-vec" {:type "string"}}}}
   {:inputSchema {:properties {"second-entry" {:type "string"}}}}])

(defn tool-defs-fn []
  [{:inputSchema {:properties {"from-fn" {:type "string"}}}}])

(deftest lazy-resolve-schema-props-shapes-test
  (testing "a `tool-def` map"
    (is (= {"from-map" {:type "string"}}
           (composite/lazy-resolve-schema-props 'hive-mcp.tools.composite-test/tool-def-map))))
  (testing "a `tools` vector — the first entry is the root tool"
    (is (= {"from-vec" {:type "string"}}
           (composite/lazy-resolve-schema-props 'hive-mcp.tools.composite-test/tools-vec))))
  (testing "a 0-arity `tool-defs` fn"
    (is (= {"from-fn" {:type "string"}}
           (composite/lazy-resolve-schema-props 'hive-mcp.tools.composite-test/tool-defs-fn))))
  (testing "an unresolvable symbol contributes nothing rather than failing"
    (is (= {} (composite/lazy-resolve-schema-props 'hive-mcp.tools.composite-test/no-such-var)))
    (is (= {} (composite/lazy-resolve-schema-props 'no.such.ns/tools)))))

(def swarm-subdomain-tools
  '[hive-mcp.tools.consolidated.agent/tools
    hive-mcp.tools.consolidated.hivemind/tools
    hive-mcp.tools.consolidated.agora/tools
    hive-mcp.tools.consolidated.olympus/tools
    hive-mcp.tools.consolidated.preset/tools])

(deftest swarm-root-folds-every-subdomain-param-test
  (let [root-props (set (keys (get-in swarm/tool-def [:inputSchema :properties])))]
    (doseq [sym swarm-subdomain-tools]
      (let [sub-props (set (keys (composite/lazy-resolve-schema-props sym)))]
        (testing (str sym " advertises params")
          (is (seq sub-props)))
        (testing (str "every param of " sym " survives the swarm fold")
          (is (empty? (set/difference sub-props root-props))))))))

;; =============================================================================
;; build-merged-tool — a contribution's :params reach the advertised schema.
;;
;; Measured live 2026-09-05: `swarm ling-wave dispatch` routed, but the swarm
;; tool's schema had no `providers` slot, the MCP layer dropped the argument,
;; and dispatch answered :wave/no-providers to every spelling of the call.
;; =============================================================================

(def ^:private merged-core
  {:name "merged-test-root" :consolidated true
   :inputSchema {:type "object"
                 :properties {"command" {:type "string"}
                              "tasks"   {:type "array" :items {:type "object"}
                                         :description "core tasks"}}}})

(defn- with-contribution [params f]
  (ext/contribute-commands! "merged-test-root" :merged-test-addon
                            {"ling-wave" {:handler (fn [_] nil) :params params}})
  (try (f)
       (finally (ext/retract-commands! "merged-test-root" :merged-test-addon))))

(deftest build-merged-tool-folds-contributed-params-test
  (with-contribution {"providers" {:type "array" :description "members"}
                      "tasks"     {:type "array" :items {:type "string"}
                                   :description "ling tasks"}}
    (fn []
      (let [props (get-in (composite/build-merged-tool merged-core)
                          [:inputSchema :properties])]
        (testing "a param the core never declared is now advertised"
          (is (= {:type "array" :description "members"} (get props "providers"))))
        (testing "a colliding param unions both shapes instead of retyping the core's"
          (is (= 2 (count (get-in props ["tasks" :anyOf]))))
          (is (= "core tasks | ling tasks" (get-in props ["tasks" :description]))))
        (testing "a free-text `command` does not acquire an enum of addon names"
          (is (nil? (get-in props ["command" :enum]))))))))

(deftest build-merged-tool-extends-an-existing-command-enum-test
  (with-contribution {}
    (fn []
      (let [core (assoc-in merged-core [:inputSchema :properties "command" :enum] ["a" "b"])
            t    (composite/build-merged-tool core)]
        (is (= ["a" "b" "ling-wave"] (get-in t [:inputSchema :properties "command" :enum])))
        (is (:composite t))))))

(deftest build-merged-tool-enum-advertises-a-subdomains-verbs-test
  ;; Measured 2026-10-01: hive-git contributes `ship` and `belt` as verb maps,
  ;; and the git tool's enum listed neither `ship land` nor `belt scan`.
  (ext/contribute-commands! "merged-test-root" :merged-test-addon
                            {"ship" {:handler {:land (fn [_] nil) :status (fn [_] nil)}}
                             "solo" {:handler (fn [_] nil)}})
  (try
    (let [core (assoc-in merged-core [:inputSchema :properties "command" :enum] ["a"])
          enum (get-in (composite/build-merged-tool core) [:inputSchema :properties "command" :enum])]
      (is (= ["a" "ship" "ship land" "ship status" "solo"] enum)
          "the subdomain stays (it answers help); each verb joins; a plain handler adds only its name"))
    (finally (ext/retract-commands! "merged-test-root" :merged-test-addon))))

(deftest build-merged-tool-folds-contributed-default-async-test
  (ext/contribute-commands! "merged-test-root" :merged-test-addon
                            {"ship"  {:handler {:land (fn [_] nil) :status (fn [_] nil)}
                                      :default-async [:land]}
                             "sweep" {:handler (fn [_] nil) :default-async true}
                             "solo"  {:handler (fn [_] nil)}})
  (try
    (testing "a declared verb joins as `<command> <verb>`, a declared command as itself"
      (is (= #{(keyword "ship land") :sweep}
             (:default-async-commands (composite/build-merged-tool merged-core)))))
    (testing "the core's own default-async commands are kept"
      (is (= #{:add (keyword "ship land") :sweep}
             (:default-async-commands
              (composite/build-merged-tool
               (assoc merged-core :default-async-commands #{:add}))))))
    (finally (ext/retract-commands! "merged-test-root" :merged-test-addon)))
  (testing "contributions that declare none leave the key absent"
    (with-contribution {}
      (fn []
        (is (not (contains? (composite/build-merged-tool merged-core)
                            :default-async-commands)))))))

(deftest build-merged-tool-is-identity-without-contributions-test
  (is (= merged-core (composite/build-merged-tool merged-core))))

;; =============================================================================
;; Deprecated contributions route but are not advertised.
;;
;; Measured 2026-10-23: the memory tool description was 6593 chars, about a
;; third of it ~25 hive-cloak aliases whose summary is "Deprecated: use
;; `cloak ...`" — bytes every client paid every session, pointing at a root
;; the visibility gate may hide.
;; =============================================================================

(deftest deprecated-contribution?-test
  (testing "the structured flag, true or a replacement string"
    (is (composite/deprecated-contribution? {:deprecated true}))
    (is (composite/deprecated-contribution? {:deprecated "use `cloak on`"})))
  (testing "the summary prefix, when no flag is declared"
    (is (composite/deprecated-contribution? {:description "Deprecated: use `cloak status`."}))
    (is (composite/deprecated-contribution? {:summary "deprecated: gone next release"})))
  (testing "an explicit false wins over the prefix"
    (is (not (composite/deprecated-contribution?
              {:deprecated false :description "Deprecated: but still advertised"}))))
  (testing "ordinary contributions"
    (is (not (composite/deprecated-contribution? {:description "Seal one entry."})))
    (is (not (composite/deprecated-contribution? {:description "Mentions deprecated mid-line."})))
    (is (not (composite/deprecated-contribution? {})))
    (is (not (composite/deprecated-contribution? nil)))))

(deftest contributed-commands-text-omits-deprecated-test
  (let [cmds {"belt"        {:handler (fn [_] nil) :description "Scan the belt."}
              "seal-status" {:handler (fn [_] nil) :description "Deprecated: use `cloak status`."}
              "old-flag"    {:handler (fn [_] nil) :deprecated true :description "Old thing."}}
        text (composite/contributed-commands-text cmds)]
    (is (str/includes? text "belt - Scan the belt."))
    (is (not (str/includes? text "seal-status")))
    (is (not (str/includes? text "old-flag")))
    (is (not (str/includes? text "Deprecated")))
    (testing "nothing advertised -> no sentence at all"
      (is (nil? (composite/contributed-commands-text
                 (select-keys cmds ["seal-status" "old-flag"])))))))

(deftest deprecated-contribution-routes-but-is-not-advertised-test
  (ext/contribute-commands! "merged-test-root" :merged-test-addon
                            {"seal-status" {:handler     (fn [_] {:type "text" :text "legacy"})
                                            :description "Deprecated: use `cloak status`."
                                            :params      {"slot" {:type "string"}}}
                             "belt"        {:handler     (fn [_] {:type "text" :text "belt"})
                                            :description "Scan the belt."}})
  (try
    (let [core (assoc-in merged-core [:inputSchema :properties "command" :enum] ["a"])
          t    (composite/build-merged-tool core)]
      (testing "absent from the description sentence"
        (is (str/includes? (:description t) "belt - Scan the belt."))
        (is (not (str/includes? (:description t) "seal-status"))))
      (testing "kept in the enum and its params kept, so schema-validating clients still accept it"
        (is (= ["a" "belt" "seal-status"] (get-in t [:inputSchema :properties "command" :enum])))
        (is (= {:type "string"} (get-in t [:inputSchema :properties "slot"])))))
    (testing "present in routing"
      (let [hs (composite/effective-handlers "merged-test-root" {:a (fn [_] nil)})]
        (is (contains? hs :seal-status))
        (is (= "legacy" (:text ((:seal-status hs) {}))))
        (is (= "legacy" (:text ((composite/build-merged-handler "merged-test-root" {:a (fn [_] nil)})
                                {:command "seal-status"}))))))
    (finally (ext/retract-commands! "merged-test-root" :merged-test-addon))))
