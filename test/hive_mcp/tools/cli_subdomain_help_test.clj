(ns hive-mcp.tools.cli-subdomain-help-test
  "A subdomain answers its own help, the dispatcher never qualifies a command
   with the subdomain it already names, and a tool's description lists every
   addon-contributed command it routes."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.cli :as cli]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.dispatch.handler]))

(defn- opaque [m] (with-meta m {:hive-mcp.tools.cli/opaque-roots (set (keys m))}))

(def ^:private help-body {:type "text" :text "ss help body"})

(def ^:private tree
  (opaque {:ss        {:help  (fn [p] (assoc help-body :seen (:command p)))
                       :drain (fn [_] :drained)}
           :goal-plan (fn [_] :goal-plan)
           :plain     {:a (fn [_] :a) :b (fn [_] :b)}}))

(defn- call [command] ((cli/make-cli-handler tree) {:command command}))

(deftest bare-subdomain-and-subdomain-help-reach-its-help-leaf
  (is (= (assoc help-body :seen "ss help") (call "ss")))
  (is (= (assoc help-body :seen "ss help") (call "ss help")))
  (is (= :drained (call "ss drain"))))

(deftest a-subtree-without-help-answers-its-listing
  ;; AF-06 (card 20261118230911-7c2010b4): `kanban help`, `workflow help` and
  ;; `swarm dag help` answered Unknown command; command=help is the documented
  ;; discovery path, so a subdomain answers its verbs.
  (doseq [c ["plain" "plain help"]]
    (testing c
      (let [r (call c)]
        (is (not (:isError r)))
        (is (str/includes? (:text r) "'plain' commands:"))
        (is (str/includes? (:text r) "  - plain a"))
        (is (str/includes? (:text r) "  - plain b"))
        (is (not (str/includes? (:text r) "plain plain"))))))
  (testing "a deeper miss is not mistaken for help"
    (let [r (call "plain zzz")]
      (is (:isError r))
      (is (str/includes? (:text r) "plain a")))))

(deftest a-default-router-subtree-still-answers-help
  (let [h (cli/make-cli-handler {:wf {:_handler (fn [p] [:routed (:command p)])
                                      :forge    {:strike (fn [_] :s)}
                                      :catchup  (fn [_] :c)}
                                 :opaque {:_handler (fn [p] [:opaque (:command p)])}})]
    (testing "help lists the verbs instead of reaching the router"
      (let [r (h {:command "wf help"})]
        (is (str/includes? (:text r) "  - wf catchup"))
        (is (str/includes? (:text r) "  - wf forge strike"))))
    (testing "other commands still route"
      (is (= :c (h {:command "wf catchup"})))
      (is (= [:routed "wf other"] (h {:command "wf other"}))))
    (testing "a router that enumerates nothing keeps its own help"
      (is (= [:opaque "opaque help"] (h {:command "opaque help"}))))))

(deftest every-consolidated-subdomain-answers-help
  ;; Over the real consolidated trees: every enumerable subdomain answers
  ;; `<subdomain> help` and bare `<subdomain>` with something other than
  ;; Unknown command. Only the trees are read; no handler is invoked except a
  ;; subdomain's own `help` leaf, which describes and does nothing. A root
  ;; that does not resolve FAILS here rather than being skipped.
  (let [roots '[hive-mcp.tools.consolidated.project/canonical-handlers
                hive-mcp.tools.consolidated.swarm/canonical-handlers
                hive-mcp.tools.consolidated.memory/canonical-handlers]
        checked (atom 0)]
    (doseq [sym roots
            :let [v (try (requiring-resolve sym) (catch Throwable t t))]]
      (testing (str sym " resolves")
        (is (var? v) (str "root did not resolve: " (if (instance? Throwable v) (ex-message v) "no such var"))))
      (when (var? v)
        (let [tree @v
              h    (cli/make-cli-handler tree)]
          (doseq [[k node] tree
                  :let [cur (hive-mcp.dispatch.handler/current node)]
                  :when (and (map? cur) (seq (remove #{:_handler :help} (keys cur)))
                             (not (contains? cur :help)))
                  c [(str (name k) " help") (name k)]]
            (swap! checked inc)
            (testing (str sym " " c)
              (let [text (str (:text (h {:command c})))]
                (is (not (str/includes? text "Unknown command")) text)
                (is (str/includes? text (str "'" (name k) "' commands:")) text)))))))
    (is (pos? @checked) "non-vacuous: at least one consolidated subdomain was checked")))

(deftest help-listing-omits-deprecated-aliases-and-names-the-bare-default
  (let [tree @(requiring-resolve 'hive-mcp.tools.consolidated.workflow/canonical-handlers)
        h    (cli/make-cli-handler {:workflow tree})
        text (str (:text (h {:command "workflow forge help"})))]
    (testing "the listing keeps the live verbs"
      (is (str/includes? text "  - workflow forge strike\n"))
      (is (str/includes? text "  - workflow forge status")))
    (testing "a deprecated alias still routes but is not listed"
      (is (not (str/includes? text "strike-imperative")))
      (is (contains? (:forge tree) :strike-imperative)))
    (testing "it says what bare `workflow forge` runs, and bare really runs it"
      (is (str/includes? text "Bare 'workflow forge' runs 'workflow forge status'."))))
  (testing "a subtree with no :_handler has no bare-default line"
    (let [h (cli/make-cli-handler {:plain {:a (fn [_] :a) :b (fn [_] :b)}})]
      (is (not (str/includes? (str (:text (h {:command "plain help"}))) "Bare")))))
  (testing "a default that is no sibling verb is named generically"
    (let [h (cli/make-cli-handler {:wf {:_handler (fn [_] :d) :x (fn [_] :x)}})]
      (is (str/includes? (str (:text (h {:command "wf help"}))) "Bare 'wf' runs its default handler.")))))

(deftest a-command-is-never-qualified-by-the-subdomain-it-names
  (doseq [c ["ss nope" "ss ss help" "plain zzz" "plain help me"]]
    (testing c
      (let [text (str (:text (call c)))
            head (first (str/split c #"\s+"))]
        (is (not (str/includes? text (str head " " c))))))))

(deftest the-reported-defect-shape
  (testing "a contributed ss WITHOUT help/_handler (the old hive-agent) no longer suggests `ss ss help`"
    (let [old (opaque {:ss        {:drain (fn [_] :d) :recent (fn [_] :r)}
                       :goal-plan (fn [_] :g)})
          r   ((cli/make-cli-handler old) {:command "ss help"})]
      (is (not (:isError r)) "AF-06: it lists its verbs instead")
      (is (not (str/includes? (:text r) "ss ss help")))
      (is (str/includes? (:text r) "ss drain")))))

(def ^:private gen-name
  (gen/fmap #(str "r" %) (gen/not-empty gen/string-alphanumeric)))

(defspec never-suggests-a-self-qualified-command 200
  (prop/for-all [roots (gen/vector-distinct gen-name {:min-elements 1 :max-elements 6})
                 token gen-name
                 leaf? gen/boolean]
    (let [r       (first roots)
          handlers (opaque (into {} (map (fn [n] [(keyword n)
                                                  (if leaf? (fn [_] :x) {:only (fn [_] :x)})]))
                                 roots))
          cmd     (str r " " token)
          result  ((cli/make-cli-handler handlers) {:command cmd})
          text    (str (:text result))]
      (not (str/includes? text (str r " " cmd))))))

(def ^:private merged-core
  {:name "ss-help-test-root" :consolidated true :description "Core."
   :inputSchema {:type "object" :properties {"command" {:type "string"}}}})

(def ^:private contributions
  {"ss"    {:handler {:help (fn [_] help-body) :drain (fn [_] :d)}
            :summary "sixth-sense: hear the hivemind"
            :description "long help\nsecond line"}
   "leafy" {:handler (fn [_] :l) :description "Leafy purpose.\nmore detail"}
   "bare"  {:handler (fn [_] :b)}})

(defn- with-contributions [f]
  (ext/contribute-commands! "ss-help-test-root" :ss-help-test-addon contributions)
  (try (f)
       (finally (ext/retract-commands! "ss-help-test-root" :ss-help-test-addon))))

(deftest description-lists-every-routed-contribution-with-its-purpose
  (with-contributions
    (fn []
      (let [desc    (:description (composite/build-merged-tool merged-core))
            routed  (->> (keys (composite/effective-handlers "ss-help-test-root" {}))
                         (map name) set)]
        (testing "non-vacuous universe"
          (is (= #{"ss" "leafy" "bare"} routed)))
        (is (str/starts-with? desc "Core. "))
        (doseq [c routed]
          (is (str/includes? desc c) c))
        (is (str/includes? desc "ss - sixth-sense: hear the hivemind") ":summary wins")
        (is (str/includes? desc "leafy - Leafy purpose.") "first :description line")
        (is (not (str/includes? desc "more detail")))
        (is (not (str/includes? desc "second line")))))))

(deftest composite-dispatch-routes-ss-help
  (with-contributions
    (fn []
      (let [h (composite/build-merged-handler "ss-help-test-root" {})]
        (is (= help-body (h {:command "ss"})))
        (is (= help-body (h {:command "ss help"})))))))

(deftest contribution-summary-caps-and-falls-back
  (is (nil? (composite/contribution-summary {})))
  (is (= "a" (composite/contribution-summary {:summary " " :description "a\nb"})))
  (is (<= (count (composite/contribution-summary {:summary (apply str (repeat 500 "x"))})) 200)))

(deftest addon-contributed-subdomain-is-suggested-only-for-its-own-verbs
  (let [tool  "unknown-hint-test-root"
        core  {:clojure  {:_handler (fn [_] :clj)}
               :analysis {:_handler (fn [_] :an)}}
        h     (composite/build-merged-handler tool core)]
    (ext/contribute-commands! tool :hint-test-carto
                              {"carto"  {:handler (fn [p] [:carto (:command p)])
                                         :verbs   ["multi" "scan" "read-form"]}
                               "cider"  {:handler {:eval (fn [_] :e) :multi (fn [_] :m)}}
                               "silent" {:handler (fn [_] :s)}})
    (try
      (testing "1 match: the declaring contribution is named first and plainly"
        (let [text (:text (h {:command "scan"}))]
          (is (str/starts-with? text "Unknown command: scan. Did you mean: carto scan?"))
          (is (not (str/includes? text "silent scan")))
          (is (not (str/includes? text "clojure scan")))))
      (testing "2+ matches: every root that routes the verb, none that does not"
        (let [text (:text (h {:command "multi"}))]
          (is (str/includes? text "Did you mean: carto multi, cider multi?"))
          (is (not (str/includes? text "analysis multi")))
          (is (not (str/includes? text "silent multi")))))
      (testing "0 matches: the plain list of valid commands, nothing fabricated"
        (is (= "Unknown command: zebra. Valid: analysis, carto, cider, clojure, silent"
               (:text (h {:command "zebra"})))))
      (testing "the qualified form still dispatches to the contribution"
        (is (= [:carto "carto multi"] (h {:command "carto multi"}))))
      (finally (ext/retract-commands! tool :hint-test-carto)))))
