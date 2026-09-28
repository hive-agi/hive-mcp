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
            [hive-mcp.tools.composite :as composite]))

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

(deftest a-subtree-without-help-errors-with-its-listing
  (doseq [c ["plain" "plain help"]]
    (testing c
      (let [r (call c)]
        (is (:isError r))
        (is (str/includes? (:text r) "'plain' routes: plain a, plain b"))
        (is (not (str/includes? (:text r) "plain plain"))))))
  (testing "a deeper miss is not mistaken for help"
    (let [r (call "plain zzz")]
      (is (:isError r))
      (is (str/includes? (:text r) "plain a")))))

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
      (is (:isError r))
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
