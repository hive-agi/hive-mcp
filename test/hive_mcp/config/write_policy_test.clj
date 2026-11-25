(ns hive-mcp.config.write-policy-test
  "The MCP `config set` write policy, tested as pure data: every input is an
   argument, so nothing is redefined and no file is touched.

     unit      : floor keys (incl. auth-probe, deny-models, provider creation),
                 dotted vector segments, ancestor writes of checked entries,
                 allowlisted writes and their value checks, non-canonical keys,
                 protected names, warn vs enforce, the env mode, addon
                 declarations, the effective allowlist
     property  : lower-case paths at or under a floor prefix, or ancestors of
                 one, are refused with an enforced rule in every mode
     trifecta  : golden refusal shapes, with mutants that accept a floor key
                 or drop the key name from the error"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.config.sensitive :as sensitive]
            [hive-mcp.config.write-policy :as wp]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private cfg
  "A config with two providers and one embedder provider."
  {:llm-providers {:openrouter {:default-model "a/b"} :venice {}}
   :embedder      {:providers {:ollama-qwen3-4b {:impl :ollama}}}})

(def ^:private modes [:warn :enforce])

(defn- decision [mode k v] (:decision (wp/decide mode k v cfg)))

;; =============================================================================
;; Floor
;; =============================================================================

(deftest floor-keys-are-refused-in-every-mode
  (doseq [k ["services.seal.gpg-bin" "addons" "secrets.x"
             "llm-providers.openrouter.api-url" "addons.hive.guard.mode"
             "tool-roots.visible" "embeddings.ollama.host" "formatters.clj"
             "services.kg.backend" "memory.stores.chroma.host"]
          mode modes]
    (let [d (wp/decide mode k "x" cfg)]
      (is (= :refuse (:decision d)) (str k " under " mode))
      (is (contains? #{:floor :ancestor-of-protected} (:rule d)) k)
      (is (= k (:key d)) "the decision names the key")
      (is (some? (:matched d)) "the decision names the protecting path"))))

(deftest every-provider-field-but-the-allowlisted-leaves-is-floor
  (testing "auth-probe would send the provider's secret to any URL"
    (doseq [mode modes]
      (is (= :refuse (decision mode "llm-providers.openrouter.auth-probe"
                               {:kind :openrouter-key-info :url "https://evil/x"})))))
  (testing "deny-models lifts a denial; type would create a provider"
    (doseq [k ["llm-providers.openrouter.deny-models" "llm-providers.evil.type"
               "llm-providers.openrouter.secret-key" "llm-providers.openrouter.dispatch"
               "llm-providers.openrouter.cache-control" "llm-providers.openrouter"]
            mode modes]
      (is (= :refuse (decision mode k [])) k)))
  (is (= [:llm-providers :*] (:matched (wp/classify "llm-providers.openrouter.deny-models" [] cfg))))
  (testing "the three allowlisted leaves stay writable"
    (doseq [leaf ["default-model" "available-models" "model-aliases"]]
      (is (= :allow (decision :enforce (str "llm-providers.openrouter." leaf) "m")) leaf))))

(deftest protected-names-are-floor-in-path-and-value
  (doseq [k ["models.api-key" "agent-defaults.ling.token" "hivemind.private-key"
             "models.keys" "models.auth" "models.pwd" "models.bearer-x"
             "models.cmd" "models.commands" "models.exec" "models.argv"
             "models.bin" "models.path" "models.script" "models.url" "models.host"]
          mode modes]
    (is (= :refuse (decision mode k "x")) k)
    (is (= :floor (:rule (wp/classify k "x" cfg))) k))
  (testing "a map value carrying a protected key is floor"
    (is (= [:command] (:matched (wp/classify "models.fallback" {:command ["rm"]} cfg))))
    (is (= :floor (:rule (wp/classify "models.fallback" [{:api-key "k"}] cfg))))
    (is (= :floor (:rule (wp/classify "models.fallback" {:x {:exec "sh"}} cfg))))))

(deftest a-dotted-vector-segment-cannot-dodge-the-floor
  (doseq [k [[:services.seal :gpg-bin] [:tool-roots.visible]
             [:llm-providers.openrouter.api-url] [:forge.spawn-mode] [:memory.stores]
             [:models.default-model]]
          mode modes]
    (let [d (wp/decide mode k "x" cfg)]
      (is (= :refuse (:decision d)) (pr-str k))
      (is (= :non-canonical (:rule d)) (pr-str k))))
  (testing "the addon id is the one segment that may carry a dot"
    (is (= [:addons :hive.keg :x] (wp/parse-key [:addons :hive.keg :x])))
    (is (nil? (wp/parse-key [:addons :hive.keg :a.b]))))
  (testing "the re-split dotted key is floor-checked too"
    (doseq [k [[:addons :hive.guard :mode] [:addons :hive :guard]]]
      (is (wp/floored? k) (pr-str k))
      (is (= :refuse (decision :warn k :shadow)) (pr-str k)))))

(deftest ancestor-writes-are-refused
  (doseq [k ["services" "llm-providers" "memory" "addons" "embedder" "services.agent"]
          mode modes]
    (let [d (wp/decide mode k {:x 1} cfg)]
      (is (= :refuse (:decision d)) (str k " under " mode))
      (is (contains? #{:ancestor-of-protected :floor} (:rule d)) k)))
  (is (= :ancestor-of-protected (:rule (wp/classify "llm-providers" {} cfg)))))

(deftest an-ancestor-of-a-checked-entry-is-an-enforced-refusal
  (doseq [[k v matched] [["agent-defaults.ling" {:provider :evil} [:agent-defaults :* :provider]]
                         ["agent-defaults" {:ling {:provider :evil}} [:agent-defaults :* :provider]]
                         ["embedder.routes" {:note :nowhere} [:embedder :routes :*]]]
          mode modes]
    (let [d (wp/decide mode k v cfg)]
      (is (= :refuse (:decision d)) (str k " under " mode))
      (is (= :ancestor-of-protected (:rule d)) k)
      (is (= matched (:matched d)) k))))

;; =============================================================================
;; Allowlist
;; =============================================================================

(deftest allowlisted-writes-are-allowed
  (doseq [[k v] [["models.default-model" "x/y"]
                 ["models.fallback-models" ["a" "b"]]
                 ["agent-defaults.ling.model" "m"]
                 ["agent-defaults.drone.provider" :venice]
                 ["agent-defaults.drone.provider" "openrouter"]
                 ["llm-providers.openrouter.default-model" "m"]
                 ["llm-providers.venice.model-aliases" {:fast "m"}]
                 ["embedder.default" :ollama-qwen3-4b]
                 ["embedder.routes.note" "ollama-qwen3-4b"]
                 ["embeddings.ollama.model" "qwen3"]
                 ["embeddings.warmup.enabled" true]
                 ["hivemind.shout-message-cap" 4096]
                 ["services.scheduler.interval-minutes" 30]
                 ["services.agent.reasoning" :high]
                 ["services.agent.chat-points.gc.keep-days" 7]]
          mode modes]
    (let [d (wp/decide mode k v cfg)]
      (is (= :allow (:decision d)) (str k " under " mode))
      (is (= :allowlisted (:rule d)) k))))

(deftest unknown-provider-is-refused-in-every-mode
  (doseq [[k v] [["agent-defaults.ling.provider" :evil]
                 ["llm-providers.evil.default-model" "m"]
                 ["embedder.default" :nowhere]
                 ["embedder.routes.note" :nowhere]]
          mode modes]
    (let [d (wp/decide mode k v cfg)]
      (is (= :refuse (:decision d)) (str k " under " mode))
      (is (= :bad-value (:rule d)) k))))

(deftest a-bad-value-message-never-quotes-the-value
  (let [d (wp/decide :warn "agent-defaults.ling.provider" "sk-live-SECRET" cfg)]
    (is (= :bad-value (:rule d)))
    (is (not (str/includes? (:reason d) "sk-live")))
    (is (not (str/includes? (pr-str (wp/refusal-error d)) "sk-live")))
    (is (str/includes? (:reason d) "agent-defaults.ling.provider"))))

(deftest non-canonical-keys-are-refused
  (doseq [k [":services.seal.x" "SERVICES.seal" "services..seal" "services.seal."
             ".models" "models.*" "" "models default" "Models.default-model"
             "Secrets.openrouter" ":secrets.x"]
          mode modes]
    (let [d (wp/decide mode k "x" cfg)]
      (is (= :refuse (:decision d)) (pr-str k))
      (is (= :non-canonical (:rule d)) (pr-str k))
      (is (nil? (:path d)) (pr-str k))))
  (doseq [k [[] ["models"] [:Models :x] [:a/b] [:models :*]]]
    (is (= :non-canonical (:rule (wp/classify k "x" cfg))) (pr-str k))))

(deftest warn-mode-would-refuse-only-a-non-allowlisted-key
  (doseq [k ["foo.bar" "models" "hivemind"]]
    (is (= :would-refuse (decision :warn k 1)) k)
    (is (= :refuse (decision :enforce k 1)) k)
    (is (= :not-allowlisted (:rule (wp/decide :warn k 1 cfg))) k))
  (testing "every other refusal rule refuses under :warn"
    (doseq [rule (disj (set (rest wp/Rule)) :allowlisted :addon-declared :not-allowlisted)]
      (is (contains? wp/enforced-rules rule) rule)
      (is (= :refuse (:decision (wp/apply-mode :warn {:decision :refuse :rule rule :reason ""
                                                      :key "k" :path [:k] :matched nil})))
          rule))))

;; =============================================================================
;; Mode
;; =============================================================================

(deftest parse-mode-fails-closed
  (is (= :warn wp/default-mode))
  (doseq [v [nil "" "   "]]
    (is (= wp/default-mode (wp/parse-mode v)) (pr-str v)))
  (doseq [v ["warn" " :WARN " "Warn"]]
    (is (= :warn (wp/parse-mode v)) v))
  (doseq [v ["enforce" "enforc" "strict" "off" "false" "0" ":warnx"]]
    (is (= :enforce (wp/parse-mode v)) v)))

(defspec any-non-blank-value-but-warn-enforces 300
  (prop/for-all [v gen/string-ascii]
    (let [norm (-> v str/trim str/lower-case (str/replace #"^:" ""))]
      (= (cond (str/blank? v) wp/default-mode
               (= "warn" norm) :warn
               :else :enforce)
         (wp/parse-mode v)))))

(deftest the-mode-is-never-read-from-config
  (testing "a config that names a mode changes no decision"
    (let [loose (assoc cfg :config-set {:mode :off} :config {:set-mode "warn"})]
      (doseq [mode modes]
        (is (= (wp/decide mode "foo.bar" 1 cfg) (wp/decide mode "foo.bar" 1 loose))))))
  (testing "current-mode is exactly parse-mode of the env var"
    (is (= (wp/parse-mode (System/getenv wp/mode-env-var)) (wp/current-mode)))))

;; =============================================================================
;; Addon declarations
;; =============================================================================

(def ^:private declarations
  {:hive.keg   [[:addons :hive.keg :sync-interval-minutes]
                [:addons :hive.rss :fetches-per-day]
                [:models :default-model]
                [:addons :hive.keg :token]
                [:addons :hive.keg :*]
                [:addons :hive.keg #{:x :token}]]
   :hive.guard [[:addons :hive.guard :mode]]
   "hive.rss"  [[:addons :hive.rss :fetches-per-day]]
   :*          [[:addons :hive.blender :code-gate]]
   "*"         [[:addons :hive.blender :code-gate]]
   :a/b        [[:addons :a/b :x]]})

(deftest every-addon-section-is-floor-until-declared
  (doseq [k ["addons.hive.blender.code-gate" "addons.hive.keg.sync-interval-minutes"
             "addons.hive" "addons.hive.keg"]
          mode modes]
    (is (= :refuse (decision mode k 1)) k))
  (is (= [:addons :*] (:matched (wp/classify "addons.hive.blender.code-gate" 1 cfg)))))

(deftest addon-declaration-opens-its-own-subtree
  (let [d (wp/decide :enforce [:addons :hive.keg :sync-interval-minutes] 30 cfg declarations)]
    (is (= :allow (:decision d)))
    (is (= :addon-declared (:rule d)))
    (is (= "addons.hive.keg.sync-interval-minutes" (:key d))))
  (is (= :allow (:decision (wp/decide :enforce [:addons :hive.rss :fetches-per-day] 4 cfg
                                      declarations)))
      "a string addon id is accepted for its own subtree")
  (is (= :refuse (:decision (wp/decide :enforce [:addons :hive.keg :sync-interval-minutes] 30 cfg)))
      "without the declaration the same key is refused"))

(deftest addon-declaration-outside-its-subtree-is-ignored
  (let [{:keys [accepted dropped]} (wp/addon-entries declarations)
        dropped-paths (set (map :path dropped))]
    (is (contains? dropped-paths [:models :default-model]))
    (is (= [{:path [:addons :hive.rss :fetches-per-day] :source :hive.rss}]
           (filter #(= [:addons :hive.rss :fetches-per-day] (:path %)) accepted))
        "only hive.rss's own declaration of its subtree is accepted")
    (is (some #(str/includes? (:reason %) "own subtree") dropped))))

(deftest a-wildcard-or-non-canonical-addon-id-opens-nothing
  (let [{:keys [accepted dropped]} (wp/addon-entries declarations)]
    (is (not-any? #(contains? #{:* "*" :a/b} (:source %)) accepted))
    (is (= 3 (count (filter #(contains? #{:* "*" :a/b} (:addon %)) dropped))))
    (doseq [mode modes]
      (is (= :refuse (:decision (wp/decide mode [:addons :hive.blender :code-gate] 1 cfg
                                           declarations)))))))

(deftest a-declaration-must-be-a-literal-path
  (let [dropped (set (map :path (:dropped (wp/addon-entries declarations))))]
    (is (contains? dropped [:addons :hive.keg :*]))
    (is (contains? dropped [:addons :hive.keg #{:x :token}])))
  (is (= :refuse (decision :enforce [:addons :hive.keg :anything] 1))))

(deftest floor-beats-any-declaration
  (let [dropped (set (map :path (:dropped (wp/addon-entries declarations))))]
    (is (contains? dropped [:addons :hive.guard :mode]))
    (is (contains? dropped [:addons :hive.keg :token])))
  (doseq [mode modes]
    (is (= :refuse (:decision (wp/decide mode [:addons :hive.guard :mode] :shadow cfg declarations))))
    (is (= :refuse (:decision (wp/decide mode [:addons :hive.keg :token] "t" cfg declarations))))))

(deftest effective-allowlist-lists-central-then-addon-entries
  (let [central (wp/effective-allowlist)
        full    (wp/effective-allowlist declarations)
        addon   (subvec full (count central))]
    (is (= (count wp/central-allowlist) (count central)))
    (is (every? #(= :central (:source %)) central))
    (is (= central (subvec full 0 (count central))))
    (is (= #{{:path [:addons :hive.keg :sync-interval-minutes] :source :hive.keg}
             {:path [:addons :hive.rss :fetches-per-day] :source :hive.rss}}
           (set addon))
        "exactly the literal, own-subtree, non-floor declarations")
    (is (not-any? #(wp/floored? (:path %)) addon))))

(deftest one-sensitive-name-regex-is-shared
  (require 'hive-mcp.tools.consolidated.config)
  (let [tool-sensitive? @(resolve 'hive-mcp.tools.consolidated.config/sensitive-key?)]
    (doseq [n ["api-key" "token" "auth" "pwd" "bearer-token" "client-secret" "keys"
               "default-model" "monkey" "keystone-model" "author-name"]]
      (is (= (sensitive/sensitive-name? n) (tool-sensitive? n))
          (str "the config tool's redaction agrees on " n))
      (when (sensitive/sensitive-name? n)
        (is (wp/protected-name? n) (str "the write policy protects " n)))))
  (doseq [n ["api-key" "token" "auth" "pwd" "bearer-token" "client-secret" "keys"]]
    (is (sensitive/sensitive-name? n) n))
  (doseq [n ["default-model" "monkey" "keystone-model" "author-name"]]
    (is (not (sensitive/sensitive-name? n)) n)))

;; =============================================================================
;; Properties: the floor holds in every mode, for canonical paths
;; =============================================================================

(def ^:private gen-seg
  "A canonical lower-case segment, so a generated path is never refused for
   being non-canonical."
  (gen/fmap (fn [[c cs]] (keyword (apply str c cs)))
            (gen/tuple (gen/elements (seq "abcdefghijklmnopqrstuvwxyz"))
                       (gen/vector (gen/elements (seq "abcdefghijklmnopqrstuvwxyz0123456789-")) 0 8))))

(defn- gen-instance
  "A literal path matching `pattern`."
  [pattern]
  (apply gen/tuple
         (map (fn [p] (cond (= :* p) gen-seg
                            (set? p) (gen/elements (vec p))
                            :else    (gen/return p)))
              pattern)))

(def ^:private gen-floor-path
  (gen/let [{:keys [path except]} (gen/elements (into wp/hard-floor wp/addon-floor))
            base   (gen-instance path)
            suffix (gen/vector gen-seg 0 3)
            value  (gen/one-of [gen/small-integer gen/string-alphanumeric
                                (gen/map gen/keyword gen/small-integer)])]
    {:path (into base suffix) :except except :value value}))

(def ^:private enforced-refusals #{:floor :ancestor-of-protected})

(defspec any-path-under-a-floor-prefix-is-refused-in-every-mode 500
  (prop/for-all [{:keys [path except value]} gen-floor-path]
    (or (some #(wp/at-or-under? % path) except)
        (and (= path (wp/parse-key path))
             (every? (fn [mode]
                       (let [d (wp/decide mode path value cfg)]
                         (and (= :refuse (:decision d))
                              (contains? enforced-refusals (:rule d)))))
                     modes)))))

(defspec any-ancestor-of-a-floor-path-is-refused-in-every-mode 300
  (prop/for-all [{:keys [path]} gen-floor-path
                 n (gen/choose 1 6)]
    (let [prefix (vec (take (min n (count path)) path))]
      (every? (fn [mode]
                (let [d (wp/decide mode prefix {:x 1} cfg)]
                  (and (= :refuse (:decision d))
                       (contains? enforced-refusals (:rule d)))))
              modes))))

(defspec a-generated-path-is-canonical 200
  (prop/for-all [{:keys [path]} gen-floor-path]
    (= path (wp/parse-key path))))

;; =============================================================================
;; Trifecta: refusal shapes
;; =============================================================================

(defn run-refusal
  "Unary adapter: {:key k :value v} -> the refusal error under :enforce, or
   {:allowed key} when the write is allowed."
  [{:keys [key value]}]
  (let [d (wp/decide :enforce key value cfg)]
    (if (= :allow (:decision d))
      {:allowed (:key d)}
      (wp/refusal-error d))))

(defn- names-key?
  [{:keys [key]} out]
  (or (contains? out :allowed)
      (and (= key (:key out)) (str/includes? (:message out) key))))

(def ^:private gen-key-input
  (gen/let [{:keys [path]} gen-floor-path
            v gen/small-integer]
    {:key (wp/key-string path) :value v}))

(deftrifecta refusal-shapes
  hive-mcp.config.write-policy-test/run-refusal
  {:golden-path "test/golden/config/write-policy-refusals.edn"
   :cases       {:seal-gpg-bin     {:key "services.seal.gpg-bin" :value "/tmp/gpg"}
                 :addons-root      {:key "addons" :value {}}
                 :secret           {:key "secrets.x" :value "v"}
                 :provider-api-url {:key "llm-providers.openrouter.api-url" :value "http://evil"}
                 :auth-probe       {:key "llm-providers.openrouter.auth-probe" :value {:url "http://evil"}}
                 :services-root    {:key "services" :value {}}
                 :checked-ancestor {:key "agent-defaults.ling" :value {:provider :evil}}
                 :unknown-provider {:key "agent-defaults.ling.provider" :value :evil}
                 :not-allowlisted  {:key "foo.bar" :value 1}
                 :non-canonical    {:key "SERVICES.seal" :value 1}
                 :addon-undeclared {:key "addons.hive.blender.code-gate" :value 1}
                 :allowed          {:key "models.default-model" :value "a/b"}}
   :gen         gen-key-input
   :pred        map?
   :num-tests   200
   :mutations   [["accepts floor key"
                  (fn [{:keys [key]}] {:allowed key})]
                 ["drops key name from error"
                  (fn [{:keys [key value]}]
                    (let [d (wp/decide :enforce key value cfg)]
                      (if (= :allow (:decision d))
                        {:allowed (:key d)}
                        (-> (wp/refusal-error d)
                            (dissoc :key)
                            (update :message str/replace key "<key>")))))]]})

(defspec every-floor-refusal-names-its-key-and-rule 300
  (prop/for-all [in gen-key-input]
    (let [out (run-refusal in)]
      (and (not (contains? out :allowed))
           (names-key? in out)
           (contains? enforced-refusals (:rule out))))))
