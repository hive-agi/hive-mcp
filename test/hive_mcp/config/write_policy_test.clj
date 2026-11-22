(ns hive-mcp.config.write-policy-test
  "The MCP `config set` write policy, tested as pure data: every input is an
   argument, so nothing is redefined and no file is touched.

     unit      : floor keys, ancestor writes, allowlisted writes and their
                 value checks, non-canonical keys, warn vs enforce, addon
                 declarations, the effective allowlist
     property  : any path at or under a floor prefix is refused in every mode
     trifecta  : golden refusal shapes, with mutants that accept a floor key
                 or drop the key name from the error"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
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

(deftest sensitive-and-command-segments-are-floor
  (doseq [k ["models.api-key" "agent-defaults.ling.token"
             "addons.hive.keg.command" "hivemind.private-key" "models.keys"]]
    (is (= :floor (:rule (wp/classify k "x" cfg))) k))
  (testing "a map value carrying a :command or secret key is floor"
    (is (= [:command] (:matched (wp/classify "models.fallback" {:command ["rm"]} cfg))))
    (is (= :floor (:rule (wp/classify "models.fallback" [{:api-key "k"}] cfg))))))

(deftest ancestor-writes-are-refused
  (doseq [k ["services" "llm-providers" "memory" "addons" "embedder" "services.agent"]
          mode modes]
    (let [d (wp/decide mode k {:x 1} cfg)]
      (is (= :refuse (:decision d)) (str k " under " mode))
      (is (contains? #{:ancestor-of-protected :floor} (:rule d)) k)))
  (is (= :ancestor-of-protected (:rule (wp/classify "llm-providers" {} cfg)))))

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

(deftest non-canonical-keys-are-refused
  (doseq [k [":services.seal.x" "SERVICES.seal" "services..seal" "services.seal."
             ".models" "models.*" "" "models default" "Models.default-model"]
          mode modes]
    (let [d (wp/decide mode k "x" cfg)]
      (is (= :refuse (:decision d)) (pr-str k))
      (is (= :non-canonical (:rule d)) (pr-str k))))
  (is (= :non-canonical (:rule (wp/classify [] "x" cfg))))
  (is (= :non-canonical (:rule (wp/classify ["models"] "x" cfg)))))

(deftest warn-mode-would-refuse-a-non-floor-non-allowlisted-key
  (doseq [k ["foo.bar" "models" "hivemind" "addons.hive.keg.sync-interval-minutes"]]
    (is (= :would-refuse (decision :warn k 1)) k)
    (is (= :refuse (decision :enforce k 1)) k)
    (is (= :not-allowlisted (:rule (wp/decide :warn k 1 cfg))) k)))

;; =============================================================================
;; Mode
;; =============================================================================

(deftest mode-comes-from-code-or-env-value
  (is (= :warn wp/default-mode))
  (is (= :enforce (wp/parse-mode "enforce")))
  (is (= :enforce (wp/parse-mode " :ENFORCE ")))
  (is (= :warn (wp/parse-mode "warn")))
  (is (= wp/default-mode (wp/parse-mode nil)))
  (is (= wp/default-mode (wp/parse-mode "off")))
  (is (contains? #{:warn :enforce} (wp/current-mode))))

(defspec parse-mode-always-answers-a-mode 200
  (prop/for-all [v gen/any-printable-equatable]
    (contains? #{:warn :enforce} (wp/parse-mode v))))

;; =============================================================================
;; Addon declarations
;; =============================================================================

(def ^:private declarations
  {:hive.keg   [[:addons :hive.keg :sync-interval-minutes]
                [:addons :hive.rss :fetches-per-day]
                [:models :default-model]
                [:addons :hive.keg :token]]
   :hive.guard [[:addons :hive.guard :mode]]
   "hive.rss"  [[:addons :hive.rss :fetches-per-day]]})

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
    (is (= 1 (count (filter #(= [:addons :hive.rss :fetches-per-day] (:path %)) accepted)))
        "only hive.rss's own declaration of its subtree is accepted")
    (is (some #(str/includes? (:reason %) "own subtree") dropped)
        "a dropped declaration says why")
    (is (not-any? #(= :hive.keg (:source %))
                  (filter #(= [:addons :hive.rss :fetches-per-day] (:path %)) accepted)))))

(deftest floor-beats-any-declaration
  (let [dropped (set (map :path (:dropped (wp/addon-entries declarations))))]
    (is (contains? dropped [:addons :hive.guard :mode]))
    (is (contains? dropped [:addons :hive.keg :token])))
  (doseq [mode modes]
    (is (= :refuse (:decision (wp/decide mode [:addons :hive.guard :mode] :shadow cfg declarations))))
    (is (= :refuse (:decision (wp/decide mode [:addons :hive.keg :token] "t" cfg declarations))))))

(deftest effective-allowlist-lists-central-then-addon-entries
  (let [central (wp/effective-allowlist)
        full    (wp/effective-allowlist declarations)]
    (is (= (count wp/central-allowlist) (count central)))
    (is (every? #(= :central (:source %)) central))
    (is (= central (subvec full 0 (count central))))
    (is (some #(= {:path [:addons :hive.keg :sync-interval-minutes] :source :hive.keg} %) full))
    (is (not-any? #(wp/floored? (:path %)) full)
        "no effective entry lies on the floor")))

;; =============================================================================
;; Property: the floor holds in every mode
;; =============================================================================

(def ^:private gen-seg
  (gen/fmap keyword (gen/not-empty gen/string-alphanumeric)))

(defn- gen-instance
  "A literal path matching `pattern`."
  [pattern]
  (apply gen/tuple
         (map (fn [p] (cond (= :* p) gen-seg
                            (set? p) (gen/elements (vec p))
                            :else    (gen/return p)))
              pattern)))

(def ^:private gen-floor-path
  (gen/let [{:keys [path except]} (gen/elements wp/floor)
            base   (gen-instance path)
            suffix (gen/vector gen-seg 0 3)
            value  (gen/one-of [gen/small-integer gen/string-alphanumeric
                                (gen/map gen/keyword gen/small-integer)])]
    {:path (into base suffix) :except except :value value}))

(defspec any-path-under-a-floor-prefix-is-refused-in-every-mode 500
  (prop/for-all [{:keys [path except value]} gen-floor-path]
    (or (some #(wp/at-or-under? % path) except)
        (every? #(= :refuse (:decision (wp/decide % path value cfg)))
                modes))))

(defspec any-ancestor-of-a-floor-path-is-refused-in-every-mode 300
  (prop/for-all [{:keys [path]} gen-floor-path
                 n (gen/choose 1 6)]
    (let [prefix (vec (take (min n (count path)) path))]
      (every? #(= :refuse (:decision (wp/decide % prefix {:x 1} cfg))) modes))))

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
                 :services-root    {:key "services" :value {}}
                 :unknown-provider {:key "agent-defaults.ling.provider" :value :evil}
                 :not-allowlisted  {:key "foo.bar" :value 1}
                 :non-canonical    {:key "SERVICES.seal" :value 1}
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

(defspec every-refusal-names-its-key-and-rule 300
  (prop/for-all [in (gen/one-of [gen-key-input
                                 (gen/fmap (fn [s] {:key s :value 1}) gen/string-ascii)])]
    (let [out (run-refusal in)]
      (and (names-key? in out)
           (or (contains? out :allowed) (keyword? (:rule out)))))))
