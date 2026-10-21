(ns hive-mcp.config.merge-test
  "Unit tests for hive-mcp.config.merge/default-config — pinning defaults
   for the dedicated carto vector store (qdrant-carto) and the
   :carto-store service key."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.config.merge :as merge]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest default-config-has-qdrant-carto
  (testing ":services :qdrant-carto defaults are present and well-formed"
    (let [svc (get-in merge/default-config [:services :qdrant-carto])]
      (is (map? svc) ":qdrant-carto service entry must exist")
      (is (= :local (:mode svc)))
      (is (= "localhost" (:host svc)))
      (is (= 6333 (:port svc)))
      (is (= "carto-snippets" (:collection svc)))
      (is (not (contains? svc :embedding))
          "the carto embedding provider and model come from user config, not defaults"))))

(deftest default-config-ships-no-model-choice
  (testing "no model id, model list or provider choice lives in default-config"
    (is (not (contains? merge/default-config :agent-defaults)))
    (is (not (contains? merge/default-config :models)))
    (is (not (contains? (:services merge/default-config) :drone)))
    (is (not (contains? (get-in merge/default-config [:services :ollama]) :model)))
    (is (not (contains? (:embedder merge/default-config) :default)))
    (is (not (contains? (:embedder merge/default-config) :providers)))
    (is (nil? (get-in merge/default-config [:embeddings :ollama :model])))
    (is (not (contains? (:embeddings merge/default-config) :openrouter))))

  (testing "so a user's own model choices are the only ones after deep-merge"
    (let [user   {:agent-defaults {:ling {:provider :venice :model "user-model"}}}
          merged (merge/deep-merge merge/default-config user)]
      (is (= user (select-keys merged [:agent-defaults]))))))

(deftest default-config-has-carto-store
  (testing ":services :carto-store defaults to :qdrant-carto backend"
    (let [svc (get-in merge/default-config [:services :carto-store])]
      (is (map? svc) ":carto-store service entry must exist")
      (is (= :qdrant-carto (:backend svc))))))

(deftest deep-merge-preserves-user-carto-overrides
  (testing "user config overrides for :qdrant-carto survive deep-merge"
    (let [user {:services {:qdrant-carto {:host "qdrant.internal"
                                          :port 16333}}}
          merged (merge/deep-merge merge/default-config user)
          svc (get-in merged [:services :qdrant-carto])]
      ;; user wins
      (is (= "qdrant.internal" (:host svc)))
      (is (= 16333 (:port svc)))
      ;; defaults still filled in
      (is (= "carto-snippets" (:collection svc)))
      ;; carto-store backend default still present
      (is (= :qdrant-carto (get-in merged [:services :carto-store :backend]))))))

;; =============================================================================
;; Facets: deep-merge and parse-key-path, driven by their contracts
;; =============================================================================

(def ^:private K [:enum :a :b])

;; the map arm is listed twice so nested maps meet nested maps often enough
;; for the :nested-merge variant to clear its floor
(def ^:private Leaf [:or :int [:map-of {:max 3} K :int] [:map-of {:min 1 :max 2} K :int]])

(def ^:private Cfg
  "Two levels of small configs, so keys collide often and the map/map,
   map/scalar and scalar/map arms of the merge are all reached."
  [:map-of {:max 3} K Leaf])

(defn- merged-oracle
  "Independent statement of the docstring: user wins at every level,
   missing keys are filled from defaults at any depth. With no defaults
   the user config comes back as given (nil stays nil)."
  [d u]
  (if (empty? d)
    u
    (into {}
          (for [k (distinct (concat (keys d) (keys u)))]
            (let [dv (get d k) uv (get u k)]
              [k (cond (not (contains? u k))      dv
                       (and (map? dv) (map? uv))  (merged-oracle dv uv)
                       :else                      uv)])))))

(deftrifecta-from-schema deep-merge-user-wins-defaults-fill
  #'merge/deep-merge
  {:in [:cat Cfg [:maybe Cfg]]
   :out [:maybe merge/ConfigMap]
   :rel (fn [[d u] out] (= (merged-oracle d u) out))
   :classify (fn [[d u] _]
               (cond (nil? u) :no-user
                     (some (fn [[k v]] (and (map? v) (map? (get u k)))) d) :nested-merge
                     :else :flat))
   :classify-domain #{:no-user :nested-merge :flat}
   :golden-path "test/golden/config/merge-deep-merge.edn"
   :num-tests 300})

;; Segments are non-blank so every parsed keyword reads back as EDN in the
;; golden; the blank-segment case is pinned in parse-key-path-keeps-empty-segments.
(def ^:private DottedKey
  [:or [:enum nil "" "  "]
   [:string {:gen/schema [:vector {:min 1 :max 4} [:enum "a" "kg" "port"]]
             :gen/fmap #(str/join "." %)}]])

(deftrifecta-from-schema parse-key-path-splits-on-dots
  #'merge/parse-key-path
  {:in [:cat DottedKey]
   :out [:maybe merge/KeyPath]
   :rel (fn [[s] out]
          (if (str/blank? s)
            (nil? out)
            ;; str/split drops trailing empty segments only
            (= (str/replace s #"\.+$" "") (str/join "." (map name out)))))
   :classify (fn [[s] _] (if (str/blank? s) :blank :path))
   :classify-domain #{:blank :path}
   :golden-path "test/golden/config/merge-parse-key-path.edn"
   :num-tests 300})

(deftest parse-key-path-keeps-empty-segments
  (is (= [:a (keyword "") :b] (merge/parse-key-path "a..b")))
  (is (= [:a] (merge/parse-key-path "a.")) "trailing empty segments are dropped"))
