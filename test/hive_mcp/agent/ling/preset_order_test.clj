(ns hive-mcp.agent.ling.preset-order-test
  (:require [clojure.test :refer [is deftest]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.agent.ling.spawn :as spawn]
            [hive-mcp.agent.ling.spawn-store :as store]))

;; The scalar is written via the spawn-store port so either backend retains
;; caller precedence; the legacy many-valued attribute is never an order source.
(deftrifecta declared-preset-order
  hive-mcp.agent.ling.spawn/ordered-presets
  {:golden-path "test/golden/agent/ling/preset-order.edn"
   :cases {:declared {:slave/presets #{"alpha" "beta" "gamma"}
                     :slave/preset-order "[\"beta\" \"gamma\" \"alpha\"]"}
           :legacy {:slave/presets #{"gamma" "alpha" "beta"}}
           :stale {:slave/presets #{"alpha" "beta"}
                   :slave/preset-order "[\"beta\" \"old\"]"}
           :empty {}}
   :gen (gen/let [names (gen/shuffle ["zeta" "alpha" "mid" "beta" "gamma"])]
          {:slave/presets (set names)
           :slave/preset-order (pr-str names)})
   :pred #(and (vector? %) (every? string? %))
   :num-tests 100
   :mutations [["hash iteration decides precedence"
                (fn [row] (vec (:slave/presets row)))]
               ["alphabetical order overrides caller precedence"
                (fn [row] (vec (sort (:slave/presets row))))]]
   :assert (fn []
             (is (= ["beta" "gamma" "alpha"]
                    (spawn/ordered-presets
                     {:slave/presets #{"alpha" "beta" "gamma"}
                      :slave/preset-order "[\"beta\" \"gamma\" \"alpha\"]"})))
             (is (= ["alpha" "beta"]
                    (spawn/ordered-presets {:slave/presets #{"beta" "alpha"}}))))})

(deftest registration-writes-declared-order-through-port
  (let [writes (atom [])
        backend (reify store/ISpawnStore
                  (add-slave! [_ id attrs] (swap! writes conj [:add id attrs]))
                  (update-slave! [_ id attrs] (swap! writes conj [:update id attrs]))
                  (remove-slave! [_ _])
                  (claims-for-slave [_ _] []))]
    (#'spawn/register-slave-row! backend "ling" {:presets ["gamma" "alpha" "beta"]})
    (is (= [:update "ling" {:slave/preset-order "[\"gamma\" \"alpha\" \"beta\"]"}]
           (second @writes)))))
