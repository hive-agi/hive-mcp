(ns hive-mcp.embeddings.resilient-health-test
  "Circuit state is exercised through the embedding port with an injected clock
   and provider stub; no live embedding service is required."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.resilient :as res]))

(defn- provider [calls fail? value]
  (reify proto/EmbeddingProvider
    (embed-text [_ _]
      (swap! calls inc)
      (if @fail? (throw (ex-info "down" {})) value))
    (embed-batch [this texts] (mapv #(proto/embed-text this %) texts))
    (embedding-dimension [_] 3)))

(defn circuit-scenario
  "Return call counts after failure, skip, cooldown probe and recovery."
  [cooldown]
  (let [now (atom 0)
        health (atom {})
        primary-calls (atom 0)
        backup-calls (atom 0)
        failing (atom true)
        chain [{:provider (provider primary-calls failing [1.0]) :provider-key :primary}
               {:provider (provider backup-calls (atom false) [2.0]) :provider-key :backup}]
        embedder (res/resilient-embedder chain
                   {:health health :warmth (atom #{}) :clock #(long @now)
                    :cooldown-ms cooldown :budget-ms 1000 :cold-budget-ms 1000
                    :total-budget-ms 10000})
        embed #(proto/embed-text embedder "text")]
    (embed)
    (let [failed @primary-calls]
      (embed)
      (let [skipped @primary-calls]
        (reset! now (* 1000000 (long cooldown)))
        (reset! failing false)
        (let [probe (embed)
              probed @primary-calls]
          (embed)
          {:failed failed :skipped skipped :probed probed
           :final @primary-calls :backup @backup-calls :probe probe
           :health @health})))))

(defn open-circuit-scenario
  "An open circuit is bypassed only if no chain entry was attempted."
  [mode]
  (let [health (atom {[:primary nil] {:until 1000000000}})
        now (atom 0)
        primary-calls (atom 0)
        backup-calls (atom 0)
        primary {:provider (provider primary-calls (atom false) [1.0])
                 :provider-key :primary}
        backup {:provider (provider backup-calls (atom false) [2.0])
                :provider-key :backup}
        chain (if (= mode :single) [primary] [primary backup])
        embedder (res/resilient-embedder chain
                   {:health health :warmth (atom #{}) :clock #(long @now)
                    :cooldown-ms 1000 :budget-ms 1000 :cold-budget-ms 1000
                    :total-budget-ms 10000})]
    {:value (proto/embed-text embedder "text")
     :primary @primary-calls
     :backup @backup-calls
     :health @health}))

(deftrifecta open-circuit-attempts-at-least-once
  hive-mcp.embeddings.resilient-health-test/open-circuit-scenario
  {:golden-path "test/golden/hive-mcp/embeddings/open-circuit-attempts-at-least-once.edn"
   :cases {:single :single :two :two}
   :gen (gen/elements [:single :two])
   :pred (fn [{:keys [value primary backup health]}]
           (or (and (= [1.0] value) (= 1 primary) (zero? backup)
                    (empty? health))
               (and (= [2.0] value) (zero? primary) (= 1 backup)
                    (= {[:primary nil] {:until 1000000000}} health))))
   :num-tests 25
   :mutations [["always-bypass" (fn [_] {:value [1.0] :primary 1 :backup 0
                                           :health {}})]
               ["always-skip" (fn [_] {:value [2.0] :primary 0 :backup 1
                                         :health {[:primary nil] {:until 1000000000}}})]]})

(deftrifecta circuit-skips-and-recovers
  hive-mcp.embeddings.resilient-health-test/circuit-scenario
  {:golden-path "test/golden/hive-mcp/embeddings/circuit-skips-and-recovers.edn"
   :cases {:one 1 :thirty 30000}
   :gen (gen/choose 1 100000)
   :pred (fn [{:keys [failed skipped probed final backup probe health]}]
           (and (= 1 failed skipped)
                (= 2 probed)
                (= 3 final)
                (= 2 backup)
                (= [1.0] probe)
                (empty? health)))
   :num-tests 25
   :mutations [["never-probe" (fn [_] {:failed 1 :skipped 1 :probed 1
                                         :final 1 :backup 2 :probe [2.0]
                                         :health {}})]
               ["never-skip" (fn [_] {:failed 1 :skipped 2 :probed 3
                                        :final 4 :backup 2 :probe [1.0]
                                        :health {}})]]})

(deftest half-open-reservation-is-single-flight
  (let [health (atom {[:primary nil] {:until 10}})
        clock 10]
    (is (true? (#'res/claim-provider! health [:primary nil] clock)))
    (is (false? (#'res/claim-provider! health [:primary nil] clock)))
    (is (= {:probing true} (get @health [:primary nil])))))
