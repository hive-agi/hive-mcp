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
