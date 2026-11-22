(ns hive-mcp.tools.consolidated.config-set-log-test
  "`config set` logs the key and the value's shape, never the value: a value
   under a key that names no secret can still carry one. Logs are captured
   through a timbre appender bound with timbre's own config, so nothing is
   redefined."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.merge :as merge]
            [hive-mcp.config.sensitive :as sensitive]
            [hive-mcp.config.source :as src]
            [hive-mcp.test.stub.log-capture :as logs]
            [hive-mcp.tools.consolidated.config :as config-tool]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private secret "sk-live-SECRET")

(def ^:private global-config @#'config/global-config)

(defn- isolate-global-config [f]
  (let [saved @global-config]
    (try (f) (finally (reset! global-config saved)))))

(use-fixtures :each isolate-global-config)

(def ^:private leaky-writes
  "Writes whose key names no secret but whose value carries one."
  [{:key "models.extra" :value {:name "m" :opts {:api-key secret :nested [{:token secret}]}}}
   {:key "models.extra" :value (pr-str {:auth {:password secret}})}
   {:key "embeddings.ollama.host" :value (str "https://x/api?token=" secret)}
   {:key "models.default-model" :value [secret]}])

(defn- logged-text
  "Every captured event, printed: what any appender could have written."
  [sink]
  (pr-str (mapv :vargs @sink)))

(defn- set-through-tool [params]
  (binding [config/*config-source* (src/atom-source (atom merge/default-config)
                                                    merge/default-config)]
    (config-tool/handle-config (assoc params :command "set"))))

(deftest config-set-logs-no-secret
  (doseq [w leaky-writes]
    (testing (:key w)
      (logs/with-captured-logs sink
        (set-through-tool w)
        (is (seq @sink) "the write is logged")
        (is (not (str/includes? (logged-text sink) "SECRET")))
        (is (some #(and (= "Config set:" (first (:vargs %)))
                        (= (:key w) (second (:vargs %))))
                  @sink)
            "the line still names the key")))))

(deftest a-persisted-write-logs-no-secret
  (testing "the file-backed branch of set-config-value! logs only the shape too"
    (let [f (doto (java.io.File/createTempFile "hive-config-set-log" ".edn") .deleteOnExit)]
      (spit f "{}")
      (reset! global-config {})
      (doseq [{:keys [key value]} leaky-writes]
        (logs/with-captured-logs sink
          (config/set-config-value! key value (.getPath f))
          (is (some #(= "Config updated:" (first (:vargs %))) @sink))
          (is (not (str/includes? (logged-text sink) "SECRET")) key))))))

(deftest value-shape-is-type-and-size-only
  (is (= {:type "string" :size 14} (sensitive/value-shape secret)))
  (is (= {:type "map" :size 2} (sensitive/value-shape {:a secret :b 1})))
  (is (= {:type "vector" :size 1} (sensitive/value-shape [secret])))
  (is (= {:type "set" :size 1} (sensitive/value-shape #{secret})))
  (is (= {:type "list" :size 1} (sensitive/value-shape (list secret))))
  (is (= {:type "nil"} (sensitive/value-shape nil)))
  (is (= {:type "number"} (sensitive/value-shape 42)))
  (is (= {:type "keyword"} (sensitive/value-shape :x)))
  (is (= {:type "boolean"} (sensitive/value-shape false)))
  (is (not (str/includes? (pr-str (sensitive/value-shape (java.net.URI. (str "https://x?token=" secret)))) "SECRET"))))
