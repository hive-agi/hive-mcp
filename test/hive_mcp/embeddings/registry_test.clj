(ns hive-mcp.embeddings.registry-test
  "The provider cache-hit line fires on every embed. At DEBUG it floods a bulk
   ingest with one identical line per chunk, so it must log at TRACE.

   The factory is a stub registered through the registry's own port
   (`register-factory!`); the real Venice factory is put back through
   `register-venice!`. Log lines are captured by a timbre appender passed in
   with `with-config`, so nothing global is redefined."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.embeddings.config :as config]
            [hive-mcp.embeddings.registry :as registry]
            [taoensso.timbre :as timbre]
            [hive-mcp.embeddings.protocol]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- capture-config
  "Timbre config whose only appender records [level message] into `sink`."
  [min-level sink]
  {:min-level min-level
   :appenders {:capture {:enabled? true
                         :fn (fn [{:keys [level vargs]}]
                               (swap! sink conj [level (apply str (interpose " " vargs))]))}}})

(defn- cache-hit-lines
  "Call get-provider twice (miss then hit) under `min-level`; return the
   captured cache-hit lines."
  [min-level cfg]
  (let [sink (atom [])]
    (timbre/with-config (capture-config min-level sink)
      (registry/get-provider cfg)
      (registry/get-provider cfg))
    (filterv #(.contains ^String (second %) "Using cached provider") @sink)))

(deftest the-cache-hit-line-is-trace-not-debug
  (let [calls (atom [])
        stub (reify hive-mcp.embeddings.protocol/EmbeddingProvider
               (embed-text [_ text] (swap! calls conj text) [1.0])
               (embed-batch [_ texts] (swap! calls into texts) (mapv (constantly [1.0]) texts))
               (embedding-dimension [_] 1))
        cfg (config/->EmbeddingConfig :venice
                                     (str "registry-test-" (random-uuid))
                                     1 {})
        had-venice? (some #{:venice} (registry/list-factories))]
    (try
      (registry/register-factory! :venice (constantly stub))
      (testing "cache hits return the same gated instance, never the raw stub"
        (let [a (registry/get-provider cfg)
              b (registry/get-provider cfg)]
          (is (identical? a b))
          (is (not (identical? stub a)))
          (is (= [1.0] (hive-mcp.embeddings.protocol/embed-text b "hello")))
          (is (= ["hello"] @calls))))
      (testing "at :debug the per-call cache-hit line is silent"
        (is (empty? (cache-hit-lines :debug cfg))))
      (testing "at :trace it is still there, at level :trace"
        (let [lines (cache-hit-lines :trace cfg)]
          (is (= 2 (count lines)))
          (is (every? #(= :trace (first %)) lines))))
      (finally
        (if had-venice?
          (registry/register-venice!)
          (registry/unregister-factory! :venice))))))
