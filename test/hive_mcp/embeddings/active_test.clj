(ns hive-mcp.embeddings.active-test
  "The global embedding-provider slot and its routing helpers, with the
   provider port satisfied by the deterministic MockEmbedder and the config
   port bound to an empty EDN config (no collection routes, so every
   collection falls back to the global provider)."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.config.test-support :as cfg]
            [hive-mcp.embeddings.active :as active]
            [hive-mcp.test-fixtures :as fixtures]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-slot [f]
  (let [prior (active/get-embedding-provider)]
    (try (cfg/with-config {} (f))
         (finally
           (if prior (active/set-embedding-provider! prior) (active/reset-embedding-provider!))))))

(use-fixtures :each own-the-slot)

(defn- thrown [thunk]
  (try (thunk) :no-throw
       (catch clojure.lang.ExceptionInfo e [(ex-message e) (ex-data e)])))

(deftest-golden provider-slot-golden
  "test/golden/hive-mcp/embeddings/active-slot.edn"
  (let [_      (active/reset-embedding-provider!)
        empty  {:configured? (active/embedding-configured?)
                :provider    (active/get-embedding-provider)
                :require     (thrown active/require-embedding!)}
        _      (active/set-embedding-provider! (fixtures/->MockEmbedder 4))
        p      (active/get-embedding-provider)
        full   {:configured? (active/embedding-configured?)
                :require     (active/require-embedding!)
                :dimension   (active/embedding-dimension p)
                :one         (active/embed-text p "hello")
                :batch       (active/embed-batch p ["a" "bb"])
                ;; a one-off provider crosses the same gate
                :one-off     (active/embed-text (fixtures/->MockEmbedder 3) "hello")}
        routed {:text      (active/embed-text-for "f04-unrouted" "hello")
                :batch     (active/embed-batch-for "f04-unrouted" ["a"])
                :dimension (active/get-dimension-for "f04-unrouted")}
        reset  (active/reset-embedding-provider!)]
    {:empty empty :full full :routed routed :reset reset
     :after (active/embedding-configured?)}))

(deftest set-nil-clears-the-slot
  (active/set-embedding-provider! (fixtures/->MockEmbedder 4))
  (active/set-embedding-provider! nil)
  (is (false? (active/embedding-configured?))))
