(ns hive-mcp.tools.consolidated.session-trifecta-test
  "Trifectas over the session tool's context-store handlers. The store is the
   in-process context store; every value is put and read back by the test, so
   the golden snapshot pins the projection (round-trip verdict), never a
   generated ctx-id or timestamp."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.session :as ss]
            [hive-mcp.tools.core :as tc]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(defn round-trip
  "Put DATA, read it back, evict it, read again. Answers what each step saw."
  [data]
  (let [put (ss/handle-context-put {:data data :tags ["m27-trifecta"]})]
    (if (:isError put)
      {:put :refused :text (:text put)}
      (let [id      (:ctx-id (body put))
            got     (body (ss/handle-context-get {:ctx_id id}))
            evicted (body (ss/handle-context-evict {:ctx_id id}))
            again   (body (ss/handle-context-get {:ctx_id id}))]
        {:put        :stored
         :read-back? (= (json/read-str (json/write-str data) :key-fn keyword) (:data got))
         :evicted    (:evicted evicted)
         :gone?      (true? (:not-found again))}))))

(deftrifecta context-store-round-trip
  hive-mcp.tools.consolidated.session-trifecta-test/round-trip
  {:golden-path "test/golden/tools/consolidated/session-context-round-trip.edn"
   :cases       {:map     {:a 1 :b "two"}
                 :vector  [1 2 3]
                 :string  "plain"
                 :nil     nil
                 :nested  {:k {:v [true false]}}}
   :gen         (gen/one-of [(gen/map gen/keyword gen/small-integer {:max-elements 3})
                             (gen/vector gen/string-alphanumeric 0 3)
                             gen/string-alphanumeric])
   :pred        #(= {:put :stored :read-back? true :evicted true :gone? true} %)
   :num-tests   50
   :mutations   [["never-evicts" (fn [d] (assoc (round-trip d) :evicted false :gone? false))]
                 ["loses-data" (fn [d] (assoc (round-trip d) :read-back? false))]]})

(deftrifecta context-get-refusal
  hive-mcp.tools.consolidated.session/handle-context-get
  {:golden-path "test/golden/tools/consolidated/session-context-get.edn"
   :cases       {:no-id {} :nil nil :unknown {:ctx_id "m27-no-such-ctx"}}
   :gen         (gen/fmap (fn [s] {:ctx_id (str "m27-absent-" s)}) gen/string-alphanumeric)
   :pred        #(and (m/validate tc/McpResponse %) (true? (:not-found (body %))))
   :num-tests   30
   :mutations   [["found-everything" (fn [p] {:type "text" :text (json/write-str {:ctx-id (:ctx_id p)})})]
                 ["throws-shaped" (fn [_] (tc/mcp-error "boom"))]]})

(deftest every-context-handler-answers-an-mcp-response
  (doseq [[h p] [[ss/handle-context-put {}]
                 [ss/handle-context-query {:tags ["m27-none"]}]
                 [ss/handle-context-evict {}]
                 [ss/handle-context-stats {}]]]
    (is (m/validate tc/McpResponse (h p)) (str h))))
