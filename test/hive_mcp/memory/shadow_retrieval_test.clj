(ns hive-mcp.memory.shadow-retrieval-test
  "RETRIEVAL-03: the shadow retrieval seam is off by default, never changes the
   primary search response, and swallows+counts shadow throws and timeouts."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.memory.shadow-retrieval :as shadow]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.memory.search :as search]
            [hive-schemas.test :as hst]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord SearchPort [hits]
  mem-proto/IMemoryStore
  (connect! [_ _] nil)
  (disconnect! [_] nil)
  (connected? [_] true)
  (health-check [_] {:healthy? true})
  (add-entry! [_ _] nil)
  (get-entry [_ _] nil)
  (update-entry! [_ _ _] nil)
  (delete-entry! [_ _] nil)
  (query-entries [_ _] nil)
  (search-similar [_ _ _] hits)
  (supports-semantic-search? [_] true)
  (cleanup-expired! [_] nil)
  (entries-expiring-soon [_ _ _] nil)
  (find-duplicate [_ _ _ _] nil)
  (store-status [_] {:configured? true})
  (reset-store! [_] nil))

(defn- hit [id d]
  {:id id :content (str "needle " id) :type "note" :tags ["t"] :distance d})

(def ^:private hits [(hit "b" 0.2) (hit "a" 0.1) (hit "c" 0.3)])

(defn- with-clean-seam [f]
  (let [prior (shadow/resolve-shadow-retriever)]
    (ext/deregister! shadow/extension-key)
    (shadow/reset-shadow-stats!)
    (try (f)
         (finally
           (ext/deregister! shadow/extension-key)
           (when prior (ext/register! shadow/extension-key prior))
           (shadow/reset-shadow-stats!)))))

(use-fixtures :each with-clean-seam)

(defn- search-text
  "The raw MCP response text of one search over `hits`."
  []
  (let [prior (get (mem-proto/registered-stores) :default)]
    (try
      (mem-proto/register-store! :default (->SearchPort hits))
      (:text (search/handle-search-semantic
              {:query "needle" :limit 10 :scope "all"
               :exclude_tags [] :directory "/tmp"}))
      (finally
        (if prior
          (mem-proto/register-store! :default prior)
          (mem-proto/unregister-store! :default))))))

(defn- await-count
  "Poll until the shadow counter K reaches N, up to 3 s. Answers the count."
  [k n]
  (loop [i 0]
    (let [c (get-in (shadow/shadow-stats) [:counts k] 0)]
      (if (or (>= c n) (> i 150))
        c
        (do (Thread/sleep 20) (recur (inc i)))))))

(deftest off-by-default-does-not-call-shadow
  (is (nil? (shadow/resolve-shadow-retriever)))
  (is (= :off (shadow/dispatch-shadow! {:query "q" :limit 1})))
  (let [text (search-text)]
    (is (string? text))
    (is (= {:dispatched 0 :ok 0 :error 0 :timeout 0 :rejected 0}
           (:counts (shadow/shadow-stats)))
        "an unregistered seam builds, submits and counts nothing")
    (is (empty? (:recent (shadow/shadow-stats))))))

(deftest shadow-throws-primary-unchanged
  (let [baseline (search-text)
        called   (atom 0)]
    (ext/register! shadow/extension-key
                   (fn [_] (swap! called inc) (throw (ex-info "shadow boom" {}))))
    (let [with-shadow (search-text)]
      (is (= baseline with-shadow) "primary response is byte-for-byte unchanged")
      (is (= 1 (await-count :error 1)) "the throw is swallowed and counted")
      (is (= 1 @called))
      (is (= 1 (get-in (shadow/shadow-stats) [:counts :dispatched]))))))

(deftest shadow-slow-is-bounded
  (let [baseline (search-text)
        release  (promise)]
    (ext/register! shadow/extension-key
                   (fn [_] (deref release 5000 nil) [{:id "late"}]))
    (binding [shadow/*shadow-timeout-ms* 100]
      (let [t0   (System/nanoTime)
            text (search-text)
            ms   (/ (- (System/nanoTime) t0) 1e6)]
        (is (= baseline text) "a slow shadow does not change the response")
        (is (< ms 2000) "the search does not wait for the shadow")
        (is (= 1 (await-count :timeout 1)) "the shadow is cut at its budget and counted")
        (is (zero? (get-in (shadow/shadow-stats) [:counts :ok])))))
    (deliver release :done)))

(deftest shadow-ok-records-comparison
  (let [seen (promise)]
    (ext/register! shadow/extension-key
                   (fn [req] (deliver seen req) [{:id "a"} {:id "z"}]))
    (let [baseline-ids ["a" "b" "c"]]
      (search-text)
      (is (= 1 (await-count :ok 1)))
      (let [req (deref seen 1000 nil)
            rec (last (:recent (shadow/shadow-stats)))]
        (testing "the shadow sees raw pre-merge store candidates"
          (is (= ["b" "a" "c"] (mapv :id (:store-candidates req))))
          (is (= baseline-ids (:primary-ids req))))
        (is (= ["a" "z"] (:shadow-ids rec)))
        (is (= 1 (:overlap rec)))))))

(hst/deftrifecta-from-schema summarize-shadow-answer-counts-overlap
  hive-mcp.memory.shadow-retrieval/summarize-shadow-answer
  {:in [:cat [:vector {:max 4} [:map [:id [:enum "a" "b" "c"]]]]
        [:vector {:max 3} [:enum "a" "b"]]]
   :out shadow/ShadowSummary
   :rel (fn [[answer primary] out]
          (and (= (mapv :id answer) (:shadow-ids out))
               (= (count (filter (set primary) (distinct (map :id answer))))
                  (:overlap out))))
   :num-tests 100})

(hst/deftrifecta-from-schema shadow-request-is-plain-values
  hive-mcp.memory.shadow-retrieval/shadow-request
  {:in [:cat [:map
              [:query [:maybe [:enum "q" "x"]]]
              [:limit [:int {:min 0 :max 5}]]
              [:store-candidates [:vector {:max 2} [:map [:id [:enum "a" "b"]]]]]
              [:primary-ids [:vector {:max 2} [:enum "a" "b"]]]]]
   :out shadow/ShadowRequest
   :rel (fn [[ctx] out]
          (and (= (:query ctx) (:query out))
               (= (:limit ctx) (:limit out))
               (= (vec (:store-candidates ctx)) (:store-candidates out))
               (= [] (:ingest-candidates out))))
   :num-tests 100})
