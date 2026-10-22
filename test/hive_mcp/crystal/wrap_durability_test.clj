(ns hive-mcp.crystal.wrap-durability-test
  "The mechanical session summary is persisted BEFORE LLM synthesis runs: a
   synthesiser that throws still leaves a persisted summary entry, and a
   synthesiser that answers rewrites that same entry (same id) instead of
   adding a second one. Covers the per-scope fan-out path (persist +
   upgrade-wraps!) and the session-end path (synthesis/synthesize)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.crystal.core :as crystal]
            [hive-mcp.crystal.fanout :as fan]
            [hive-mcp.crystal.harvest.by-scope :as bs]
            [hive-mcp.crystal.persist :as persist]
            [hive-mcp.crystal.synthesis :as synthesis]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.vectordb.facade :as facade]
            [hive-mcp.crystal.harvest.collect :as coll]
            [hive-mcp.crystal.wrap-handler :as wrap]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- with-store [store f]
  (let [before (mem-proto/registered-stores)]
    (mem-proto/reset-registry!)
    (try
      (when store (mem-proto/register-store! :default store))
      (f)
      (finally
        (mem-proto/reset-registry!)
        (doseq [[k s] before] (mem-proto/register-store! k s))))))

(defn- with-synth [f body]
  (let [prior (ext/get-extension :cc/summarize-progress)]
    (if f (ext/register! :cc/summarize-progress f) (ext/deregister! :cc/summarize-progress))
    (try (body)
         (finally
           (if prior
             (ext/register! :cc/summarize-progress prior)
             (ext/deregister! :cc/summarize-progress))))))

(defn- map-store
  "An IMemoryStore over an atom {id entry}."
  [db]
  (reify mem-proto/IMemoryStore
    (add-entry! [_ e]
      (let [id (str "mem-" (inc (count @db)))]
        (swap! db assoc id e) id))
    (update-entry! [_ id updates]
      (when (contains? @db id)
        (swap! db update id merge updates)
        (get @db id)))
    (get-entry [_ id] (get @db id))))

(defn- hbs []
  (-> (bs/empty-by-scope {:session "2026-10-22" :directory "/tmp" :agent-id "ag"})
      (bs/assoc-scope "hive"
                      (assoc bs/empty-scope-slice
                             :progress-notes [{:title "fix" :content "did fix"}]
                             :git-commits ["sha1 fix"]))))

(defn- throwing-synth [& _] (throw (ex-info "LLM timed out" {})))

(defn- llm-synth [& _]
  {:type :note :content "## Session Synthesis: llm" :tags ["session-summary" "llm-synthesized"]
   :duration :short})

(deftest fanout--mechanical-persisted-before-synthesis-throws
  (let [db (atom {})]
    (with-store (map-store db)
      (fn []
        (with-synth throwing-synth
          (fn []
            (let [mech (persist/persist-wraps! (fan/mechanical-wraps (hbs)))]
              (is (thrown? Exception (fan/synthesize-wraps (hbs))))
              (is (= 1 (:persisted mech)))
              (is (= 1 (count @db)) "the mechanical summary survives the throw")
              (is (some #{"wrap-mechanical"} (:tags (val (first @db))))))))))))

(deftest fanout--success-upgrades-in-place
  (let [db (atom {})]
    (with-store (map-store db)
      (fn []
        (with-synth llm-synth
          (fn []
            (let [mech (persist/persist-wraps! (fan/mechanical-wraps (hbs)))
                  id   (-> mech :results first :id)
                  up   (persist/upgrade-wraps! mech (fan/synthesize-wraps (hbs)))]
              (is (= 1 (count @db)) "no duplicate entry")
              (is (= id (-> up :results first :id)) "same id, KG edges preserved")
              (is (= "## Session Synthesis: llm" (:content (get @db id))))
              (is (some #{"llm-synthesized"} (:tags (get @db id)))))))))))

(deftest native-wrap--synthesis-throws-wrap-still-persisted
  (let [db (atom {})]
    (with-redefs [hive-mcp.crystal.harvest.collect/harvest-all-by-scope (fn [_] (hbs))]
      (with-store (map-store db)
        (fn []
          (with-synth throwing-synth
            (fn []
              (let [r    ((requiring-resolve 'hive-mcp.crystal.wrap-handler/handle-native-wrap)
                          {:directory "/tmp" :agent_id "ag"})
                    body ((requiring-resolve 'clojure.data.json/read-str) (:text r) :key-fn keyword)]
                (is (not (:isError r)))
                (is (= 1 (:persisted body)))
                (is (.startsWith ^String (:synthesis body) "failed"))
                (is (= 1 (count @db)))))))))))

(defn- harvested []
  {:progress-notes [{:content "Implemented feature X" :tags ["progress"]}]
   :completed-tasks []
   :git-commits ["abc1234 feat: add feature X"]
   :directory nil
   :recalls {}
   :memory-ids-created []
   :memory-ids-accessed []
   :session-timing {:session-start "2026-01-15T10:00:00Z"
                    :session-end "2026-01-15T12:00:00Z"
                    :duration-minutes 120}
   :summary {:progress-count 1 :task-count 0 :commit-count 1 :recall-count 0}})

(defn- run-synthesize [synth]
  (let [db  (atom {})
        ids (atom 0)]
    (with-redefs [facade/index-memory-entry!
                  (fn [e] (let [id (str "s-" (swap! ids inc))] (swap! db assoc id e) id))
                  facade/update-entry!
                  (fn [id u] (swap! db update id merge u) (get @db id))
                  synthesis/run-lifecycle-ops! (fn [& _] {})]
      (with-synth synth
        (fn [] {:result (synthesis/synthesize (harvested)) :db @db})))))

(deftest session-end--synthesis-throws-summary-persisted
  (let [{:keys [result db]} (run-synthesize throwing-synth)]
    (is (string? (:summary-id result)))
    (is (= 1 (count db)))
    (is (some #{"wrap-mechanical"} (:tags (get db (:summary-id result)))))
    (is (false? (get-in result [:synthesis :upgraded?])))))

(deftest session-end--synthesis-success-upgrades-same-entry
  (let [{:keys [result db]} (run-synthesize llm-synth)
        e (get db (:summary-id result))]
    (is (= 1 (count db)) "one entry, upgraded in place")
    (is (true? (get-in result [:synthesis :upgraded?])))
    (is (.startsWith ^String (:content e) "## Session Synthesis: llm"))
    (is (not (some #{"wrap-mechanical"} (:tags e))))))

(deftest no-synthesiser--not-delegated
  (testing "without a registered synthesiser the summary is stored once, mechanically"
    (with-redefs [ext/get-extension (fn [_] nil)]
      (is (false? (crystal/synthesis-delegated?))))))
