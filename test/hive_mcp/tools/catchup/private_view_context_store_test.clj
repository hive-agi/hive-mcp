(ns hive-mcp.tools.catchup.private-view-context-store-test
  "Catchup under a private view writes nothing to the process-global context
   store: no context-refs and no shed blocks. The shared view, run the same
   way, still writes both, so the private assertion cannot pass vacuously.

   The :catchup/private-view? provider is registered through the real
   extension registry and restored afterwards. Store: mem-stub."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.context-store :as context-store]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.tools.catchup.bundle-cache :as bc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private extension-keys
  [:catchup/private-view? :cu/a :catchup/lens :catchup/persona-lens])

(defn- restore-extensions [t]
  (let [prior (mapv (fn [k] [k (ext/get-extension k ::absent)]) extension-keys)]
    (try
      (run! ext/deregister! extension-keys)
      (bc/reset-cache!)
      (t)
      (finally
        (doseq [[k value] prior]
          (if (= ::absent value)
            (ext/deregister! k)
            (ext/register! k value)))
        (bc/reset-cache!)))))

(use-fixtures :each mem-stub/with-stub-store restore-extensions)

(defn- tmp-project! [pid]
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "catchup-pv-" pid))
              (.mkdirs))]
    (spit (io/file dir ".hive-project.edn") (pr-str {:project-id pid}))
    (.getAbsolutePath dir)))

(defn- rm-tree! [dir]
  (run! io/delete-file (reverse (file-seq (io/file dir)))))

(defn- seed-project!
  "Axioms (written as context-refs) and session wraps (answered in the
   inline context block, the first block shed) scoped to PID."
  [pid]
  (let [store (mem-stub/->stub)
        scope (str "scope:project:" pid)]
    (mem-stub/install! store)
    (mem-stub/seed!
     store
     (vec (concat
           (for [i (range 6)]
             {:content (str "Enclave-only axiom " i ": " (apply str (repeat 200 "a")))
              :type    "axiom"
              :tags    ["catchup-pv" scope]})
           (for [i (range 12)]
             {:content (str "Enclave-only session " i ": " (apply str (repeat 300 "s")))
              :type    "note"
              :tags    ["session-wrap" "session-summary" scope]}))))))

(defn- run-tags
  "The tags catchup's context-store writes for PID carry: the raw project-id
   on context-refs, scope:project:<pid> on shed blocks."
  [pid]
  #{pid (str "scope:project:" pid)})

(defn- written-for
  "Every live context-store entry tagged catchup that belongs to PID."
  [pid]
  (filterv #(some (run-tags pid) (:tags %))
           (context-store/context-query :tags ["catchup"] :limit 10000)))

(defn- evict-run! [pid]
  (context-store/evict-by-tags! (vec (run-tags pid))))

(defn- blocks [resp] (mapv #(json/read-str (:text %) :key-fn keyword) resp))

(defn- shed-stubs [resp] (filterv :shed (blocks resp)))

(defn- run-catchup [pid private?]
  (let [dir (tmp-project! pid)]
    (seed-project! pid)
    (ext/register! :catchup/private-view? (fn [_ _] private?))
    (try
      (catchup/handle-native-catchup {:directory     dir
                                      :_caller_id    (str "pv-test-" pid)
                                      :inline_budget 1500})
      (finally
        (ext/deregister! :catchup/private-view?)
        (rm-tree! dir)))))

(deftest a-shared-view-writes-refs-and-sheds
  (let [pid  (str "catchup-pv-shared-" (random-uuid))
        resp (run-catchup pid false)
        hdr  (first (blocks resp))
        refs (get-in hdr [:memory-piggyback :context-refs])]
    (try
      (is (sequential? resp))
      (testing "context-refs are written and named in the header"
        (is (seq refs))
        (is (every? #(some? (context-store/context-get %)) (vals refs)))
        (is (some #(contains? (:tags %) pid) (written-for pid))))
      (testing "the small budget sheds blocks into the context store"
        (is (seq (shed-stubs resp)))
        (is (some #(contains? (:tags %) "catchup-shed") (written-for pid))))
      (finally
        (evict-run! pid)))))

(deftest a-private-view-writes-nothing-to-the-context-store
  (let [pid  (str "catchup-pv-private-" (random-uuid))
        resp (run-catchup pid true)
        hdr  (first (blocks resp))]
    (try
      (is (sequential? resp))
      (testing "no context-store entry carries this project's scope"
        (is (= [] (written-for pid))))
      (testing "the answer names no context-refs"
        (is (empty? (get-in hdr [:memory-piggyback :context-refs]))))
      (testing "nothing is shed: every block stays inline"
        (is (= [] (shed-stubs resp)))
        (is (not-any? #(= "inline-budget" (:_block %)) (blocks resp)))
        (is (some #(re-find #"Enclave-only session" (:text %)) resp)
            "the private content is answered inline"))
      (finally
        (evict-run! pid)))))
