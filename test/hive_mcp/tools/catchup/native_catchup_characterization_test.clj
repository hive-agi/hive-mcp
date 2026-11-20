(ns hive-mcp.tools.catchup.native-catchup-characterization-test
  "Characterization tests for hive-mcp.tools.catchup/handle-native-catchup.

   These pin the EXISTING observable behaviour of the target before the QH-2
   behaviour-preserving refactor. They are intentionally structural rather
   than exhaustive: every assertion names a key / block-id / shape that must
   survive the decomposition into small named private fns.

   Branches pinned:
     1. No store configured  -> single error response, :isError true
     2. Store configured, empty, real project dir
        -> vector of blocks, header first, project-id resolved from .hive-project.edn
     3. Store configured + seeded entries -> axioms / decisions blocks reflect seeds
     4. Caller cwd resolution: explicit :directory wins over _caller_cwd
     5. Extension seams :catchup/status-providers, :catchup/bundle-profile,
        :catchup/caps, :catchup/persona-lens, :catchup/lens all default to nil
        -> response shape unchanged when none registered

   No with-redefs. Uses mem-stub/with-stub-store + a real on-disk project dir
   (the seam the production code already consults)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.memory-piggyback :as mp]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.catchup :as catchup]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private extension-keys
  [:cu/a :catchup/lens :catchup/persona-lens])

(defn- restore-extensions
  "Run T with the characterization extension seats removed, then restore the
   exact prior values (including absence) so no test registration leaks."
  [t]
  (let [prior (mapv (fn [k] [k (ext/get-extension k ::absent)]) extension-keys)]
    (try
      (run! ext/deregister! extension-keys)
      (t)
      (finally
        (doseq [[k value] prior]
          (if (= ::absent value)
            (ext/deregister! k)
            (ext/register! k value)))))))

(use-fixtures :each mem-stub/with-stub-store restore-extensions)

;; -----------------------------------------------------------------------------
;; Helpers
;; -----------------------------------------------------------------------------

(defn- tmp-project!
  "Create a real on-disk directory with a .hive-project.edn declaring PID."
  [pid]
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "qh2-char-" pid "-" (random-uuid)))
              (.mkdirs))]
    (spit (io/file dir ".hive-project.edn") (pr-str {:project-id pid}))
    (.getAbsolutePath dir)))

(defn- rm-tree! [dir]
  (run! io/delete-file (reverse (file-seq (io/file dir)))))

(defn- parse-block
  "Parse one MCP text block's JSON into a keyword-keyed map."
  [{:keys [text]}]
  (json/read-str text :key-fn keyword))

(defn- parse-response
  "handle-native-catchup returns either a single MCP error map or a vector
   of MCP text blocks. Normalise to {:blocks [...] :error? bool}."
  [resp]
  (if (map? resp)
    {:error? (boolean (:isError resp))
     :blocks [(parse-block resp)]}
    {:error? false
     :blocks (mapv parse-block resp)}))

(defn- block-by-name [resp name]
  (some #(when (= name (:_block %)) %) (:blocks (parse-response resp))))

;; -----------------------------------------------------------------------------
;; Branch 1 — no store configured
;; -----------------------------------------------------------------------------

(deftest no-store-configured-returns-error
  ;; The mem-stub/with-stub-store fixture installs a stub for the test, so we
  ;; clear the registry slot here via the seam the production code reads
  ;; (mem-proto/store-set?). Doing it this way keeps the test free of
  ;; with-redefs; we re-install the stub afterwards so the fixture's restore
  ;; still sees a coherent registry.
  (let [registry-ns (find-ns 'hive-spi.memory.registry)
        reset!*     (ns-resolve registry-ns 'reset-registry!)
        install!    (ns-resolve 'hive-mcp.test.stub.memory-store 'install!)
        ->stub      (ns-resolve 'hive-mcp.test.stub.memory-store '->stub)]
    (when reset!*
      (reset!*)
      (try
        (let [resp (catchup/handle-native-catchup {:directory "/tmp/whatever"})]
          (testing "single error response, isError true"
            (is (map? resp))
            (is (true? (:isError resp))))
          (testing "body names the missing store"
            (let [parsed (parse-block resp)]
              (is (false? (:success parsed)))
              (is (= "Memory store not configured" (:error parsed))))))
        (finally
          (install! (->stub)))))))

;; -----------------------------------------------------------------------------
;; Branch 2 — store configured, empty project dir
;; -----------------------------------------------------------------------------

(deftest empty-store-produces-well-formed-response
  (let [pid (str "qh2-empty-" (random-uuid))
        dir (tmp-project! pid)]
    (try
      (let [resp (catchup/handle-native-catchup {:directory dir
                                                 :_caller_id "coordinator"})
            parsed (parse-response resp)
            names  (mapv :_block (:blocks parsed))]
        (testing "response is a vector of blocks"
          (is (false? (:error? parsed)))
          (is (sequential? resp))
          (is (pos? (count resp))))
        (testing "header is the first block"
          (is (= "header" (first names))))
        (testing "header carries the resolved project-id"
          (let [hdr (block-by-name resp "header")]
            (is (= pid (:project hdr)))))
        (testing "meta block is present"
          (is (some #{"meta"} names))))
      (finally (rm-tree! dir)))))

;; -----------------------------------------------------------------------------
;; Branch 3 — store seeded with one axiom + one decision
;; -----------------------------------------------------------------------------

(deftest seeded-entries-reach-their-blocks
  (let [pid  (str "qh2-seeded-" (random-uuid))
        dir  (tmp-project! pid)
        store (mem-stub/->stub)
        _     (mem-stub/install! store)
        ids   (mem-stub/seed!
               store
               [{:content "QH2 characterization axiom: behaviour must not change."
                 :type    "axiom"
                 :tags    ["qh2-char" (str "scope:project:" pid)]}
                {:content "QH2 characterization decision: CPPB decomposition."
                 :type    "decision"
                 :tags    ["qh2-char" (str "scope:project:" pid)]}])]
    (try
      (is (= 2 (count ids)))
      (let [resp   (catchup/handle-native-catchup {:directory dir
                                                   :_caller_id "coordinator"})
            parsed (parse-response resp)]
        (testing "no error"
          (is (false? (:error? parsed))))
        (testing "axioms count reflects the seed"
          (let [hdr (block-by-name resp "header")]
            (is (map? hdr))
            (is (<= 1 (get-in hdr [:counts :axioms])))))
        (testing "the response still has header first + meta last-ish"
          (let [names (mapv :_block (:blocks parsed))]
            (is (= "header" (first names)))
            (is (some #{"meta"} names)))))
      (finally (rm-tree! dir)))))

(deftest extensions-and-scope-filtered-piggyback
  (let [pid        (str "qh2-extensions-" (random-uuid))
        other-pid  (str "qh2-other-" (random-uuid))
        dir        (tmp-project! pid)
        caller-id  (str "qh2-characterization-" (random-uuid))
        enrichment (atom [])
        lens-calls (atom [])
        persona    {:favorite-axiom "in-scope"}
        store      (mem-stub/->stub)]
    (mem-stub/install! store)
    (ext/register!
     :cu/a (fn [payload] (swap! enrichment conj payload)))
    (ext/register!
     :catchup/persona-lens
     (fn [candidate project]
       (when (and (= candidate caller-id) (= project pid)) persona)))
    (ext/register!
     :catchup/lens
     (fn [axioms context]
       (swap! lens-calls conj {:axioms axioms :context context})
       axioms))
    (mp/clear-buffer! caller-id)
    (try
      (let [[in-scope-id out-of-scope-id]
            (mem-stub/seed!
             store
             [{:content "In-scope axiom for the current project."
               :type "axiom"
               :tags ["qh2-char" (str "scope:project:" pid)]}
              {:content "Out-of-scope axiom for an unrelated project."
               :type "axiom"
               :tags ["qh2-char" (str "scope:project:" other-pid)]}])]
        (catchup/handle-native-catchup {:directory dir :_caller_id caller-id})
        (testing "enrichment receives exactly the historical payload keys"
          (is (= 1 (count @enrichment)))
          (is (= #{:directory :project-id :caller-id
                   :decisions :decisions-raw :conventions :conventions-raw
                   :sessions :sessions-raw :axioms :principles
                   :priority-principles :priority-conventions}
                 (set (keys (first @enrichment))))))
        (testing "lens receives the resolved persona lens in its context"
          (is (= 1 (count @lens-calls)))
          (is (= persona (get-in (first @lens-calls) [:context :persona-lens]))))
        (testing "only the in-scope axiom is drained"
          (is (= [in-scope-id]
                 (mapv :id (:batch (mp/drain! caller-id)))))
          (is (not= in-scope-id out-of-scope-id))))
      (finally
        (mp/clear-buffer! caller-id)
        (rm-tree! dir)))))

;; -----------------------------------------------------------------------------
;; Branch 4 — :directory wins over _caller_cwd
;; -----------------------------------------------------------------------------

(deftest explicit-directory-beats-caller-cwd
  (let [pid-explicit (str "qh2-explicit-" (random-uuid))
        pid-cwd      (str "qh2-cwd-"      (random-uuid))
        dir-explicit (tmp-project! pid-explicit)
        dir-cwd      (tmp-project! pid-cwd)]
    (try
      (let [resp (catchup/handle-native-catchup {:directory   dir-explicit
                                                 :_caller_cwd dir-cwd
                                                 :_caller_id  "coordinator"})
            hdr  (block-by-name resp "header")]
        (is (some? hdr))
        (is (= pid-explicit (:project hdr))))
      (finally
        (rm-tree! dir-explicit)
        (rm-tree! dir-cwd)))))

;; -----------------------------------------------------------------------------
;; Branch 5 — without any extensions registered the shape is unchanged
;; -----------------------------------------------------------------------------

(deftest no-extensions-default-shape
  (let [pid (str "qh2-noext-" (random-uuid))
        dir (tmp-project! pid)]
    (try
      (let [resp   (catchup/handle-native-catchup {:directory dir
                                                   :_caller_id "coordinator"})
            parsed (parse-response resp)
            names  (mapv :_block (:blocks parsed))]
        (testing "no extension -> still header first, meta present"
          (is (= "header" (first names)))
          (is (some #{"meta"} names)))
        (testing "no isError on the vector form"
          (is (false? (:error? parsed)))))
      (finally (rm-tree! dir)))))
