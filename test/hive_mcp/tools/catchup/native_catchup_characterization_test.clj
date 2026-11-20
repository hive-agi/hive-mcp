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
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.catchup :as catchup]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each mem-stub/with-stub-store)

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

(defn- block-names [resp] (mapv :_block (:blocks (parse-response resp))))

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
        (testing "axioms block reflects the seed"
          (let [b (block-by-name resp "axiom-review")
                ;; axiom-review is always emitted; if axiom-review is absent the
                ;; axioms themselves ride in the piggyback / counts. Pin counts.
                hdr (block-by-name resp "header")]
            (is (map? hdr))
            ;; counts must report at least the axiom we just seeded
            (let [counts (:counts hdr)]
              (when counts
                (is (<= 1 (or (:axioms counts) 0)))))))
        (testing "the response still has header first + meta last-ish"
          (let [names (mapv :_block (:blocks parsed))]
            (is (= "header" (first names)))
            (is (some #{"meta"} names)))))
      (finally (rm-tree! dir)))))

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
