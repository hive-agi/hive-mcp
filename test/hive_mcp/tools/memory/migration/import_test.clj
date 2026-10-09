(ns hive-mcp.tools.memory.migration.import-test
  "The legacy JSON import reads the old Emacs storage files straight from
   disk (projects/<id>/<type>.json, global/<type>.json). It no longer
   round-trips through Emacs: the elisp `hive-mcp-memory-query` bridge it
   used to call always failed (card 20260727160752-135ba32d)."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.tools.memory.migration.import :as import]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-root []
  (let [d (.toFile (java.nio.file.Files/createTempDirectory
                    "legacy-mem" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (.deleteOnExit d)
    (.getPath d)))

(defn- write-legacy!
  "Write COUNTS ({\"note\" n ...}) entries per type for PROJECT-ID under ROOT."
  [root project-id counts]
  (let [dir (import/legacy-project-dir root project-id)]
    (.mkdirs dir)
    (doseq [[stem n] counts]
      (spit (io/file dir (str stem ".json"))
            (json/write-str (vec (for [i (range n)]
                                   {:id (str project-id "-" stem "-" i)
                                    :type stem
                                    :content (str stem " " i)})))))
    root))

(defn- dry-run-counts
  "Dry-run the import over a fresh legacy tree holding COUNTS for PROJECT-ID;
   answers the decoded :by-type map (or the error text)."
  [{:keys [project-id counts]}]
  (let [root (write-legacy! (temp-root) project-id counts)
        r (atom nil)]
    (ms/with-stub-store
      (fn []
        (reset! r (import/handle-import-json {:project-id project-id :dry-run true
                                              :legacy-dir root}))))
    (let [body (json/read-str (:text @r) :key-fn keyword)]
      (or (:by-type body) (:error body)))))

(def ^:private gen-counts
  (gen/hash-map "note" (gen/choose 0 4) "snippet" (gen/choose 0 4)
                "convention" (gen/choose 0 4) "decision" (gen/choose 0 4)))

(deftrifecta import-reads-legacy-files-from-disk
  dry-run-counts
  {:gen (gen/hash-map :project-id (gen/elements ["hive" "global" "p-1"])
                      :counts gen-counts)
   :pred #(and (map? %) (every? nat-int? (vals %))
               (= #{:notes :snippets :conventions :decisions} (set (keys %))))
   :num-tests 20
   :mutations [["error-string" (fn [_] "Failed to read JSON")]
               ["missing-type" (fn [_] {:notes 0 :snippets 0 :conventions 0})]]
   :assert (fn []
             (is (= {:notes 2 :snippets 0 :conventions 1 :decisions 3}
                    (dry-run-counts {:project-id "hive"
                                     :counts {"note" 2 "snippet" 0
                                              "convention" 1 "decision" 3}})))
             (is (= {:notes 1 :snippets 0 :conventions 0 :decisions 0}
                    (dry-run-counts {:project-id "global" :counts {"note" 1}}))
                 "a missing type file reads as zero entries"))})

(deftest import-writes-entries-into-the-store
  (let [root (write-legacy! (temp-root) "hive" {"note" 2 "decision" 1})]
    (ms/with-stub-store
      (fn []
        (let [r (json/read-str (:text (import/handle-import-json {:project-id "hive" :legacy-dir root}))
                               :key-fn keyword)]
          (is (= 3 (:imported r)))
          (testing "a second import skips every entry"
            (let [r2 (json/read-str (:text (import/handle-import-json {:project-id "hive" :legacy-dir root}))
                                    :key-fn keyword)]
              (is (= 0 (:imported r2)))
              (is (= 3 (get-in r2 [:skipped :total]))))))))))

(deftest import-reports-a-missing-legacy-directory
  (ms/with-stub-store
    (fn []
      (let [r (import/handle-import-json {:project-id "nope" :dry-run true :legacy-dir (temp-root)})]
        (is (str/includes? (:text r) "Failed to read JSON: no legacy memory directory"))))))

(deftest import-reports-unparseable-json
  (let [root (temp-root)
        dir (import/legacy-project-dir root "hive")]
    (.mkdirs dir)
    (spit (io/file dir "note.json") "{not json")
    (ms/with-stub-store
      (fn []
        (let [r (import/handle-import-json {:project-id "hive" :dry-run true :legacy-dir root})]
          (is (str/includes? (:text r) "unreadable legacy JSON")))))))
