(ns hive-mcp.tools.consolidated.memory-bare-batch-test
  "Bare `memory batch` is not a generic batcher: an operations array, or a
   call naming no files, is refused synchronously with a pointer to
   batch-add / batch-edit / batch-get, and only a file batch reaches the
   ingest addon as `ingest batch` (friction 20261017021220-211e647f)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.memory :as memory]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording-delegate
  "A delegate port stub that records every call it receives."
  [calls]
  (fn [params]
    (swap! calls conj params)
    {:type "text" :text "{\"queued\":true}"}))

(deftest command-operations-are-refused-without-reaching-ingest
  (let [calls   (atom [])
        handler (memory/make-bare-batch-handler (recording-delegate calls))
        resp    (handler {:command    "batch"
                          :operations [{:command "kg edge" :from "a" :to "b" :relation "relates"}]})]
    (is (true? (:isError resp)))
    (doseq [verb ["batch-add" "batch-edit" "batch-get"]]
      (is (str/includes? (:text resp) verb) verb))
    (is (str/includes? (:text resp) "belongs to the ingest addon"))
    (is (empty? @calls) "no ingest job was asked for")))

(deftest operations-as-json-text-are-refused-too
  (let [calls   (atom [])
        handler (memory/make-bare-batch-handler (recording-delegate calls))]
    (is (true? (:isError (handler {:command "batch" :operations "[{\"command\":\"add\"}]"}))))
    (is (true? (:isError (handler {:command "batch" :operations [{:path "/a.md"}] :paths ["/a.md"]})))
        "operations are refused even beside paths: ingest batch takes none")
    (is (empty? @calls))))

(deftest a-call-naming-no-files-is-refused
  (let [calls   (atom [])
        handler (memory/make-bare-batch-handler (recording-delegate calls))
        resp    (handler {:command "batch"})]
    (is (true? (:isError resp)))
    (is (str/includes? (:text resp) "batch-add"))
    (is (empty? @calls))))

(deftest a-file-batch-is-delegated-as-ingest-batch
  (let [calls   (atom [])
        handler (memory/make-bare-batch-handler (recording-delegate calls))]
    (testing "paths"
      (handler {:command "batch" :paths ["/b/a.md"]})
      (is (= "ingest batch" (:command (last @calls))))
      (is (= ["/b/a.md"] (:paths (last @calls)))))
    (testing "dir"
      (handler {:command "batch" :dir "/b" :glob "*.md"})
      (is (= "ingest batch" (:command (last @calls))))
      (is (= "/b" (:dir (last @calls)))))))

(deftest the-memory-tree-routes-bare-batch-through-the-refusal
  (let [resp ((:batch memory/canonical-handlers)
              {:command "batch" :operations [{:command "kg edge"}]})]
    (is (true? (:isError resp)))
    (is (str/includes? (:text resp) "batch-edit"))))

(deftest the-advertised-description-names-the-batch-verbs
  (let [desc (:description memory/tool-def)]
    (is (str/includes? desc "no generic 'batch'"))
    (is (str/includes? desc "ingest addon"))))
