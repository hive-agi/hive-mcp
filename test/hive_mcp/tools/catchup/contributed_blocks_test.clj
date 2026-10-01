(ns hive-mcp.tools.catchup.contributed-blocks-test
  "Every block registered in hive-mcp.spi.catchup-registry reaches the catchup
   answer as its own {\"_block\": \"<id>\", ...} section, in :block/order, with
   no per-id rendering in the host. Blocks are registered in the REAL registry
   and catchup runs end to end against the stub memory store."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.spi.catchup-registry :as blocks]
            [hive-mcp.test.stub.memory-store :as mem-stub]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.tools.catchup.format :as fmt]
            [hive-mcp.tools.kanban.catchup-block :as kanban-block]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private stub-ids
  [:zz-contributed-early :zz-contributed-late :zz-contributed-silent :zz-contributed-broken])

(defn- isolate-registry
  "Snapshot the block registry, run T with only the stub blocks installed,
   then restore every block that was there before."
  [t]
  (let [prior (blocks/registered-blocks)]
    (try
      (blocks/reset-registry!)
      (t)
      (finally
        (blocks/reset-registry!)
        (run! blocks/register-block! prior)))))

(use-fixtures :each mem-stub/with-stub-store isolate-registry)

(defn- register-stubs! []
  (blocks/register-block! {:block/id    :zz-contributed-late
                           :block/order 90
                           :block/fn    (fn [_] {:marker "late"})})
  (blocks/register-block! {:block/id    :zz-contributed-early
                           :block/order 10
                           :block/fn    (fn [{:keys [project-id]}]
                                          {:marker "early" :seen-project project-id})})
  (blocks/register-block! {:block/id    :zz-contributed-silent
                           :block/order 50
                           :block/fn    (fn [_] nil)})
  (blocks/register-block! {:block/id    :zz-contributed-broken
                           :block/order 60
                           :block/fn    (fn [_] (throw (ex-info "boom" {})))}))

(defn- project-dir! [project-id]
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "contributed-blocks-test-" project-id))
              (.mkdirs))]
    (spit (io/file dir ".hive-project.edn") (pr-str {:project-id project-id}))
    (.getAbsolutePath dir)))

(defn- run-catchup []
  (let [project-id (str "cbt-" (random-uuid))
        dir        (project-dir! project-id)]
    (try
      {:project-id project-id
       :sections   (mapv #(json/read-str (:text %) :key-fn keyword)
                         (catchup/handle-native-catchup {:directory  dir
                                                         :_caller_id "coordinator"}))}
      (finally
        (run! io/delete-file (reverse (file-seq (io/file dir))))))))

(deftest every-contributed-block-is-its-own-section
  (register-stubs!)
  (let [{:keys [project-id sections]} (run-catchup)
        names   (mapv :_block sections)
        by-name (into {} (map (juxt :_block identity)) sections)]
    (testing "an arbitrary contributed id lands as a section named after it"
      (is (contains? by-name "zz-contributed-early"))
      (is (contains? by-name "zz-contributed-late"))
      (is (= "early" (get-in by-name ["zz-contributed-early" :marker])))
      (is (= project-id (get-in by-name ["zz-contributed-early" :seen-project]))
          "the block fn receives the catchup context"))
    (testing "sections follow :block/order"
      (is (< (.indexOf names "zz-contributed-early")
             (.indexOf names "zz-contributed-late"))))
    (testing "a nil value is omitted and a throwing block does not sink the rest"
      (is (not (contains? by-name "zz-contributed-silent")))
      (is (not (contains? by-name "zz-contributed-broken"))))
    (testing "the host's own sections are still there"
      (is (= "header" (first names)))
      (is (contains? by-name "meta")))))

(deftest kanban-section-renders-as-before
  (testing "a non-empty board renders the same kanban section the host used to hand-build"
    (let [summary {:counts {:todo 2 :inprogress 0 :inreview 0 :done 1}
                   :recent-todos [{:id "t1" :title "One" :tags ["kanban"]}]
                   :scope-tag "scope:project:p"}
          [section] (fmt/contributed-sections [[:kanban (kanban-block/catchup-section summary)]])
          parsed (json/read-str (:text section) :key-fn keyword)]
      (is (= #{:_block :counts :recent-todos :scope-tag :hint} (set (keys parsed))))
      (is (= "kanban" (:_block parsed)))
      (is (= 2 (get-in parsed [:counts :todo])))
      (is (re-find #"kanban list" (:hint parsed)))))
  (testing "an empty board emits no section"
    (is (nil? (kanban-block/catchup-section {:counts {} :recent-todos []})))
    (is (= [] (fmt/contributed-sections
               [[:kanban (kanban-block/catchup-section {:counts {} :recent-todos []})]])))))
