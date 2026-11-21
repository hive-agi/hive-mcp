(ns hive-mcp.project.tree-shadow-warn-test
  "A checkout shadowed by a duplicate project-id is WARNed once per process,
   not once per scan (kanban 20261118161704-483c0ccf). Every forced rescan of
   the workspace rediscovered the same ~60 worktrees and re-logged each one."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.project.scope :as scope]
            [hive-mcp.project.tree :as tree]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "project-tree-shadow" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- project! [root rel config]
  (let [d (io/file root rel)]
    (.mkdirs d)
    (spit (io/file d ".hive-project.edn") (pr-str config))
    d))

(defn- worktree! [root rel config]
  (doto (project! root rel config)
    (-> (io/file ".git") (spit "gitdir: /elsewhere"))))

(defn- capturing
  "Run `f` with timbre at :debug and every log line captured.
   -> [result [{:level kw :msg str}]]"
  [f]
  (let [seen (atom [])]
    [(log/with-merged-config
       {:min-level :debug
        :appenders {:println {:enabled? false}
                    :capture {:enabled? true
                              :min-level :debug
                              :fn (fn [{:keys [level vargs]}]
                                    (swap! seen conj {:level level
                                                      :msg   (first vargs)
                                                      :data  (second vargs)}))}}}
       (f))
     @seen]))

(defn- shadow-lines [lines level]
  (filter #(and (= level (:level %))
                (re-find #"(?i)shadowed checkout" (str (:msg %))))
          lines))

(defn- with-isolated-state [f]
  (let [projects (tree/query-all-projects)
        configs  @@#'scope/project-configs
        aliases  @@#'scope/reverse-alias-index
        shadows  @@#'tree/reported-shadows]
    (tree/reset-index!)
    (reset! @#'tree/reported-shadows #{})
    (try (f)
         (finally
           (tree/reset-index!)
           (when (seq projects) (tree/persist-project-entities! projects))
           (reset! @#'scope/project-configs configs)
           (reset! @#'scope/reverse-alias-index aliases)
           (reset! @#'tree/reported-shadows shadows)))))

(deftest shadowed-checkout-warns-once-per-process
  (let [root (temp-dir)]
    (try
      (project! root "." {:project-id "sw-root"})
      (project! root "a" {:project-id "sw-a" :parent-id "sw-root"})
      (project! root "b" {:project-id "sw-b" :parent-id "sw-root"})
      (worktree! root "wt-a" {:project-id "sw-a"})
      (worktree! root "wt-b" {:project-id "sw-b"})
      (with-isolated-state
        (fn []
          (let [path         (.getPath root)
                [s1 first-l] (capturing #(tree/scan-project-tree! path))
                [s2 again-l] (capturing #(tree/scan-project-tree! path))
                [_ third-l]  (capturing #(tree/scan-project-tree! path))]
            (is (:success s1))
            (is (:success s2))
            (testing "the first scan WARNs each shadowed pair once"
              (is (= #{["sw-a" (.getAbsolutePath (io/file root "wt-a"))]
                       ["sw-b" (.getAbsolutePath (io/file root "wt-b"))]}
                     (set (map (juxt (comp :project-id :data) (comp :shadowed-path :data))
                               (shadow-lines first-l :warn))))))
            (testing "later scans of the same tree WARN nothing"
              (is (empty? (shadow-lines again-l :warn)))
              (is (empty? (shadow-lines third-l :warn))))
            (testing "later scans fold the repeats into one debug summary line"
              (is (= [{:count 2}] (mapv :data (shadow-lines again-l :debug)))))
            (testing "a NEW shadowed checkout still WARNs"
              (worktree! root "wt-a2" {:project-id "sw-a"})
              (let [[_ lines] (capturing #(tree/scan-project-tree! path))]
                (is (= [["sw-a" (.getAbsolutePath (io/file root "wt-a2"))]]
                       (mapv (juxt (comp :project-id :data) (comp :shadowed-path :data))
                             (shadow-lines lines :warn)))))))))
      (finally (rm-rf root)))))
