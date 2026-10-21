(ns hive-mcp.project.tree-facets-test
  "Golden over project-tree discovery, persistence and the cached-tree
   queries, driven by a real scan of a temp directory tree: a root, a child
   and a grandchild, a duplicate id in a linked worktree (shadowed), an
   unrendered template id (skipped) and an ignored node_modules. The index,
   the tree cache and the scope config cache are process state: each is
   snapshot and restored around the scenario. Paths and timestamps are
   host-specific, so the golden projects them away."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-mcp.project.scope :as scope]
            [hive-mcp.project.tree :as tree]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "project-tree" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- project! [root rel config]
  (let [d (io/file root rel)]
    (.mkdirs d)
    (spit (io/file d ".hive-project.edn") (pr-str config))
    d))

(defn- with-isolated-state [f]
  (let [projects (tree/query-all-projects)
        configs  @@#'scope/project-configs
        aliases  @@#'scope/reverse-alias-index]
    (tree/reset-index!)
    (try (f)
         (finally
           (tree/reset-index!)
           (when (seq projects) (tree/persist-project-entities! projects))
           (reset! @#'scope/project-configs configs)
           (reset! @#'scope/reverse-alias-index aliases)))))

(defn- entity-view [e]
  (when e
    (-> (select-keys e [:project/id :project/type :project/parent-id :project/tags])
        (assoc :has-path? (string? (:project/path e))))))

(defn- scenario
  "Steps run in an explicit let, because a large map literal evaluates in
   hash order and the last two steps (a rescan, a no-op persist) mutate the
   cache the queries read."
  []
  (let [root (temp-dir)]
    (try
      (project! root "." {:project-id "f08-root"})
      (project! root "mid" {:project-id "f08-mid" :parent-id "f08-root" :project-type "library"
                            :tags ["clj"]})
      (project! root "mid/leaf" {:project-id "f08-leaf" :parent "f08-mid"})
      (let [wt (project! root "zz-worktree" {:project-id "f08-mid"})]
        (spit (io/file wt ".git") "gitdir: /elsewhere"))
      (project! root "tmpl" {:project-id "{{artifact/id}}"})
      (project! root "node_modules/pkg" {:project-id "f08-ignored"})
      (with-isolated-state
        (fn []
          (let [path         (.getPath root)
                before       {:stale? (tree/tree-stale? path) :cached (tree/get-cached-tree)}
                scan         (tree/scan-project-tree! path)
                after        {:stale-root? (boolean (tree/tree-stale? path))
                              :stale-elsewhere? (tree/tree-stale? "/no-such-root-f08")}
                _            (tree/invalidate-tree-cache!)
                all          (mapv entity-view (tree/query-all-projects))
                by-id        (entity-view (tree/query-project-by-id "f08-leaf"))
                missing      (tree/query-project-by-id "f08-nope")
                children     (mapv :project/id (tree/query-project-children "f08-root"))
                has-children {:root (tree/has-children? "f08-root") :leaf (tree/has-children? "f08-leaf")}
                descendants  (vec (sort (tree/get-descendant-ids "f08-root")))
                desc-tags    (vec (sort (tree/get-descendant-scope-tags "f08-mid")))
                desc-global  (tree/get-descendant-scope-tags "global")
                desc-scopes  (tree/get-descendant-scopes "f08-leaf")
                scope-parent (scope/get-parent-scope "f08-leaf")
                maybe-scan   (tree/maybe-scan-project-tree! path)
                persist-none (tree/persist-project-entities! [])
                missing-root (tree/scan-project-tree! "/no-such-root-f08")]
            {:1-before       before
             :2-scan         (-> scan (select-keys [:success :projects-found :roots])
                                 (assoc :tree-children (:children (:tree scan))))
             :3-after        after
             :4-all          all
             :5-by-id        by-id
             :6-missing      missing
             :7-children     children
             :8-has-children has-children
             :9-descendants  descendants
             :10-desc-tags   desc-tags
             :11-desc-global desc-global
             :12-desc-scopes desc-scopes
             :13-scope-parent scope-parent
             :14-maybe-scan  maybe-scan
             :15-persist-none persist-none
             ;; an empty scan persists nothing, and persist answers nil for
             ;; no entities, so the scan reports failure with a nil :error
             :16-missing-root missing-root})))
      (finally (rm-rf root)))))

(deftest-golden-fn project-tree-scan-golden
  "test/golden/project/tree-scan.edn"
  scenario)

(deftest reset-index-empties-everything
  (with-isolated-state
    (fn []
      (tree/persist-project-entities! [{:project/id "x"}])
      (is (= 1 (count (tree/query-all-projects))))
      (is (nil? (tree/reset-index!)))
      (is (= [] (tree/query-all-projects)))
      (is (nil? (tree/get-cached-tree))))))
