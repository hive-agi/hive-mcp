(ns hive-mcp.test.stub.hierarchy
  "A fixture project hierarchy installed through the hierarchy's own seams,
   plus the pure oracle a test checks visibility against.

   PARENTS is {project-id parent-id-or-nil}. `with-hierarchy` installs it as
     - parent links: registered project configs (`project-scope/register-project-config!`
       with :parent-id), which `project-scope/visible-scopes` walks UP;
     - descendants: the project tree cache (`tree/tree-cache` holds
       `tree/build-project-tree` of the fixture, which `tree/get-cached-tree`
       answers), which `project-scope/descendant-scopes`,
       `descendant-scope-tags` and `tree/get-descendant-ids` walk DOWN.
   Registered configs and the prior cached tree are restored afterwards; no
   var root is replaced.

   `fixture` is the shared shape: root \"ws\" with children \"a\" (child \"a1\")
   and \"b\", and \"far\", unrelated to the tree."
  (:require [hive-mcp.project.scope :as project-scope]
            [hive-mcp.project.tree :as tree]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def fixture
  "ws -> {a -> {a1}, b}; far stands alone."
  {"ws" nil "a" "ws" "a1" "a" "b" "ws" "far" nil})

(defn ancestors-of
  "Ancestor ids of PID under PARENTS, nearest first."
  [parents pid]
  (vec (take-while some? (rest (iterate #(get parents %) pid)))))

(defn descendants-of
  "Descendant ids of PID under PARENTS."
  [parents pid]
  (let [children (fn [p] (keep (fn [[c pp]] (when (= pp p) c)) parents))]
    (loop [queue (vec (children pid)) seen #{}]
      (if-let [c (first queue)]
        (if (seen c)
          (recur (subvec queue 1) seen)
          (recur (into (subvec queue 1) (children c)) (conj seen c)))
        seen))))

(defn visible-ids
  "Oracle V(PID): PID, its ancestors, its descendants and \"global\"."
  [parents pid]
  (-> #{pid "global"}
      (into (ancestors-of parents pid))
      (into (descendants-of parents pid))))

(defn board-ids
  "Oracle for the kanban board of PID: PID and its descendants."
  [parents pid]
  (conj (descendants-of parents pid) pid))

(defn scope-tag [pid] (str "scope:project:" pid))

(defn scope-tags-of
  "`scope:project:*` tags of IDS (\"global\" excluded)."
  [ids]
  (set (map scope-tag (disj (set ids) "global"))))

(defn- ->tree [parents]
  (tree/build-project-tree
   (mapv (fn [[id p]] (cond-> {:project/id id} p (assoc :project/parent-id p)))
         parents)))

(defn with-hierarchy
  "Run F with PARENTS installed as the project hierarchy (see ns doc)."
  [parents f]
  (let [registered (into {} (keep (fn [id] (when-let [c (project-scope/get-project-config id)] [id c])))
                         (keys parents))
        cache      @#'tree/tree-cache
        prior-tree @cache]
    (try
      (doseq [[id p] parents]
        (project-scope/register-project-config! id (cond-> {:project-id id} p (assoc :parent-id p))))
      (reset! cache (->tree parents))
      (f)
      (finally
        (reset! cache prior-tree)
        (doseq [id (keys parents)] (project-scope/deregister-project-config! id))
        (doseq [[id c] registered] (project-scope/register-project-config! id c))))))

(defn hierarchy-fixture
  "clojure.test fixture installing `fixture`."
  [f]
  (with-hierarchy fixture f))

(defn consistent?
  "True iff the installed seams answer PARENTS' oracle for every project."
  [parents]
  (every? (fn [pid]
            (and (= (set (project-scope/visible-scopes pid))
                    (conj (set (ancestors-of parents pid)) pid "global"))
                 (= (set (project-scope/descendant-scopes pid))
                    (descendants-of parents pid))
                 (= (set (project-scope/full-hierarchy-scope-tags pid))
                    (conj (scope-tags-of (visible-ids parents pid)) "scope:global"))))
          (keys parents)))
