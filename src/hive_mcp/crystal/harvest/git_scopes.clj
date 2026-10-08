(ns hive-mcp.crystal.harvest.git-scopes
  "HCR-aware commit harvest: an umbrella wrap also reads the git history of
   descendant projects that live in their OWN repositories.

   Why: `git log` in an umbrella directory sees nothing of a child repo that
   the umbrella gitignores, so a wrap of the umbrella reported 0 commits while
   the children shipped work, and synthesis baked that into false
   ghost-completion claims. Kanban 20260516114613-10aeb0be.

   Pure core (`descendant-repos`, `merge-commit-sets`) plus one effectful
   runner (`harvest-descendant-commits`) that takes its git reader as an
   argument, so the core is testable without a filesystem or a project tree.")
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-timeout-ms
  "Wall-clock bound for one descendant repository's `git log`."
  5000)

(defn descendant-repos
  "Descendant projects that are separate git repositories.

   ROOT-GIT-ROOT is the git root of the wrapped directory (nil when it is not
   inside a repository). ENTITIES are project-tree entities with :project/id,
   :project/path and :project/git-root. An entity is kept when its git root is
   known and differs from ROOT-GIT-ROOT; a descendant inside the umbrella's own
   repository is already covered by the umbrella's `git log`. Several projects
   sharing one repository yield that repository once, under the first project
   id in order. Returns [{:pid :dir}] with :dir the repository root."
  [root-git-root entities]
  (->> entities
       (keep (fn [{:project/keys [id git-root]}]
               (when (and id git-root (not= git-root root-git-root))
                 {:pid id :dir git-root})))
       (reduce (fn [{:keys [seen out] :as acc} {:keys [dir] :as repo}]
                 (if (contains? seen dir)
                   acc
                   {:seen (conj seen dir) :out (conj out repo)}))
               {:seen #{} :out []})
       :out))

(defn merge-commit-sets
  "Fold descendant commit sets into the umbrella's commit result.

   ROOT is the umbrella result {:commits [str] :count n ...}. SCOPED is a seq
   of {:pid :commits [str]} (an :error entry contributes no commits but is
   reported). Descendant commits are appended prefixed with \"[pid] \" so the
   source scope survives into synthesis prose. Adds :by-scope {pid count} for
   every descendant that contributed at least one commit, and :scope-errors
   when any descendant failed. With no SCOPED input ROOT is returned as is."
  [root scoped]
  (if (empty? scoped)
    root
    (let [root-commits (vec (:commits root))
          tagged (into [] (mapcat (fn [{:keys [pid commits]}]
                                    (map #(str "[" pid "] " %) commits)))
                       scoped)
          by-scope (into (sorted-map)
                         (keep (fn [{:keys [pid commits]}]
                                 (when (seq commits) [pid (count commits)])))
                         scoped)
          errors (filterv :error scoped)
          all (into root-commits tagged)]
      (cond-> (assoc root :commits all :count (count all))
        (seq by-scope) (assoc :by-scope by-scope)
        (seq errors) (assoc :scope-errors (mapv #(select-keys % [:pid :error]) errors))))))

(defn harvest-descendant-commits
  "Run GIT-LOG for each repository in REPOS in parallel, each bounded by
   TIMEOUT-MS. GIT-LOG is (fn [dir] {:commits [str]} | {:error any}).
   Returns [{:pid :commits} | {:pid :error}] in REPOS order. Never throws."
  ([git-log repos] (harvest-descendant-commits git-log repos default-timeout-ms))
  ([git-log repos timeout-ms]
   (let [futs (mapv (fn [{:keys [pid dir]}]
                      [pid (future (try (git-log dir)
                                        (catch Throwable t
                                          {:error (str (.getName (class t)) ": " (.getMessage t))})))])
                    repos)]
     (mapv (fn [[pid fut]]
             (let [r (deref fut timeout-ms ::timeout)]
               (cond
                 (= ::timeout r) (do (future-cancel fut) {:pid pid :error :timeout})
                 (:error r)      {:pid pid :error (:error r)}
                 :else           {:pid pid :commits (vec (:commits r))})))
           futs))))
