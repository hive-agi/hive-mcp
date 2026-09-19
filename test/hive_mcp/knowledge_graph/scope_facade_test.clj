(ns hive-mcp.knowledge-graph.scope-facade-test
  "`hive-mcp.knowledge-graph.scope` and `tools.memory.scope/get-current-project-id`
   are old names for kernel code in `hive-mcp.project.scope`. Two properties keep
   an old name honest: it names nothing the kernel does not have, and it calls
   through the kernel VAR, so a reload or a redef of the kernel reaches a
   caller that still spells the old name."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.knowledge-graph.scope :as kg-scope]
            [hive-mcp.project.scope :as project-scope]
            [hive-mcp.tools.memory.scope :as mem-scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- arglists [v] (:arglists (meta v)))

(deftest every-old-name-has-a-kernel-var-of-the-same-shape
  (let [kernel (ns-publics 'hive-mcp.project.scope)
        facade (ns-publics 'hive-mcp.knowledge-graph.scope)]
    (is (seq facade))
    (doseq [[sym v] facade]
      (testing (str sym)
        (is (contains? kernel sym) "the old name points at nothing")
        (is (= (arglists (get kernel sym)) (arglists v)))))))

(deftest a-redef-of-the-kernel-var-reaches-the-old-names
  (with-redefs [project-scope/visible-scopes (constantly [:redefined])
                project-scope/get-current-project-id (constantly :redefined)]
    (is (= [:redefined] (kg-scope/visible-scopes "anything")))
    (is (= :redefined (mem-scope/get-current-project-id "/anywhere")))
    (is (= :redefined (mem-scope/get-current-project-id)))))

(deftest both-names-share-one-registry
  (let [pid (str "facade-test-" (random-uuid))]
    (try
      (kg-scope/register-project-config! pid {:aliases [(str pid "-alias")]})
      (is (= pid (project-scope/resolve-project-id (str pid "-alias"))))
      (finally
        (project-scope/deregister-project-config! pid)))
    (is (= (str pid "-alias") (kg-scope/resolve-project-id (str pid "-alias"))))))
