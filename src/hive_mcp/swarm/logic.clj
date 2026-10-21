(ns hive-mcp.swarm.logic
  "Logic programming engine for swarm hivemind coordination.

   Compat shim: moved to hive-agent.swarm.logic in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.logic" sym))

;; Value schemas for the shim contracts, transcribed from the docstrings of
;; hive-agent.swarm.logic. Ids are whatever the callers pass (strings in
;; practice); core.logic unifies any value.

(def Id
  "A slave, task or file id."
  :string)

(def FileConflict
  "`check-file-conflicts`: a file another slave holds."
  [:map [:file Id] [:held-by Id]])

(def Claim
  "`get-all-claims` / `get-claim-for-file`."
  [:map [:file Id] [:slave-id Id]])

(def LogicDb
  "The logic-db atom, exposed for external locking only."
  [:fn {:gen/fmap (fn [_] (atom nil)) :gen/schema :nil}
   #(instance? clojure.lang.IAtom %)])

(defn add-claim!
  {:arglists '([file-path slave-id])}
  [& args]
  (apply (impl 'add-claim!) args))

;; mutators answer nil: the value is the swap, logged
(m/=> add-claim! [:=> [:cat Id Id] :nil])

(defn add-slave!
  {:arglists '([slave-id status])}
  [& args]
  (apply (impl 'add-slave!) args))

(m/=> add-slave! [:=> [:cat Id :keyword] :nil])

(defn add-task-file!
  {:arglists '([task-id file-path])}
  [& args]
  (apply (impl 'add-task-file!) args))

;; answers the new pldb value (swap!'s result), opaque to callers
(m/=> add-task-file! [:=> [:cat Id Id] :map])

(defn check-file-conflicts
  {:arglists '([requesting-slave files])}
  [& args]
  (apply (impl 'check-file-conflicts) args))

;; nil for no files; otherwise a (possibly empty) seq. files is membero'd, so
;; any sequential works; a set does not unify and is not accepted
(m/=> check-file-conflicts [:=> [:cat [:maybe Id] [:maybe [:sequential Id]]]
                            [:maybe [:sequential FileConflict]]])

(defn check-would-deadlock
  {:arglists '([task-id dep-task-id])}
  [& args]
  (apply (impl 'check-would-deadlock) args))

(m/=> check-would-deadlock [:=> [:cat Id Id] :boolean])

(defn db-stats
  {:arglists '([])}
  [& args]
  (apply (impl 'db-stats) args))

(m/=> db-stats [:=> [:cat] [:map [:slaves [:int {:min 0}]] [:tasks [:int {:min 0}]] [:claims [:int {:min 0}]]]])

(defn get-all-claims
  {:arglists '([])}
  [& args]
  (apply (impl 'get-all-claims) args))

(m/=> get-all-claims [:=> [:cat] [:vector Claim]])

(defn get-claim-for-file
  {:arglists '([file-path])}
  [& args]
  (apply (impl 'get-claim-for-file) args))

(m/=> get-claim-for-file [:=> [:cat Id] [:maybe Claim]])

(defn get-files-for-task
  {:arglists '([task-id])}
  [& args]
  (apply (impl 'get-files-for-task) args))

(m/=> get-files-for-task [:=> [:cat Id] [:vector Id]])

(defn get-logic-db-atom
  {:arglists '([])}
  [& args]
  (apply (impl 'get-logic-db-atom) args))

(m/=> get-logic-db-atom [:=> [:cat] LogicDb])

(defn release-claim-for-file!
  {:arglists '([file-path])}
  [& args]
  (apply (impl 'release-claim-for-file!) args))

(m/=> release-claim-for-file! [:=> [:cat Id] :boolean])

(defn release-claims-for-slave!
  {:arglists '([slave-id])}
  [& args]
  (apply (impl 'release-claims-for-slave!) args))

(m/=> release-claims-for-slave! [:=> [:cat Id] :nil])

(defn release-claims-for-task!
  {:arglists '([task-id])}
  [& args]
  (apply (impl 'release-claims-for-task!) args))

(m/=> release-claims-for-task! [:=> [:cat Id] :nil])

(defn remove-claim!
  {:arglists '([file-path slave-id])}
  [& args]
  (apply (impl 'remove-claim!) args))

(m/=> remove-claim! [:=> [:cat Id Id] :nil])

(defn reset-db!
  {:arglists '([])}
  [& args]
  (apply (impl 'reset-db!) args))

(m/=> reset-db! [:=> [:cat] :nil])

(defn slave-exists?
  {:arglists '([slave-id])}
  [& args]
  (apply (impl 'slave-exists?) args))

(m/=> slave-exists? [:=> [:cat Id] :boolean])
