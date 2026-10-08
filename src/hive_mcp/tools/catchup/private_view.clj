(ns hive-mcp.tools.catchup.private-view
  "Catchup's per-caller seams that run BEFORE it reads memory.

     :catchup/pre-query      a fn, or a collection of fns, each called as
                             (f {:caller-id :directory :project-id}) on the
                             request's bindings before any memory query. An
                             addon uses it to put the caller in the state its
                             project needs (hive-knowledge: bind the caller to
                             the project's enclave, unlocking its key). Each
                             runs under `pre-query-budget-ms`; a throw or a
                             timeout is logged and catchup goes on.
     :catchup/private-view?  (f candidate-caller-id project-id) -> truthy when
                             this caller's view of memory is private to it.
                             Catchup then reads around the shared bundle and
                             content caches (bundle-cache/*private-view*). A
                             provider that throws counts as private: a wrong
                             answer then costs a slower catchup, never a leak.

   The core never names the addon behind either key."
  (:require [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.catchup.bundle-cache :as bc]
            [hive-mcp.tools.catchup.caller :as catchup-caller]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def pre-query-budget-ms
  "Longest one pre-query hook may hold catchup: long enough for a human to
   answer a passphrase prompt."
  150000)

(defn hooks-of
  "The hook fns an extension value names: one fn, or the fns in a collection."
  [v]
  (cond
    (fn? v)   [v]
    (coll? v) (filterv fn? v)
    :else     []))

(defn- run-hook [f ctx budget-ms]
  (let [fut (future (f ctx))
        r   (try (deref fut budget-ms ::timeout)
                 (catch Exception e
                   {::error (or (ex-message (or (ex-cause e) e)) (str (class e)))}))]
    (cond
      (= r ::timeout)
      (do (future-cancel fut)
          (log/warn "catchup: pre-query hook timed out after" budget-ms "ms")
          {:timeout true})

      (and (map? r) (contains? r ::error))
      (do (log/warn "catchup: pre-query hook failed:" (::error r))
          {:error (::error r)})

      :else
      {:ok r})))

(defn run-pre-query!
  "Run every :catchup/pre-query hook for `ctx`, in order. One result per hook:
   {:ok value} | {:error message} | {:timeout true}."
  ([ctx] (run-pre-query! ctx pre-query-budget-ms))
  ([ctx budget-ms]
   (mapv #(run-hook % ctx budget-ms) (hooks-of (ext/get-extension :catchup/pre-query)))))

(defn private-view?
  "True when the registered :catchup/private-view? provider says this
   caller's view of memory is private, or throws. False with no provider."
  [raw-caller-id project-id]
  (if-let [f (ext/get-extension :catchup/private-view?)]
    (try
      (boolean (catchup-caller/resolve-for-caller f raw-caller-id project-id))
      (catch Exception e
        (log/warn "catchup: private-view? provider threw; treating the view as private:" (ex-message e))
        true))
    false))

(defn in-view
  "Call `f` (0-arity) with the shared catchup caches bypassed when `private?`."
  [private? f]
  (binding [bc/*private-view* (boolean private?)]
    (f)))
