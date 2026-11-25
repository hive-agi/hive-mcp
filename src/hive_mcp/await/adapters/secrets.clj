(ns hive-mcp.await.adapters.secrets
  "Adapter: the ISecretResolver await sources resolve a connection ref
   through.

   A ref NAMES a secret, it never is one: `pass:<path>` reads the first line
   of `pass show <path>` (hive-mcp.config.secrets/pass-show) and
   `env:<NAME>` reads an environment variable. Anything else is refused at
   create time by the source (see ref-kind), so an inline password never
   reaches the awaiter registry, which is persisted as plain EDN."
  (:require [clojure.string :as str]
            [hive-mcp.await.sources.boundary :as b]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn ref-kind
  "[:pass path] or [:env name] for a secret ref, or nil when `ref` is not
   one. Pure."
  [ref]
  (when (string? ref)
    (let [s (str/trim ref)]
      (cond
        (re-matches #"pass:[A-Za-z0-9._/@+-]+" s) [:pass (subs s 5)]
        (re-matches #"env:[A-Za-z_][A-Za-z0-9_]*" s) [:env (subs s 4)]
        :else nil))))

(m/=> ref-kind [:=> [:cat :any] [:maybe [:tuple [:enum :pass :env] :string]]])

(defn- missing [ref]
  (ex-info (str "await: secret " ref " did not resolve")
           {:error :await/secret-missing :ref ref}))

(defrecord PassEnvResolver []
  b/ISecretResolver
  (resolve-secret! [_ ref]
    (let [[kind k] (ref-kind ref)
          v (case kind
              :pass ((requiring-resolve 'hive-mcp.config.secrets/pass-show) k)
              :env  (System/getenv ^String k)
              nil)]
      (if (str/blank? v) (throw (missing ref)) v))))

(def default-resolver
  "The process resolver: pass(1) and the environment."
  (->PassEnvResolver))
