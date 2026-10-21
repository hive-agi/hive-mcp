(ns hive-mcp.tools.memory.crud.add-params
  "Params `memory add` refuses instead of silently dropping.

   `embedder` looks like a per-call embedding-provider override, but no write
   path reads it: the store embeds with the provider bound to its slot. A
   caller that passes it (e.g. `embedder \"noop\"` to skip a broken embed seam)
   was told nothing and got the default embedder's failure. Refusing it names
   the param, the same way `memory edit` rejects an unknown param."
  (:require [hive-mcp.tools.core :as core :refer [mcp-error]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def unsupported
  "Add params that look honoured but are not, with the reason given back."
  {:embedder (str "the embedder is chosen per store slot, not per call; "
                  "nothing would be done with it")})

(defn refuse-unsupported
  "An MCP error naming each unsupported param present in `args`, or nil."
  [args]
  (when-let [ks (seq (sort (filter #(contains? args %) (keys unsupported))))]
    (mcp-error (str "memory add: unsupported param"
                    (when (next ks) "s") " "
                    (apply str (interpose ", " (map name ks)))
                    " (" (apply str (interpose "; " (map unsupported ks))) ")."
                    " Nothing was written."))))

(m/=> refuse-unsupported [:=> [:cat [:maybe :map]] [:maybe core/McpResponse]])

(defn guard
  "Wrap an add handler so an unsupported param is refused before it runs."
  [handler]
  (fn [args]
    (or (refuse-unsupported args)
        (handler args))))

(m/=> guard [:=> [:cat ifn?] ifn?])
