(ns hive-mcp.knowledge-graph.edges.ids
  "Edge id generation. Leaf slice of the edges façade — no edge-graph deps."
  (:require [malli.core :as m]
            [hive-mcp.knowledge-graph.edges.schema :as es]))

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(defn generate-edge-id
  "Generate a unique edge ID."
  []
  (str (random-uuid)))

(m/=> generate-edge-id [:=> [:cat] es/EdgeId])
