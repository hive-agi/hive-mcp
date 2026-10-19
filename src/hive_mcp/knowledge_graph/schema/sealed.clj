(ns hive-mcp.knowledge-graph.schema.sealed
  "Schema of sealed KG edges (hive-knowledge seal.kg-edges).

   Sealing an edge retracts it and writes one `:kg-sealed/blob` row in its
   place; deleting an entry while rows exist that the delete cannot open
   writes one `:kg-sealed/tombstone` per id. The attributes belong to the KG
   schema every store opens with: DataScript and Datalevin fix their schema
   at open time, so an attribute declared later by a first-use transact gets
   neither :db/unique nor :db/noHistory there. Datahike gets the same specs
   from norm 009-kg-sealed.

   Must match hive-knowledge.seal.kg-edges/blob-schema and tombstone-schema.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def sealed-edge-schema
  "DataScript-shaped schema of the sealed-edge attributes."
  {:kg-sealed/blob      {:db/noHistory true
                         :db/doc "A sealed KG edge: WORK(payload) or WORK(VAULT(payload))"}
   :kg-sealed/tombstone {:db/unique :db.unique/identity
                         :db/noHistory true
                         :db/doc "SHA-256 (hex) of a deleted entry id that sealed rows may still name"}})
