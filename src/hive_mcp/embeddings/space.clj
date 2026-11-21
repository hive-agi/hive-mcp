(ns hive-mcp.embeddings.space
  "EmbeddingSpace: the identity of a vector space.

   The value layer now lives in `hive-spi.embeddings.space`, so a vector-store
   backend addon can stamp, admit and audit collections with no host on the
   classpath. This namespace re-exports it unchanged, so every host caller
   keeps its `space/...` spelling and there is one implementation.

   Strata (each calls only downward):

     guard    check-compatible!   refuse to mix two spaces
     identity space-id            stable sha256 of the canonical space
     value    ->space             a normalized, validated EmbeddingSpace
     floor    unknown-space-id    what an unattributed vector set answers to"
  (:require [hive-spi.embeddings.space :as spi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Schemas and constants.
(def Metric spi/Metric)
(def EmbeddingSpace spi/EmbeddingSpace)
(def space-keys spi/space-keys)
(def defaults spi/defaults)
(def SpaceId spi/SpaceId)
(def SpaceKey spi/SpaceKey)
(def unknown-space-id spi/unknown-space-id)
(def stamp-keys spi/stamp-keys)
(def StampMetadata spi/StampMetadata)
(def Admission spi/Admission)
(def AuditVerdict spi/AuditVerdict)
(def AuditRow spi/AuditRow)

;; Functions. Contracts (m/=>) are declared once, on the hive-spi originals.
(def ->space spi/->space)
(def config->space spi/config->space)
(def canonical spi/canonical)
(def space-id spi/space-id)
(def differing-keys spi/differing-keys)
(def compatible? spi/compatible?)
(def check-compatible! spi/check-compatible!)
(def space->metadata spi/space->metadata)
(def metadata->space spi/metadata->space)
(def admit spi/admit)
(def audit spi/audit)
