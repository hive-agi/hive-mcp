(ns hive-mcp.embeddings.bulk
  "Bulk embedding: many texts, none of them on a caller's critical path.

   A bulk caller (carto re-embedding a worktree through the addon
   :embedding/embed-batch port) must neither compete with interactive work nor
   hold the provider for longer than an interactive caller can wait. So it
   always runs in the :batch lane, whatever lane the calling thread carries,
   and sends at most `max-texts` texts per provider call: the gate permit is
   returned between chunks, and an interactive call queued at the provider
   waits for one short chunk instead of one long batch.

   Kanban 20261020223606-16e6e1ef: a back-to-back stream of ~8.5 s batches
   starved interactive embeds for two hours."
  (:require [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.shared-gate :as shared]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-max-texts
  "Texts per provider call when [:embeddings :batch :max-texts] is unset."
  8)

(defn max-texts-from-config
  "Chunk size from a config map: [:embeddings :batch :max-texts] when it is a
   positive integer, else `default-max-texts`."
  [cfg]
  (let [n (get-in cfg [:embeddings :batch :max-texts])]
    (if (pos-int? n) n default-max-texts)))

(m/=> max-texts-from-config [:=> [:cat :any] pos-int?])

(defn- configured-max-texts
  "Chunk size from the global config, resolved lazily; any failure yields the
   default."
  []
  (try
    (max-texts-from-config ((requiring-resolve 'hive-mcp.config.core/get-global-config)))
    (catch Throwable _ default-max-texts)))

(defn embed-batch!
  "Embed `texts` with `provider` in the :batch lane, `max-texts` per call.
   Answers one vector per text, in order. A failed chunk throws; the caller
   gets no partial result."
  ([provider texts] (embed-batch! provider texts {}))
  ([provider texts {:keys [max-texts]}]
   (let [n (if (pos-int? max-texts) max-texts (configured-max-texts))]
     (shared/with-lane :batch
       (into [] (mapcat #(proto/embed-batch provider (vec %))) (partition-all n texts))))))

(m/=> embed-batch! [:function
                    [:=> [:cat proto/Provider [:sequential :string]] [:vector :any]]
                    [:=> [:cat proto/Provider [:sequential :string]
                          [:map [:max-texts {:optional true} [:maybe pos-int?]]]]
                     [:vector :any]]])
