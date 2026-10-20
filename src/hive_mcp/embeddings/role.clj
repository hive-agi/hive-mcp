(ns hive-mcp.embeddings.role
  "Pure, revisioned text shaping for retrieval queries and stored documents.
   Shaping happens before the provider port; no embedding provider needs to
   know about query/document roles. A template revision must be included in
   the embedding-space identity before a non-raw template is enabled."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def qwen-retrieval-instruction
  "Given a memory search query, retrieve stored memory passages relevant to the query")

(def TemplateSpec
  "A template revision, bare or under :template; nil means :raw. Unknown
   revisions are admitted and fail closed in the body."
  [:or [:maybe :keyword] [:map [:template {:optional true} [:maybe :keyword]]]])

(defn shape-query
  "Shape query TEXT according to SPEC (:template revision or revision keyword).
   Nil/:raw preserves the original bytes; unknown revisions fail closed."
  [spec text]
  (let [revision (if (map? spec) (:template spec) spec)]
    (case (or revision :raw)
      :raw text
      :qwen3-retrieval-v1
      (str "Instruct: " qwen-retrieval-instruction "\nQuery: " text)
      (throw (ex-info "Unknown query template revision"
                      {:template revision :role :query})))))

(m/=> shape-query [:=> [:cat TemplateSpec :string] :string])

(defn shape-document
  "Serialize ENTRY for a backend's document role. :raw/:content-only
   serialize content alone (including structured JSON content); :metadata-envelope
   mirrors the Chroma Type/Tags/Content document format. The caller chooses
   the mode explicitly so existing indexed vectors are never rewritten."
  [spec {:keys [content type tags]}]
  (let [revision (if (map? spec) (:template spec) spec)
        serialized (if (string? content) content (json/write-str content))]
    (case (or revision :raw)
      (:raw :content-only) serialized
      :metadata-envelope
      (str "Type: " type "\n"
           (when (seq tags) (str "Tags: " (str/join ", " tags) "\n"))
           "Content: " serialized)
      (throw (ex-info "Unknown document template revision"
                      {:template revision :role :document})))))

;; Non-string content is JSON-serialized, so content is open.
(m/=> shape-document [:=> [:cat TemplateSpec
                           [:map
                            [:content :any]
                            [:type {:optional true} :any]
                            [:tags {:optional true} [:maybe [:sequential :any]]]]]
                      :string])
