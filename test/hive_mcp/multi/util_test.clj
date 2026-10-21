(ns hive-mcp.multi.util-test
  "Name-matched facets for `hive-mcp.multi.util/decode-mcp-text`: the MCP
   text envelope ({:text json}) decodes to a keyword-keyed value; anything
   else (non-JSON text, a non-string :text, a non-map) comes back unchanged."
  (:require [clojure.data.json :as json]
            [hive-mcp.multi.util :as util]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Payload
  [:or [:map [:id [:enum "p-1" "p-2"]] [:count [:int {:min 0 :max 9}]]]
   [:map [:entry [:map [:id [:enum "e-1"]]]]]
   [:vector {:max 2} [:int {:min 0 :max 9}]]])

(def ^:private Envelope
  "A value decode-mcp-text may receive: a JSON envelope (with and without
   :type), an envelope whose text is not JSON, a non-string :text, or a
   value that is not a map at all."
  [:or {:gen/schema [:tuple Payload
                     [:enum :json :json :json-typed :not-json :text-not-string :string :nil :vector]]
        :gen/fmap (fn [[payload shape]]
                    (case shape
                      :json            {:text (json/write-str payload)}
                      :json-typed      {:type "text" :text (json/write-str payload)}
                      :not-json        {:type "text" :text "plain words, not json"}
                      :text-not-string {:text payload}
                      :string          (json/write-str payload)
                      :nil             nil
                      :vector          [payload]))}
   :map :string :nil [:vector :any]])

(defn- decodable? [v]
  (and (map? v) (string? (:text v)) (not= "plain words, not json" (:text v))))

(deftrifecta-from-schema decode-mcp-text-decodes-only-json-envelopes
  #'util/decode-mcp-text
  {:in [:cat Envelope]
   ;; the contract's :out is :any; this is the output domain over the
   ;; generated inputs, tight enough to derive mutants from
   :out [:or Payload [:map [:text :any]] :string :nil [:vector Payload]]
   ;; a failed decode returns the input value (equal, carrying the rescue's
   ;; error metadata), so the passthrough arm is checked with =, not identical?
   :rel (fn [[v] out]
          (if (decodable? v)
            (= out (json/read-str (:text v) :key-fn keyword))
            (= out v)))
   :classify (fn [[v] _] (if (decodable? v) :decoded :passthrough))
   :classify-domain #{:decoded :passthrough}
   :golden-path "test/golden/hive-mcp/multi/util-decode-mcp-text.edn"
   :num-tests 300})
