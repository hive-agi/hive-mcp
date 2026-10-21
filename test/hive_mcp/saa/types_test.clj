(ns hive-mcp.saa.types-test
  "Facets for the SAA closed sums: a trifecta on raw-msg->phase-message over
   every documented raw shape (and unrecognized ones), each mapped to its
   PhaseMessage variant with the phase stamp resolved as documented
   (:saa-phase, then :phase, then the argument), plus a golden over the ADT
   constructors."
  (:require [hive-mcp.saa.types :as types]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private raw-entries
  [[:phase {:optional true} [:enum :silence :act]]
   [:saa-phase {:optional true} [:enum :abstract]]
   [:content {:optional true} [:enum "hello" "done"]]
   [:data {:optional true} [:enum "payload"]]
   [:error {:optional true} [:enum "boom"]]
   [:message {:optional true} [:enum "bad"]]])

(def ^:private Raw
  ;; Generated with :type always drawn (an optional :type would send half the
  ;; draws into the default arm); :tool-use and :none stand for unrecognized
  ;; shapes, :none being dissoc'ed to a type-less message. :error and
  ;; :saa-complete are their own :or branch: as 1 of 7 flat enum members
  ;; :pm/error drew 5 of 100 on an unlucky seed, under the classify floor.
  (into [:map {:gen/schema (into [:map [:type [:or [:enum :error :saa-complete]
                                               [:enum :message :complete :result :tool-use :none]]]]
                                 raw-entries)
               :gen/fmap (fn [m] (if (= :none (:type m)) (dissoc m :type) m))}
         [:type {:optional true} [:enum :message :complete :result :error :saa-complete :tool-use]]]
        raw-entries))

(def ^:private expected-variant
  {:message :pm/chunk :complete :pm/phase-complete :result :pm/phase-complete
   :error :pm/error :saa-complete :pm/saa-complete})

(deftrifecta-from-schema raw-msg->phase-message-contract
  hive-mcp.saa.types/raw-msg->phase-message
  {:in [:cat Raw [:maybe [:enum :silence :abstract :act]]]
   :out (-> (m/function-schemas) (get-in ['hive-mcp.saa.types 'raw-msg->phase-message :schema])
            m/children second)
   :rel (fn [[raw phase] out]
          (let [variant (get expected-variant (:type raw) :pm/chunk)
                ph (or (:saa-phase raw) (:phase raw) phase)]
            (and (= variant (:adt/variant out))
                 (case variant
                   :pm/chunk (and (= ph (:phase out))
                                  (= (if (= :message (:type raw)) (or (:content raw) (:data raw)) raw)
                                     (:content out)))
                   :pm/phase-complete (and (= ph (:phase out))
                                           (= (or (:content raw) (:data raw) raw) (:payload out)))
                   :pm/error (and (= ph (:phase out)) (= (or (:error raw) (:message raw)) (:error out)))
                   :pm/saa-complete (= (dissoc raw :type) (:summary out))))))
   :classify (fn [_ out] (:adt/variant out))
   :classify-domain #{:pm/chunk :pm/phase-complete :pm/error :pm/saa-complete}
   :golden-path "test/golden/hive-mcp/saa/types-raw-msg-phase-message.edn"})

(golden/deftest-golden saa-adt-constructors-golden
  "test/golden/hive-mcp/saa/types-adt-constructors.edn"
  {:entry (types/saa-registry-entry :saa/tool-intent {:intent :read :tools ["r"] :owner :o})
   :entry? (types/saa-registry-entry? (types/saa-registry-entry :saa/tool-intent {:intent :read :tools ["r"] :owner :o}))
   :not-entry? (types/saa-registry-entry? {:x 1})
   :message (types/phase-message :pm/started {:phase :silence})
   :message? (types/phase-message? (types/phase-message :pm/started {:phase :silence}))
   :phase-value-valid (m/validate types/PhaseMessageValue (types/phase-message :pm/error {:phase :act :error "e"}))
   :restore-arg-nil (m/validate types/RestoreArg nil)})
