(ns hive-mcp.tools.swarm.core-contract-test
  "Contract facets for the swarm response constructors and the pure prompt
   and status helpers of tools.swarm: dispatch's prompt stages, collect's
   chat point, the lazy preset header and the lings formatter."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.swarm.collect :as collect]
            [hive-mcp.tools.swarm.core :as core]
            [hive-mcp.tools.swarm.dispatch :as dispatch]
            [hive-mcp.tools.swarm.prompt :as prompt]
            [hive-mcp.tools.swarm.status :as status]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Payload
  [:map-of [:enum :a :b :status] [:or :int :string]])

(deftrifecta-from-schema mcp-success-carries-the-payload-as-json
  #'core/mcp-success
  {:in [:cat Payload]
   :out [:map [:type [:= "text"]] [:text :string]]
   :rel (fn [[data] out]
          (and (not (:isError out))
               (= (update-keys data name) (json/read-str (:text out)))))
   :num-tests 100})

(deftrifecta-from-schema mcp-error-json-is-an-error-with-the-payload
  #'core/mcp-error-json
  {:in [:cat Payload]
   :out core/SwarmError
   :rel (fn [[data] out] (= (update-keys data name) (json/read-str (:text out))))
   :num-tests 100})

(deftrifecta-from-schema inject-changes-section-prepends-only-a-section
  #'dispatch/inject-changes-section
  {:in [:cat [:string {:max 8}] [:maybe [:string {:min 1 :max 8}]]]
   :out :string
   :rel (fn [[p section] out]
          (if section
            (and (str/starts-with? out section) (str/ends-with? out p))
            (= p out)))
   :classify (fn [[_ section] _] (if section :with-section :no-section))
   :classify-domain #{:with-section :no-section}
   :num-tests 100})

(deftrifecta-from-schema shout-reminder-keeps-the-prompt-first
  #'dispatch/inject-shout-reminder
  {:in [:cat [:string {:max 12}]]
   :out :string
   :rel (fn [[p] out] (and (str/starts-with? out p)
                           (str/includes? out "hivemind_shout")))
   :num-tests 100})

(deftrifecta-from-schema resolve-spawn-mode-defaults-to-claude
  #'dispatch/resolve-spawn-mode
  {:in [:cat [:maybe [:map [:ling/spawn-mode {:optional true}
                            [:enum :claude :vterm :headless :agent-sdk]]]]]
   :out :keyword
   :rel (fn [[d] out] (= out (or (:ling/spawn-mode d) :claude)))
   :classify (fn [[d] _] (if (:ling/spawn-mode d) :explicit :default))
   :classify-domain #{:explicit :default}
   :num-tests 150 :classify-floor 13})

(deftrifecta-from-schema chat-point-response-is-snake-case
  #'collect/chat-point->response
  {:in [:cat collect/ChatPoint]
   :out collect/ChatPointResponse
   :rel (fn [[cp] out] (= out {:run_id (:run-id cp) :tip (:tip cp)
                               :resumable (boolean (:resumable? cp))}))
   :num-tests 100})

(deftrifecta-from-schema format-lings-keys-the-elisp-list-by-id
  #'status/format-lings-for-response
  {:in [:cat [:vector {:max 4} [:map [:slave-id [:enum "a" "b" "c"]]
                                [:name [:enum "x" "y"]]]]]
   :out [:map-of :string [:map [:name :string] [:age-minutes :nil]]]
   :rel (fn [[lings] out]
          (= out (into {} (map (fn [l] [(:slave-id l)
                                         {:name (:name l) :presets nil :cwd nil
                                          :project-id nil :status nil
                                          :age-minutes nil}]))
                       lings)))
   :num-tests 100})

(golden/deftest-golden swarm-pure-helpers-golden
  "test/golden/hive-mcp/tools/swarm/pure-helpers.edn"
  (sorted-map
   :not-loaded      (core/addon-not-loaded-error)
   :error           (core/mcp-error "boom")
   :timeout         (core/mcp-timeout-error "Status check" :extra-data {:slave_id "s1"})
   :changes-none    (dispatch/build-changes-section [])
   :changes-one     (dispatch/build-changes-section [{:file "src/a.clj" :agent "ling-1"}])
   :staleness-none  (dispatch/inject-staleness-context "prompt" {})
   :preset-header   (prompt/build-lazy-preset-header ["tdd" "clarity"])
   :preset-none     (prompt/build-lazy-preset-header [])
   :chat-point      (collect/chat-point->response {:run-id "r1" :tip 4 :resumable? 1})
   :lings-empty     (status/format-lings-for-response nil)))

(deftest every-constructor-arm-discriminates
  (is (true? (:isError (core/mcp-error "x"))))
  (is (nil? (:isError (core/mcp-success {:a 1})))))
