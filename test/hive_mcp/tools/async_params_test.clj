(ns hive-mcp.tools.async-params-test
  "Trifectas for the async params every tool schema declares, driven by the
   contracts in `hive-mcp.tools.async-params`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.async-params :as ap]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- opt-out? [props]
  (str/starts-with? (get-in props ["async" :description]) "Set false"))

(deftrifecta-from-schema async-props-wording-follows-the-command-set
  hive-mcp.tools.async-params/async-props
  {:in          ap/DefaultAsyncCommands
   :out         ap/AsyncProps
   :rel         (fn [in out] (= (boolean (seq in)) (opt-out? out)))
   :num-tests   200
   :golden-path "test/golden/tools/async-params/async-props.edn"})

(def ^:private Tool
  "Tools with and without their own `async`, with and without commands that
   queue by default, so both wordings and the keep-own branch are reached."
  [:map
   [:default-async-commands [:maybe [:vector {:max 2} [:enum "run" "scan"]]]]
   [:inputSchema [:map [:properties [:map-of [:enum "async" "path" "limit"]
                                     [:map [:type [:enum "string" "boolean"]]]]]]]])

(deftrifecta-from-schema with-async-params-declares-both-and-keeps-own
  hive-mcp.tools.async-params/with-async-params
  {:in          Tool
   :out         ap/AsyncToolDef
   :rel         (fn [in out]
                  (let [own (get-in in [:inputSchema :properties])]
                    (= (get-in out [:inputSchema :properties])
                       (merge (ap/async-props (:default-async-commands in)) own))))
   :idempotent? true
   :num-tests   200
   :golden-path "test/golden/tools/async-params/with-async-params.edn"})

(def ^:private WireAsync
  "What a client may send for `async`: booleans, nil, the spelled-out strings
   in any case and padding, and non-string non-boolean values."
  [:or :nil :boolean
   [:enum "false" " FALSE " "0" "no" "" "true" "yes" "1" "anything"]
   [:int {:min -2 :max 2}] :keyword])

(defn- flag-oracle [v]
  (cond (nil? v) nil
        (boolean? v) v
        (string? v) (not (contains? #{"false" "0" "no" ""} (str/lower-case (str/trim v))))
        :else true))

(deftrifecta-from-schema async-flag-reads-the-spelled-boolean
  hive-mcp.tools.async-params/async-flag
  {:in          WireAsync
   :out         [:maybe :boolean]
   :rel         (fn [in out] (= out (flag-oracle in)))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/tools/async-params/async-flag.edn"})

(deftest async-flag-string-false-is-false
  (is (false? (ap/async-flag "false")))
  (is (false? (ap/async-flag " No ")))
  (is (true? (ap/async-flag "true")))
  (is (nil? (ap/async-flag nil))))
