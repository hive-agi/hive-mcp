(ns hive-mcp.extensions.deftool-contract-test
  "Contract facets for schema->tool: the tool-def it projects from a
   registered schema key."
  (:require [clojure.test :refer [deftest is]]
            [hive-spi.schema.registry :as sreg]
            [hive-mcp.extensions.deftool :as dt]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(sreg/register! ::probe-args [:map [:name :string] [:times {:optional true} :int]])
(sreg/register! ::probe-query [:map [:q :string]])

(deftrifecta-from-schema schema->tool-projects-the-registered-schema
  #'dt/schema->tool
  {:in [:cat [:enum "m11-tool-a" "m11-tool-b"] [:maybe [:enum "probe tool"]]
        [:enum ::probe-args ::probe-query]
        [:fn {:gen/elements [identity (fn [p] (:name p))]} ifn?]]
   :out [:map [:name :string] [:description [:maybe :string]]
         [:inputSchema [:map [:type [:= "object"]] [:properties :map]]]
         [:handler ifn?]]
   :rel (fn [[tool-name description schema-key _] out]
          (and (= tool-name (:name out))
               (= description (:description out))
               (= (if (= ::probe-args schema-key) #{:name :times} #{:q})
                  (set (keys (get-in out [:inputSchema :properties]))))))
   :num-tests 50})

(deftest the-projected-handler-guards-with-the-schema
  (let [t (dt/schema->tool "m11-tool" nil ::probe-query identity)]
    (is (= {:q "x"} ((:handler t) {:q "x"})))
    (is (= :schema/invalid
           (try ((:handler t) {}) :no-throw
                (catch clojure.lang.ExceptionInfo e (:error (ex-data e))))))))
