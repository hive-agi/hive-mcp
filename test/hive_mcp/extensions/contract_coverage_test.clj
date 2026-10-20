(ns hive-mcp.extensions.contract-coverage-test
  "Contract coverage gate for src/hive_mcp/extensions: every public defn
   carries an m/=> contract or a reasoned exemption, and every contract's
   argument list and result generate."
  (:require [clojure.test :refer [deftest is]]
            [malli.core :as m]
            [malli.generator :as mg]
            [hive-mcp.extensions.deftool]
            [hive-mcp.extensions.delegate]
            [hive-mcp.extensions.dispatch-wrap]
            [hive-mcp.extensions.lifecycle]
            [hive-mcp.extensions.loader]
            [hive-mcp.extensions.mount-host]
            [hive-mcp.extensions.reactive]
            [hive-mcp.extensions.registry]
            [hive-mcp.extensions.runtime]
            [hive-mcp.extensions.soft]
            [hive-schemas.coverage :as coverage]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private exemptions
  {'hive-mcp.extensions.mount-host/current
   "Identity on every value that is not a stale AddonRegistryHost: it is handed any IMountHost (or :not-a-host) and answers that same value, so its only true contract is [:=> [:cat :any] :any], which contracts nothing. The rebuild arm is pinned by mount-host-contract-test and mount_host_test."})

(coverage/deftest-contract-coverage extension-contracts
  {:paths ["src/hive_mcp/extensions"]
   :exempt exemptions})

(def ^:private extension-namespaces
  '[hive-mcp.extensions.deftool hive-mcp.extensions.delegate hive-mcp.extensions.dispatch-wrap
    hive-mcp.extensions.lifecycle hive-mcp.extensions.loader hive-mcp.extensions.mount-host
    hive-mcp.extensions.reactive hive-mcp.extensions.registry hive-mcp.extensions.runtime
    hive-mcp.extensions.soft])

(deftest every-extension-contract-generates-both-sides
  (doseq [ns-sym extension-namespaces
          [fn-sym {:keys [schema]}] (get (m/function-schemas) ns-sym)
          arity (if (= :function (m/type schema)) (m/children schema) [schema])
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str ns-sym "/" fn-sym " " (pr-str (m/form side))))))

(deftest exemptions-carry-a-reason
  (is (every? #(> (count %) 40) (vals exemptions))))
