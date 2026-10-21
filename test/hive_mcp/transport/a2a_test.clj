(ns hive-mcp.transport.a2a-test
  "Facets for hive-mcp.transport.a2a (cov-facets f07). bind-host is the pure
   exposure policy and is driven from its contract. The gateway itself is
   pinned in its stopped state only: start! binds a real aleph server (the
   started paths are in a2a_bind_test.clj and server/transport/
   a2a_gateway_test.clj), and the stopped-state answers are what a unit run
   can observe without a socket."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.transport.a2a :as a2a]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema bind-host-contract a2a/bind-host
  {:in  [:cat [:maybe [:enum "" " " "key-1"]] [:maybe [:enum "0.0.0.0" "10.1.2.3" "127.0.0.1"]]]
   :out [:string {:min 1}]
   :rel (fn [[api-key requested] out]
          (if (str/blank? api-key)
            (= out "127.0.0.1")
            (= out (or requested "0.0.0.0"))))
   :golden-path "test/golden/hive-mcp/f07-a2a-bind-host.edn"})

(deftest-golden a2a-gateway-stopped-golden
  "test/golden/hive-mcp/f07-a2a-gateway-stopped.edn"
  (do (a2a/stop!)
      {:status     (a2a/status)
       :stop-again (a2a/stop!)}))

(deftest a-keyless-gateway-never-binds-wide
  (is (= "127.0.0.1" (a2a/bind-host nil "0.0.0.0")))
  (is (= "127.0.0.1" (a2a/bind-host "" "10.1.2.3"))))
