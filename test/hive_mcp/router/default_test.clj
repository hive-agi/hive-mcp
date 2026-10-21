(ns hive-mcp.router.default-test
  "Golden over the default IRouter / IEscalator record. The embedder-block
   port is satisfied by a constant 0-arg fn (the seam make-router exposes),
   so the live config is never read."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.router.default :as default]
            [hive-mcp.router.protocol :as proto]
            [hive-test.golden :refer [deftest-golden-fn]]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private block
  {:default    :small
   :routes     {:note :small :plan :heavy}
   :providers  {:small {:impl :ollama :model "s" :dimension 384 :max-tokens 100}
                :heavy {:impl :venice :model "h" :dimension 768 :max-tokens 10000}}
   :escalation {:heavy-tier-key :heavy}})

(defn- scenario []
  (let [r     (default/make-router (constantly block))
        empty (default/make-router (constantly {}))
        small (:ok (proto/resolve-for-type r :note))
        flips (atom block)
        live  (default/make-router #(deref flips))
        before (proto/resolve-for-type live :note)
        _     (swap! flips assoc-in [:routes :note] :heavy)
        after (proto/resolve-for-type live :note)
        inv-before @(:invalidated? r)]
    (proto/invalidate! r)
    {:routed-keyword   (proto/resolve-for-type r :note)
     :routed-string    (proto/resolve-for-type r "plan")
     :unrouted-default (proto/resolve-for-type r :unknown)
     :no-default       (proto/resolve-for-type empty :note)
     :small-doc        (proto/escalate-if-large r small 50)
     :large-doc        (proto/escalate-if-large r small 500)
     :hot-flip         {:before before :after after}
     :invalidated      {:before inv-before :after @(:invalidated? r)}}))

(deftest-golden-fn default-router-golden
  "test/golden/router/default-router.edn"
  scenario)

(deftest make-router-satisfies-both-ports
  (is (m/validate default/Router (default/make-router (constantly block))))
  (is (m/validate default/Router (default/make-router))))
