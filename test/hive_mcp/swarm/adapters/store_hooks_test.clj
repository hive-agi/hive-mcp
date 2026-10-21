(ns hive-mcp.swarm.adapters.store-hooks-test
  "Facets for hive-mcp.swarm.adapters.store-hooks.

   scope-rows is pure: with a VALID session-ref it keeps exactly the rows the
   ref owns under hive-mcp.session.identity (a coordinator owns its session
   and every descendant, anything else only its own); without one it passes
   the rows through. The trifecta drives the scoped branch with a fixed
   parent-of chain s3 -> s2 -> s1 and an independent ownership table as the
   oracle. (The pass-through branch already has its own trifecta in
   adapters-contract-test.)"
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.adapters.store-hooks :as store-hooks]
            [hive-schemas.test :as hst]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private chain {"s2" "s1" "s3" "s2"})

(def ^:private owned
  "Which row sessions each ref owns over `chain`, written out by hand."
  {[:coordinator "s1"] #{"s1" "s2" "s3"}
   [:coordinator "s2"] #{"s2" "s3"}
   [:coordinator "s3"] #{"s3"}
   [:ling "s1"]        #{"s1"}
   [:ling "s2"]        #{"s2"}
   [:ling "s3"]        #{"s3"}})

(def ^:private SessionRef
  [:map [:session/id [:enum "s1" "s2" "s3"]] [:session/kind [:enum :coordinator :ling]]])

(def ^:private Rows [:vector {:gen/max 6} [:map [:k [:enum "s1" "s2" "s3" "s4"]]]])

(hst/deftrifecta-from-schema scope-rows-keeps-what-the-session-owns
  hive-mcp.swarm.adapters.store-hooks/scope-rows
  {:in  [:cat [:map [:session-ref SessionRef] [:parent-of [:= {} chain]]] [:= :k] Rows]
   :out [:sequential :map]
   :rel (fn [[{:keys [session-ref]} _ rows] out]
          (let [mine (owned [(:session/kind session-ref) (:session/id session-ref)])]
            (= (filterv #(contains? mine (:k %)) rows) (vec out))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/swarm/adapters/store-hooks-scope-rows.edn"})

(deftest scope-rows-examples-test
  (let [rows [{:k "s1"} {:k "s2"} {:k "s3"} {:k "s4"}]]
    (is (= [{:k "s2"} {:k "s3"}]
           (store-hooks/scope-rows {:session-ref {:session/id "s2" :session/kind :coordinator}
                                    :parent-of chain}
                                   :k rows))
        "a coordinator keeps its own rows and its descendants'")
    (is (= [{:k "s2"}]
           (store-hooks/scope-rows {:session-ref {:session/id "s2" :session/kind :ling}
                                    :parent-of chain}
                                   :k rows))
        "a ling keeps only its own")
    (is (= rows (store-hooks/scope-rows {} :k rows)) "no session-ref: unscoped")))

(deftest install-answers-the-hooks-or-nil-test
  (is (m/validate [:maybe store-hooks/Hooks] (store-hooks/install!))))
