(ns hive-mcp.nats.m16-facet-test
  "Trifectas for the pure pieces of nats.bridge (subject builders) and
   nats.client (clamp-depth). The connected/subscription paths are covered by
   bridge_test.clj / client_test.clj without a server."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.nats.bridge :as bridge]
            [hive-mcp.nats.client :as client]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- out-schema [nsym fsym]
  (-> (m/function-schemas) (get-in [nsym fsym :schema]) m/children second))

(def ^:private Token
  "The true domain of (name x): strings, keywords, symbols, nil. Numbers are
   outside it — (name 3) throws ClassCastException; recorded as a bug on
   safe-token, never papered over here."
  [:or :string :keyword :symbol :nil])

(hst/deftrifecta-from-schema shout-subject-contract
  hive-mcp.nats.bridge/shout-subject
  {:in [:cat Token Token]
   :out (out-schema 'hive-mcp.nats.bridge 'shout-subject)
   :rel (fn [[project agent] out]
          (and (clojure.string/starts-with? out "hive.v1.shout.")
               (let [ps (if (or (nil? project) (and (string? project) (re-matches #"\s*" project)))
                         "global" (name project))
                     as (if (or (nil? agent) (and (string? agent) (re-matches #"\s*" agent)))
                         "unknown" (name agent))]
                 (= out (str "hive.v1.shout." ps "." as)))))
   :golden-path "test/golden/hive-mcp/m16-shout-subject.edn"})

(hst/deftrifecta-from-schema wave-subject-contract
  hive-mcp.nats.bridge/wave-subject
  {:in [:cat Token Token]
   :out (out-schema 'hive-mcp.nats.bridge 'wave-subject)
   :rel (fn [[run-id task-id] out]
          (and (clojure.string/starts-with? out "hive.v1.wave.")
               (clojure.string/includes? out ".completed.")))
   :golden-path "test/golden/hive-mcp/m16-wave-subject.edn"})

(def ^:private Nested
  "Bounded nested data for clamp-depth."
  [:or :string :int :boolean :nil
   [:vector {:gen/max 2} [:or :string :int]]
   [:map {:gen/max 2} [:x :int]]])

(hst/deftrifecta-from-schema clamp-depth-contract
  hive-mcp.nats.client/clamp-depth
  {:in [:cat Nested]
   :out (out-schema 'hive-mcp.nats.client 'clamp-depth)
   :rel (fn [[in] out]
          ;; scalars pass through; collections come back as collections
          (if (coll? in) (coll? out) (= in out)))
   ;; clamp-depth's true output domain is :any (scalars pass through
   ;; unchanged), and :any yields no schema-derived mutants; the golden below
   ;; pins the truncation markers instead.
   :mutation false
   :golden-path "test/golden/hive-mcp/m16-clamp-depth.edn"})

;; Deep-nest golden: clamped markers appear exactly at the cap.
(def ^:private deep (reduce (fn [acc _] {:a acc}) :leaf (range 40)))

(golden/deftest-golden nats-pure-golden
  "test/golden/hive-mcp/m16-nats-pure.edn"
  {:clamp-deep (get-in (client/clamp-depth deep) (vec (interpose :a (repeat 33 :a))))
   :clamp-scalar (client/clamp-depth "plain")
   :clamp-map (client/clamp-depth {:k {:k2 {:k3 1}}})
   :clamp-set (client/clamp-depth #{1 2})
   :clamp-explicit-depth (client/clamp-depth {:a {:b {:c :d}}} 1)
   :shout-nil-nil (bridge/shout-subject nil nil)
   :shout-blank (bridge/shout-subject "  " "")
   :shout-kw (bridge/shout-subject :hive :forja-1)
   :event-subject (bridge/event-subject :agent-spawn)
   :event-wildcard (bridge/event-wildcard)
   :agent-subject (bridge/agent-subject "ling-1" :completed)
   :agent-wildcard (bridge/agent-wildcard :failed)
   :tool-subject (bridge/tool-subject :memory-add)
   :tool-subject-nil (bridge/tool-subject nil)
   :tool-wildcard (bridge/tool-wildcard)
   :wave-subject (bridge/wave-subject "run-1" "task-9")
   :shout-wildcard-0 (bridge/shout-wildcard)
   :shout-wildcard-1 (bridge/shout-wildcard "hive")})

(deftest connected-and-stop-are-safe-test
  ;; No server in unit runs: graceful-degradation contract is nil/false, never throw.
  (is (some? [(client/connected?)]))
  (is (nil? (client/stop!)))
  (is (nil? (client/remove-connection-watch! :m16-test))))
