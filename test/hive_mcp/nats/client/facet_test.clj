(ns hive-mcp.nats.client.facet-test
  "Facets for hive-mcp.nats.client (cov-facets f07). clamp-depth is pure and
   gets trifectas on its two observable regimes (shallow payloads pass through
   unchanged; depth 0 replaces a collection with its marker). The connection
   fns are pinned in their graceful-degradation state: unit runs have no NATS
   server, so every IO fn is the documented nil no-op."
  (:require [hive-mcp.nats.client :as client]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Shallow
  "Payloads well under max-payload-depth: clamp-depth must return them unchanged."
  [:map-of {:gen/max 4} :keyword
   [:or :int :string [:vector {:gen/max 3} :int] [:map-of {:gen/max 2} :keyword :boolean]]])

(deftrifecta-from-schema clamp-depth-shallow-identity client/clamp-depth
  {:in  [:cat Shallow]
   :out Shallow
   :rel (fn [[in] out] (= in out))
   :golden-path "test/golden/hive-mcp/f07-client-clamp-shallow.edn"})

(deftrifecta-from-schema clamp-depth-zero-marks client/clamp-depth
  {:in  [:cat [:or [:map-of {:gen/max 2} :keyword :int]
               [:vector {:gen/max 2} :int]
               [:set {:gen/max 2} :int]
               :int :string]
         [:= 0]]
   :out [:or [:enum "<truncated:map>" "<truncated:vec>" "<truncated:set>"] :int :string]
   :rel (fn [[in _] out]
          (cond (map? in)    (= out "<truncated:map>")
                (vector? in) (= out "<truncated:vec>")
                (set? in)    (= out "<truncated:set>")
                :else        (= out in)))
   :golden-path "test/golden/hive-mcp/f07-client-clamp-zero.edn"})

(deftest-golden client-disconnected-golden
  "test/golden/hive-mcp/f07-client-disconnected.edn"
  {:connected?   (client/connected?)
   :publish!     (client/publish! "hive.v1.test" {:a 1})
   :subscribe!   (client/subscribe! "hive.v1.test" identity)
   :unsubscribe! (client/unsubscribe! "hive.v1.test")
   :stop!        (client/stop!)
   :watch-add    (client/add-connection-watch! ::f07 (fn [_ _] nil))
   :watch-remove (client/remove-connection-watch! ::f07)
   :clamp-seq    (client/clamp-depth (list 1 (list 2 (list 3))) 2)})
