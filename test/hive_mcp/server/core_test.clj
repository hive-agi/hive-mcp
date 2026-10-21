(ns hive-mcp.server.core-test
  "Facets for hive-mcp.server.core (cov-facets f07). profile-from and
   apply-overlay are pure and are driven from their contracts. The config
   readers are pinned on the resources on the classpath (key sets only, so a
   value tweak in system.edn does not churn the golden). start!/reset! boot the
   whole Integrant system and are not exercised here; stop! is pinned on its
   no-system arm."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.server.core :as sc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Profile
  [:maybe [:enum "desktop" "k8s-headless" :k8s-minimal :custom]])

(deftrifecta-from-schema profile-from-contract sc/profile-from
  {:in  [:cat Profile Profile Profile]
   :out :keyword
   :rel (fn [[cli env cfg] out]
          (= out (keyword (or cli env cfg "desktop"))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/f07-core-profile-from.edn"})

(def ^:private Overlay
  [:map-of {:gen/max 3} [:enum :hive/a :hive/b :hive/c]
   [:maybe [:map-of {:gen/max 2} [:enum :x :y] :int]]])

(deftrifecta-from-schema apply-overlay-contract sc/apply-overlay
  {:in  [:cat Overlay Overlay]
   :out [:map-of :any :any]
   :rel (fn [[base overlay] out]
          (and
           ;; a key the overlay sets to nil is excluded
           (every? (fn [[k v]] (or (some? v) (not (contains? out k)))) overlay)
           ;; a base key the overlay does not mention survives untouched
           (every? (fn [[k v]] (or (contains? overlay k) (= v (get out k)))) base)
           ;; an overlay map value is merged over the base value
           (every? (fn [[k v]] (or (nil? v) (= (merge (get base k) v) (get out k)))) overlay)))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/f07-core-apply-overlay.edn"})

(deftest-golden core-config-golden
  "test/golden/hive-mcp/f07-core-config.edn"
  {:base-keys         (vec (sort (keys (sc/read-base-config))))
   :missing-profile   (sc/read-profile-config :f07-no-such-profile)
   :headless-keys     (vec (sort (keys (sc/read-profile-config "k8s-headless"))))
   :explicit-profile  (sc/resolve-profile "k8s-minimal")
   :stop-when-stopped (when-not @sc/system (sc/stop!))})

(deftest load-system-config-applies-overrides
  (let [cfg (sc/load-system-config :desktop {:hive/nats nil})]
    (is (not (contains? cfg :hive/nats)) "a nil override excludes the key")
    (is (contains? cfg :hive/config))))
