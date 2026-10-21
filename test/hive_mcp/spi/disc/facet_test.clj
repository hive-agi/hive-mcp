(ns hive-mcp.spi.disc.facet-test
  "Facets for hive-mcp.spi.disc (cov-facets f07): a recording IDiscKnowledge
   adapter is installed through the facade's own install!, every facade fn is
   driven through it, and the port laws run against what `current` answers
   while it is installed."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.spi.disc :as disc]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord RecordingDisc [seen]
  disc/IDiscKnowledge
  (-staleness-value [_ reason]
    (swap! seen conj [:staleness-value reason])
    (get {:file-changed 0.8 :git-commit 0.5} reason))
  (-propagate-staleness! [_ path base reason]
    (swap! seen conj [:propagate path base reason])
    {:propagated 1 :path path})
  (-top-stale-files [_ opts]
    (swap! seen conj [:top-stale opts])
    (vec (take (or (:n opts) 2) ["a.clj" "b.clj" "c.clj"]))))

(defn- with-installed [f]
  (let [seen (atom [])]
    (disc/install! (->RecordingDisc seen))
    (try (f seen) (finally (disc/uninstall!)))))

(deftest-golden disc-facade-golden
  "test/golden/hive-mcp/f07-spi-disc-facade.edn"
  (with-installed
    (fn [seen]
      {:installed?  (instance? RecordingDisc (disc/current))
       :staleness   [(disc/staleness-value :file-changed) (disc/staleness-value :unknown)]
       :propagate   (disc/propagate-staleness! "src/a.clj" 0.8 :file-changed)
       :top-stale   [(disc/top-stale-files {:n 1}) (disc/top-stale-files nil)]
       :reset-cache (disc/reset-cache!)
       :calls       @seen})))

(deftest-port-contract recording-disc-port-laws
  {:port/methods
   {disc/-staleness-value     {:in [:cat [:enum :file-changed :git-commit :other]]
                               :out [:maybe number?]}
    disc/-propagate-staleness! {:in [:cat [:string {:min 1}] [:double {:min 0 :max 1}] :keyword]
                               :out [:map [:propagated :int] [:path :string]]
                               :rel (fn [[path] out] (= path (:path out)))}
    disc/-top-stale-files     {:in [:cat [:maybe [:map [:n {:optional true} [:int {:min 0 :max 3}]]]]]
                               :out [:vector :string]}}}
  (->RecordingDisc (atom [])))

(deftest uninstall-falls-back-to-the-host-adapter
  (with-installed (fn [_] (is (instance? RecordingDisc (disc/current)))))
  (is (not (instance? RecordingDisc (disc/current)))))
