(ns hive-mcp.spi.emacs.facet-test
  "Facets for hive-mcp.spi.emacs (cov-facets f07): an in-memory IEmacsDaemons
   adapter (ling -> daemon bindings in an atom) is installed through the
   facade's install!, the facade fns are driven through it in an explicit
   sequence, and the port laws run against the adapter."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.spi.emacs :as emacs]
            [hive-schemas.port :refer [deftest-port-contract]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defrecord InMemoryDaemons [bindings]
  emacs/IEmacsDaemons
  (-default-daemon-id [_] "server")
  (-ensure-default-daemon! [_] "server")
  (-select-daemon-for-ling [_ ling-id]
    (if-let [d (get @bindings ling-id)]
      {:daemon-id d :reason :bound}
      {:daemon-id "server" :reason :default}))
  (-bind-ling! [_ daemon-id slave-id] (swap! bindings assoc slave-id daemon-id) daemon-id)
  (-unbind-ling! [_ _daemon-id slave-id] (swap! bindings dissoc slave-id) nil)
  (-get-daemon-for-ling [_ slave-id] (get @bindings slave-id)))

(deftest-golden emacs-facade-golden
  "test/golden/hive-mcp/f07-spi-emacs-facade.edn"
  (let [bindings (atom {})]
    (emacs/install! (->InMemoryDaemons bindings))
    (try
      ;; Sequenced with let: a map literal of more than 8 entries is a hash
      ;; map and does not evaluate its values in the order written.
      (let [default        (emacs/default-daemon-id)
            ensure         (emacs/ensure-default-daemon!)
            select-unbound (emacs/select-daemon-for-ling "ling-1")
            bind           (emacs/bind-ling! "d2" "ling-1")
            get-bound      (emacs/get-daemon-for-ling "ling-1")
            select-bound   (emacs/select-daemon-for-ling "ling-1")
            bound-snapshot @bindings
            unbind         (emacs/unbind-ling! "d2" "ling-1")
            get-unbound    (emacs/get-daemon-for-ling "ling-1")]
        {:steps [[:default default]
                 [:ensure ensure]
                 [:select-unbound select-unbound]
                 [:bind bind]
                 [:get-bound get-bound]
                 [:select-bound select-bound]
                 [:bindings-while-bound bound-snapshot]
                 [:unbind unbind]
                 [:get-unbound get-unbound]
                 [:reset-cache (emacs/reset-cache!)]]})
      (finally (emacs/uninstall!)))))

(deftest-port-contract in-memory-daemons-port-laws
  {:port/methods
   {emacs/-default-daemon-id      {:in [:cat] :out [:= "server"]}
    emacs/-select-daemon-for-ling {:in [:cat [:string {:min 1}]]
                                   :out [:map [:daemon-id :string] [:reason [:enum :bound :default]]]}
    emacs/-bind-ling!             {:in [:cat [:string {:min 1}] [:string {:min 1}]]
                                   :out :string
                                   :rel (fn [[daemon] out] (= daemon out))}
    emacs/-get-daemon-for-ling    {:in [:cat [:string {:min 1}]] :out [:maybe :string]}}}
  (->InMemoryDaemons (atom {})))

(deftest install-returns-the-impl
  (let [impl (->InMemoryDaemons (atom {}))]
    (try (is (= impl (emacs/install! impl)))
         (finally (emacs/uninstall!)))))
