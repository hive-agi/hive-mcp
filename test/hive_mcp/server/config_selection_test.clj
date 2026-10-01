(ns hive-mcp.server.config-selection-test
  "config.edn alone selects the system: [:system :profile] picks the profile,
   [:system :overrides] edits it, HIVE_MCP_CONFIG names the file, and the
   :bare profile boots the core with addon discovery off."
  (:require [clojure.test :refer [deftest is testing]]
            [integrant.core :as ig]
            [hive-mcp.config.path :as config-path]
            [hive-mcp.server.core :as core]
            [hive-mcp.system.layer3 :as layer3]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest profile-precedence-includes-config
  (testing "CLI > env > config.edn > :desktop"
    (is (= :desktop (core/profile-from nil nil nil)))
    (is (= :bare (core/profile-from nil nil :bare)))
    (is (= :bare (core/profile-from nil nil "bare")))
    (is (= :k8s-minimal (core/profile-from nil "k8s-minimal" :bare)))
    (is (= :k8s-headless (core/profile-from "k8s-headless" "k8s-minimal" :bare))))
  (testing "the 2-arity keeps its old meaning"
    (is (= :desktop (core/profile-from nil nil)))))

(deftest config-path-resolution
  (is (= "/h/.config/hive-mcp/config.edn" (config-path/resolve-config-path nil "/h")))
  (is (= "/h/.config/hive-mcp/config.edn" (config-path/resolve-config-path "  " "/h")))
  (is (= "/tmp/second.edn" (config-path/resolve-config-path "/tmp/second.edn" "/h"))))

(deftest apply-overlay-semantics
  (let [base {:t/a {:x 1 :ref (ig/ref :t/b)} :t/b {} :t/c {:y 2}}]
    (testing "a nil value removes the key"
      (is (not (contains? (core/apply-overlay base {:t/c nil}) :t/c))))
    (testing "a map value deep-merges and keeps the base's refs"
      (let [out (core/apply-overlay base {:t/a {:x 9}})]
        (is (= 9 (get-in out [:t/a :x])))
        (is (ig/ref? (get-in out [:t/a :ref])))))
    (testing "an empty overlay is the identity"
      (is (= base (core/apply-overlay base {}))))))

(def ^:private desktop-transports
  #{:hive/mcp-stdio :hive/legacy-channel :hive/ws-channel :hive/channel-bridge
    :hive/olympus :hive/a2a-gateway :hive/websocket-mcp :hive/nats
    :hive/swarm-sync :hive/registry-sync})

(deftest bare-profile-is-core-only
  (let [cfg (core/load-system-config :bare {})]
    (testing "addon discovery is off, and the extensions refs survive the overlay"
      (is (= :none (layer3/discovery-mode (:hive/extensions cfg))))
      (is (ig/ref? (get-in cfg [:hive/extensions :config]))))
    (testing "every desktop channel is gone"
      (is (empty? (filter desktop-transports (keys cfg)))))
    (testing "what is left can be driven and reloaded"
      (is (every? (set (keys cfg)) [:hive/nrepl :hive/mcp-http :hive/hot-reload :hive/keepalive]))
      (is (true? (get-in cfg [:hive/mcp-http :enabled]))))
    (testing "ports come from config.edn :services, never from the profile"
      (is (not (contains? (:hive/mcp-http cfg) :port))))
    (testing "the result is a system Integrant can order"
      (is (seq (ig/dependency-graph cfg))))))

(deftest config-overrides-edit-the-profile
  (let [cfg (core/load-system-config :bare {:hive/mcp-http nil
                                            :hive/extensions {:discover :classpath}})]
    (is (not (contains? cfg :hive/mcp-http)))
    (is (= :classpath (layer3/discovery-mode (:hive/extensions cfg))))))

(deftest extensions-discovery-mode
  (is (= :classpath (layer3/discovery-mode {})))
  (is (= :classpath (layer3/discovery-mode nil)))
  (is (= :none (layer3/discovery-mode {:discover :none}))))

(deftest bare-extensions-mount-nothing
  (let [state (ig/init-key :hive/extensions {:discover :none})]
    (is (= {:status :bare :registered 0 :total 0 :sources []} state))))
