(ns hive-mcp.system.layer5-stdio-surface-test
  "The stdio context is a live tool surface from boot on: :hive/mcp-stdio
   registers its :tools atom as :mcp-stdio, so a reactive refresh (evict,
   activate) reaches stdio, not only a hot-reload refresh."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.server.routes :as routes]
            [hive-mcp.system.layer5 :as layer5]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording-register
  "A register-surface! port that records what it was handed."
  [log]
  (fn [id surface] (swap! log conj [id surface]) id))

(deftest the-stdio-context-registers-its-tools-atom-as-a-surface
  (let [log   (atom [])
        tools (atom {"t" {:tool {:name "t"} :handler identity}})]
    (is (= :mcp-stdio (layer5/register-stdio-surface! (recording-register log) {:tools tools})))
    (let [[[id surface]] @log]
      (is (= :mcp-stdio id))
      (is (= :tools-atom (:surface/kind surface)))
      (is (identical? tools (:surface/tools-atom surface)) "the context's own atom, not a copy"))))

(deftest a-context-without-a-tools-atom-registers-nothing
  (let [log (atom [])]
    (is (nil? (layer5/register-stdio-surface! (recording-register log) {})))
    (is (empty? @log))))

(deftest the-registered-surface-receives-the-installed-table
  (testing "the surface it registers is one routes' :tools-atom method installs into"
    (let [tools (atom {})
          table {"t" {:tool {:name "t"} :handler identity}}
          sink  (atom nil)]
      (layer5/register-stdio-surface! (fn [_ s] (reset! sink s)) {:tools tools})
      (is (true? (routes/install-table! @sink table)))
      (is (= table @tools)))))
