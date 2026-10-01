(ns hive-mcp.hot.lifecycle.bare-conformance-test
  "The hive-mcp CORE, with zero addons, is reloadable.

   Live-only: the subject is a running bare host (config.edn
   {:system {:profile :bare}}, or BARE=1 bin/instance2.sh). The suite detects
   it from the running system itself, :hive/extensions reporting :bare, so a
   config-only launch qualifies with no env var. Anywhere else every test
   records an explicit skip. It mutates the host and writes one probe file under
   core's source root: never point it at the live server.

   Three claims:
     1. no addon is mounted, and the core still serves its tool table;
     2. repeated core reloads conform to the lifecycle model with no addons;
     3. an edit to a core namespace is live after `hot core-reload`, while the
        served tool table and the system map keep their identity."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.hot.core :as core-hot]
            [hive-mcp.hot.lifecycle.handlers-host :as live]
            [hive-mcp.hot.lifecycle.model :as model]
            [hive-mcp.hot.lifecycle.runner :as runner]
            [hive-mcp.server.core :as server]
            [hive-mcp.tools.consolidated.hot :as hot]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn bare-host?
  "Is this JVM running a bare hive-mcp system?"
  []
  (= :bare (get-in @server/system [:hive/extensions :status])))

(def ^:private skip "skipped: not inside a bare hive-mcp (:hive/extensions :status :bare)")

(defn- core-reload!
  "Run the operator's `hot core-reload`. Returns its report as a keyword map."
  []
  (json/read-str (:text (hot/handle-core-reload {})) :key-fn keyword))

(def scenarios
  {:core-reload-once  [{:op/kind :core-reload}]
   :core-reload-twice [{:op/kind :core-reload} {:op/kind :core-reload}]
   :core-reload-x5    (vec (repeat 5 {:op/kind :core-reload}))})

(deftest bare-host-mounts-no-addon
  (if-not (bare-host?)
    (is true skip)
    (let [spec (live/live-spec)]
      (is (empty? (:spec/addon-tools spec)) (pr-str (:spec/addon-tools spec)))
      (is (empty? (live/phases)))
      (is (seq (:spec/core-tools spec))))))

(deftest bare-core-reload-conforms-to-the-model
  (if-not (bare-host?)
    (is true skip)
    (let [spec (live/live-spec)
          host (live/handlers-host {})]
      (doseq [[k script] scenarios]
        (testing (name k)
          (let [live-trace (runner/canonical (runner/run-script host script))]
            (is (empty? (model/violations spec live-trace)))
            (is (= (runner/canonical (model/run spec script)) live-trace))))))))

(def ^:private probe-ns 'hive-mcp.hot.bare-reload-probe)

(defn- probe-file []
  (io/file (first (core-hot/core-roots)) "hive_mcp/hot/bare_reload_probe.clj"))

(defn- write-probe! [f stamp]
  (spit f (str "(ns hive-mcp.hot.bare-reload-probe)\n\n(def stamp " stamp ")\n")))

(defn- stamp [] @(ns-resolve probe-ns 'stamp))

(deftest an-edit-to-core-is-live-after-core-reload
  (if-not (bare-host?)
    (is true skip)
    (let [f      (probe-file)
          tools  (live/advertised-tools)
          system @server/system
          ctx    @server/server-context-atom]
      (is (seq (core-hot/core-roots)) "core runs from a source root, not a jar")
      (try
        (write-probe! f 1)
        (require probe-ns :reload)
        (is (= 1 (stamp)))
        (Thread/sleep 1100)
        (write-probe! f 2)
        (let [r (core-reload!)]
          (is (:ok? r) (pr-str r))
          (is (some #{(str probe-ns)} (map str (:loaded r))) (pr-str (:loaded r))))
        (is (= 2 (stamp)) "the edited core namespace is live")
        (testing "nothing else moved"
          (is (= tools (live/advertised-tools)))
          (is (identical? system @server/system))
          (is (identical? ctx @server/server-context-atom)))
        (testing "a reload with nothing changed loads nothing"
          (let [r (core-reload!)]
            (is (:ok? r))
            (is (empty? (:loaded r)) (pr-str (:loaded r)))))
        (finally
          (io/delete-file f true)
          (remove-ns probe-ns)
          (dosync (alter @#'clojure.core/*loaded-libs* disj probe-ns))
          (core-reload!))))))
