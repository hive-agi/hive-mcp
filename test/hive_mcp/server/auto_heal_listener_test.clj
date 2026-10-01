(ns hive-mcp.server.auto-heal-listener-test
  "The :mcp-auto-heal listener is registered keyed and reaches its body and
   the server context THROUGH VARS, so neither a reload of server.init nor a
   context wired after boot leaves it stale.

   The registration port is injected (a recording add-listener!); hive-hot's
   real listener table is never touched."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.server.core]
            [hive-mcp.server.init :as init]
            [hive-mcp.hot.reseat :as reseat]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- registered-listener
  "Run register-hot-reload-listener! against a recording port. Answers the
   registrations it made: [[id f] ...]."
  []
  (let [calls (atom [])]
    (init/register-hot-reload-listener! (fn [id f] (swap! calls conj [id f])))
    @calls))

(deftest the-listener-is-registered-keyed-and-idempotent
  (let [first-run  (registered-listener)
        second-run (registered-listener)]
    (is (= [:mcp-auto-heal] (map first first-run)) "one keyed registration")
    (is (= (map first first-run) (map first second-run))
        "a re-run registers the same key, which REPLACES the entry in hive-hot")))

(deftest the-listener-reaches-its-body-through-the-var
  (let [[[_ listener]] (registered-listener)]
    (is (not (identical? listener @#'init/on-hot-reload-event!))
        "the fn value of the body is not what is registered")
    (is (nil? (listener {:type :reload-start})) "only :reload-success heals")
    (testing "the body runs through the var: rebinding it is seen by the SAME listener"
      (let [orig @#'init/on-hot-reload-event!
            seen (atom nil)]
        (try
          (alter-var-root #'init/on-hot-reload-event! (constantly (fn [e] (reset! seen e) :rebound)))
          (is (= :rebound (listener {:type :reload-success})))
          (is (= {:type :reload-success} @seen))
          (finally (alter-var-root #'init/on-hot-reload-event! (constantly orig))))))))

(deftest the-listener-sees-the-real-server-context
  (testing "the context atom is server.core's, read through its var"
    (is (identical? @(resolve 'hive-mcp.server.core/server-context-atom)
                    (init/server-context-atom))))
  (testing "a context wired AFTER registration is the one the event refreshes"
    (let [ctx-atom (init/server-context-atom)
          before   @ctx-atom
          tools    (atom {"stale" {:tool {:name "stale"} :handler (constantly nil)}})
          [[_ listener]] (registered-listener)]
      (try
        (reset! ctx-atom {:tools tools})
        (listener {:type :reload-success :loaded [] :unloaded [] :ms 1})
        (is (not (contains? @tools "stale"))
            "refresh-tools! rebuilt the table of the context current at the event")
        (is (seq @tools))
        (finally (reset! ctx-atom before))))))

(deftest a-watcher-reload-re-seats-the-loaded-namespaces
  (let [k      'hive-mcp.server.auto-heal-listener-test.seat
        seated (atom [])
        [[_ listener]] (registered-listener)]
    (try
      (reseat/register-reseater! k (fn [loaded] (swap! seated conj loaded) :ok))
      (listener {:type :reload-success :loaded ['hive-mcp.other] :unloaded [] :ms 0})
      (is (= [] @seated) "a namespace that was not loaded is not re-seated")
      (listener {:type :reload-success :loaded [k 'hive-mcp.other] :unloaded [] :ms 0})
      (is (= [[(str k) "hive-mcp.other"]] @seated)
          "the watcher path re-seats too, not only hive-mcp.hot.core/reload!")
      (finally (reseat/unregister-reseater! k)))))
