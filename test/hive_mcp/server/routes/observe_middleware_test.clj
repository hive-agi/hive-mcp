;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(ns hive-mcp.server.routes.observe-middleware-test
  "Contract tests for the post-call tool observer seam (`:tool/observe`).

   The seam is SOFT: with no extension registered a call behaves exactly as it
   did before the layer existed. With one registered, every finished call is
   reported once, and the observer can neither change nor break the call."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.server.routes.middleware :as mw]))

(defn- with-observer*
  "Register `f` as the observer extension for the duration of `body-fn`."
  [f body-fn]
  (try
    (ext/register! mw/tool-observe-ext-key f)
    (body-fn)
    (finally (ext/deregister! mw/tool-observe-ext-key))))

(def ^:private ran [{:type "text" :text "ran"}])

(deftest with-no-observer-registered-the-call-is-untouched
  (ext/deregister! mw/tool-observe-ext-key)
  (let [calls (atom [])
        h (mw/wrap-handler-observe (fn [args] (swap! calls conj args) ran) "memory")]
    (is (= ran (h {:command "add"})))
    (is (= [{:command "add"}] @calls))))

(deftest a-finished-call-is-reported-once-with-its-outcome
  (let [seen (atom [])]
    (with-observer*
      (fn [observation] (swap! seen conj observation))
      (fn []
        (let [h (mw/wrap-handler-observe (fn [_] ran) "project")
              out (h {:command "kanban update" :_caller_id "coordinator:1"
                      :directory "/tmp/x"})]
          (is (= ran out) "the result is returned unchanged")
          (is (= 1 (count @seen)))
          (let [o (first @seen)]
            (is (= "project" (:tool o)))
            (is (= "kanban update" (:command o))
                "the whole command string, not only its first word")
            (is (= :ok (:outcome o)))
            (is (= ran (:result o)))
            (is (= "coordinator:1" (:caller-id o)))
            (is (integer? (:at o)))
            (is (= {:command "kanban update" :_caller_id "coordinator:1"
                    :directory "/tmp/x"}
                   (:input o)))))))))

(deftest a-call-without-a-command-reports-nil-command
  (let [seen (atom [])]
    (with-observer*
      (fn [observation] (swap! seen conj observation))
      (fn []
        ((mw/wrap-handler-observe (fn [_] ran) "bash") {:x 1})
        (is (nil? (:command (first @seen))))))))

(deftest a-throwing-handler-is-reported-and-rethrown
  (let [seen (atom [])
        boom (ex-info "boom" {:k 1})]
    (with-observer*
      (fn [observation] (swap! seen conj observation))
      (fn []
        (let [h (mw/wrap-handler-observe (fn [_] (throw boom)) "memory")]
          (is (identical? boom
                          (try (h {:command "add"}) nil
                               (catch clojure.lang.ExceptionInfo e e)))
              "the handler's own exception reaches the caller")
          (is (= 1 (count @seen)))
          (is (= :error (:outcome (first @seen))))
          (is (identical? boom (:error (first @seen)))))))))

(deftest a-throwing-observer-cannot-break-the-call
  (with-observer*
    (fn [_] (throw (ex-info "observer is broken" {})))
    (fn []
      (let [h (mw/wrap-handler-observe (fn [_] ran) "memory")]
        (is (= ran (h {:command "add"})))))))
