(ns hive-mcp.tools.consolidated.memory-var-seam-test
  "The `memory` dispatch tables hold VARS, and a reload of a handler var is seen
   through them.

   Card 20260821211613-0c0e618f counts 22 captured entries in each of
   `handlers` (22/26) and `canonical-handlers` (22/34) — a handler folded into a
   dispatch map BY VALUE froze at wiring time, so a reloaded handler var was
   reachable through the var and dead through dispatch. That is the mechanism
   behind 20260731170139-5a73833c and the habit of restarting hive-mcp to make a
   fix take effect.

   Two layers, because either alone is escapable:

     1. INVARIANT — every leaf of both tables is a var. An inline `fn` or a
        built closure in the map is equally captured, so the invariant has to be
        about the leaves, not just about the entries a census happened to flag.
        `:kg` and `:migration` are excluded by name: each is one subtree that
        `composite/lazy-resolve-handlers` re-reads per call, so its leaves are
        already late-bound individually.

     2. BEHAVIOUR — rebind a handler var and call through the table and through
        `tool-def`'s :handler. This is the assertion that would still fail if
        the invariant were satisfied by a wrapper that deref'd at build time."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.dispatch.handler :as dispatch]
            [hive-mcp.tools.consolidated.memory :as mem]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private late-bound-roots
  "Roots whose value is a whole subtree re-read per call, not a leaf."
  #{:kg :migration :plan})

(defn- leaves
  "MAP's entries without the late-bound subdomain roots."
  [m]
  (remove (comp late-bound-roots key) m))

(deftest every-handler-leaf-is-a-var
  (testing "`handlers` holds only vars"
    (is (seq mem/handlers))
    (is (empty? (remove (comp var? val) mem/handlers))
        (str "captured entries: "
             (pr-str (sort (keep (fn [[k v]] (when-not (var? v) k)) mem/handlers))))))

  (testing "`canonical-handlers` leaves hold only vars"
    (is (seq mem/canonical-handlers))
    (is (empty? (remove (comp var? val) (leaves mem/canonical-handlers)))
        (str "captured entries: "
             (pr-str (sort (keep (fn [[k v]] (when-not (var? v) k))
                                 (leaves mem/canonical-handlers)))))))

  (testing "the tool-def names its handler by var, not by value"
    (is (var? (:handler mem/tool-def)))
    (is (dispatch/handler? (:handler mem/tool-def)))
    (is (var? (:handler (first mem/tools))))))

(deftest a-reload-of-a-handler-var-is-seen-through-the-dispatch-map
  (testing "a reloaded facade handler answers through `canonical-handlers`"
    (let [v    #'hive-mcp.tools.memory/handle-mcp-memory-expire
          impl @v
          call (fn [] ((get mem/canonical-handlers :expire) {}))]
      (try
        (is (not= "sentinel" (:text (call))))
        (alter-var-root v (constantly (fn [_] {:type "text" :text "sentinel"})))
        (is (= "sentinel" (:text (call)))
            "the table follows the var; it does not hold the fn read at load time")
        (finally (alter-var-root v (constantly impl)))))))

(deftest a-reload-is-seen-through-the-tool-def-handler
  (testing "`tool-def`'s :handler routes to the reloaded handler"
    (let [v #'hive-mcp.tools.memory/handle-mcp-memory-expire
          impl @v
          call (fn [] ((:handler mem/tool-def) {:command "expire"}))]
      (try
        (alter-var-root v (constantly (fn [_] {:type "text" :text "sentinel"})))
        (is (= "sentinel" (:text (call))))
        (finally (alter-var-root v (constantly impl)))))))

(deftest the-local-read-and-batch-defs-are-vars-too
  (testing "the four read verbs are vars whose value reaches `dispatch-memory-read`"
    (let [v    #'hive-mcp.tools.consolidated.memory/read-query
          impl @v
          seen (atom nil)]
      (try
        (is (= v (get mem/canonical-handlers :query)))
        (with-redefs [mem/dispatch-memory-read (fn [event-id _] (reset! seen event-id) {:ok true})]
          ((get mem/canonical-handlers :query) {:type "note"}))
        (is (= :memory/query @seen) "the table entry calls its var, and the var keeps its routing")
        (finally (alter-var-root v (constantly impl))))))

  (testing "the constructed batch verbs are vars, so their closures are rebindable"
    (doseq [[k v] [[:batch-add      #'hive-mcp.tools.consolidated.memory/batch-add-handler]
                   [:batch-feedback #'hive-mcp.tools.consolidated.memory/batch-feedback-handler]
                   [:batch-tags     #'hive-mcp.tools.consolidated.memory/batch-tags-handler]
                   [:batch-duration #'hive-mcp.tools.consolidated.memory/batch-duration-handler]
                   [:batch-promote  #'hive-mcp.tools.consolidated.memory/batch-promote-handler]
                   [:batch-demote   #'hive-mcp.tools.consolidated.memory/batch-demote-handler]
                   [:batch          #'hive-mcp.tools.consolidated.memory/batch-handler]]]
      (is (var? (get mem/canonical-handlers k)) (str k " is a var"))
      (is (identical? (get mem/canonical-handlers k) v) (str k " is THAT var")))))

(deftest metadata-still-routes-by-id
  (testing "naming :metadata did not lose the id/no-id split"
    (let [seen (atom [])]
      (with-redefs [mem/dispatch-memory-read (fn [event-id _] (swap! seen conj event-id) {:ok true})]
        ((get mem/canonical-handlers :metadata) {:id "x"})
        ((get mem/canonical-handlers :metadata) {:type "note"}))
      (is (= [:memory/get-metadata :memory/query] @seen)))))
