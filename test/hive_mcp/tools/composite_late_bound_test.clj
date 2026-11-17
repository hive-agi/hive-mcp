(ns hive-mcp.tools.composite-late-bound-test
  "A root's lazily resolved handlers follow a reload of the subdomain they name.

   Friction 20261110235404-7305c23d: `project` merged the kanban consolidator's
   handlers through lazy-resolve-handlers, which derefed them ONCE at load, so a
   hot core-reload of the kanban namespaces stayed invisible through `project`."
  (:require [clojure.string]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.composite :as composite]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private sub 'hive-mcp.tools.composite-late-bound-test.sub)

(def ^:private sym (symbol (str sub) "handlers"))

(defn- load-sub!
  "Recreate the subdomain namespace, as a reload does: a new namespace object
   holding a new `handlers` var."
  [handlers]
  (remove-ns sub)
  (create-ns sub)
  (intern sub 'handlers handlers))

(defn- answers [v]
  {:list   (fn [_] v)
   :two    (fn [a b] [v a b])
   :nested {:go (fn [_] v)}})

(deftest leaves-follow-a-reload
  (try
    (load-sub! (answers :v1))
    (let [tree (composite/lazy-resolve-handlers sym)]
      (testing "before the reload"
        (is (= :v1 ((:list tree) {})))
        (is (= :v1 ((get-in tree [:nested :go]) {}))))
      (load-sub! (answers :v2))
      (testing "after the reload the same tree reaches the new code"
        (is (= :v2 ((:list tree) {})))
        (is (= :v2 ((get-in tree [:nested :go]) {})))
        (is (= [:v2 1 2] ((:two tree) 1 2))))
      (testing "a verb the reload removed answers from the handler read at load time"
        (load-sub! {})
        (is (= :v1 ((:list tree) {}))))
      (testing "a namespace that cannot be resolved does the same"
        (remove-ns sub)
        (is (= :v1 ((:list tree) {})))))
    (finally (remove-ns sub))))

(defn- text-handlers [v]
  {:list (fn [_] {:type "text" :text v})})

(deftest a-stale-fallback-is-marked-never-silent
  (try
    (load-sub! (text-handlers "v1"))
    (let [tree (composite/lazy-resolve-handlers sym)]
      (is (nil? (:stale-handler ((:list tree) {}))) "a live handler is not marked")
      (doseq [[label reload! reason]
              [["namespace unloaded (mid-reload or a failed reload)" #(remove-ns sub) "ns-not-loaded"]
               ["var gone" #(do (remove-ns sub) (create-ns sub)) "var-missing"]
               ["verb removed" #(load-sub! {}) "verb-missing"]]]
        (testing label
          (reload!)
          (let [resp ((:list tree) {})]
            (is (= reason (get-in resp [:stale-handler :reason])))
            (is (= ["list"] (get-in resp [:stale-handler :path])))
            (is (clojure.string/starts-with? (:text resp) "v1"))
            (is (clojure.string/includes? (:text resp) "[stale handler]")))))
      (testing "a recovered leaf is live and unmarked again"
        (load-sub! (text-handlers "v2"))
        (let [resp ((:list tree) {})]
          (is (= "v2" (:text resp)))
          (is (nil? (:stale-handler resp))))))
    (finally (remove-ns sub))))

(def ^:private never-loaded
  "A namespace with a source file on the classpath under test/, never loaded
   by this suite: proves the call path does not `require`."
  'hive-mcp.tools.composite-late-bound-test.never-loaded)

(defn- unload!
  "Unload NS-SYM the way clj-reload's unload pass does."
  [ns-sym]
  (remove-ns ns-sym)
  (dosync (alter @#'clojure.core/*loaded-libs* disj ns-sym)))

(deftest the-call-path-never-loads-a-namespace
  (let [nl-sym (symbol (str never-loaded) "handlers")]
    (try
      (testing "loaded-var reads only loaded namespaces"
        (unload! never-loaded)
        (is (nil? (composite/loaded-var nl-sym)))
        (is (nil? (find-ns never-loaded)) "loaded-var did not load it"))
      (testing "current-at on an unloaded namespace reports it, without loading it"
        (is (= {:stale :ns-not-loaded} (composite/current-at nl-sym [:list])))
        (is (nil? (find-ns never-loaded))))
      (testing "a late-bound leaf called mid-reload does not load the namespace"
        (let [tree (composite/lazy-resolve-handlers nl-sym)] ; root load: may load it
          (is (some? (find-ns never-loaded)))
          (unload! never-loaded)
          (let [resp ((:list tree) {})]
            (is (nil? (find-ns never-loaded)) "the request thread did not load the namespace")
            (is (= "ns-not-loaded" (get-in resp [:stale-handler :reason]))))))
      (finally (unload! never-loaded)))))

(deftest subtree-metadata-survives
  (try
    (load-sub! {:agent (with-meta {:kill (fn [_] :k)} {:hive-mcp.dispatch.verbs/root "agent" ::tag 1})})
    (let [tree (composite/lazy-resolve-handlers sym)]
      (is (= "agent" (:hive-mcp.dispatch.verbs/root (meta (:agent tree)))))
      (is (= 1 (::tag (meta (:agent tree))))))
    (finally (remove-ns sub))))

(deftest the-swarm-root-routes-a-contributed-agent-verb
  ;; Review H1: late-bound dropped map metadata, so ::verbs/root "agent" was lost
  ;; under `swarm` and an addon-contributed `agent kill` became Unknown command.
  ;; Stubs only: the contributed handler is a local fn, nothing live is touched.
  (let [verbs-ns   'hive-mcp.dispatch.verbs
        _          (require verbs-ns 'hive-mcp.tools.consolidated.swarm)
        contribute (ns-resolve verbs-ns 'contribute-verb!)
        retract    (ns-resolve verbs-ns 'retract-verb!)
        root-of    (ns-resolve verbs-ns 'root-of)
        swarm      @(ns-resolve 'hive-mcp.tools.consolidated.swarm 'handle-swarm)
        owner      "hive.agent"
        prior      (get ((ns-resolve verbs-ns 'contributions) "agent") :kill)]
    (is (= "agent" (root-of (composite/lazy-resolve-handlers 'hive-mcp.tools.consolidated.agent/handlers))))
    (if prior
      (is false "a live :kill contribution is registered; this suite must run on a clean registry")
      (try
        (testing "without the addon, kill names its owner instead of a generic unknown command"
          (is (re-find #"hive\.agent" (str (:text (swarm {:command "agent kill"}))))))
        (testing "the contribution routes through the real swarm root"
          (contribute "agent" :kill {:handler (fn [_] {:type "text" :text "contributed-kill"})
                                     :owner owner})
          (is (= "contributed-kill" (:text (swarm {:command "agent kill"})))))
        (finally (retract "agent" :kill owner))))))

(def ^:private root-handlers
  "A domain root shaped like hive-mcp.tools.consolidated.project/canonical-handlers:
   its own verbs merged with a subdomain folded in through lazy-resolve-handlers
   at THIS namespace's load."
  nil)

(deftest a-domain-root-routes-a-reloaded-subdomain-to-the-new-code
  (try
    (load-sub! {:list (fn [_] {:type "text" :text "v1"})})
    (alter-var-root #'root-handlers
                    (constantly {:info (fn [_] {:type "text" :text "root"})
                                 :kanban (composite/lazy-resolve-handlers sym)}))
    (let [root (composite/build-merged-handler "late-bound-test-root" #'root-handlers)]
      (is (= "v1" (:text (root {:command "kanban list"}))))
      ;; The reload: the subdomain namespace is recreated with new code, and the
      ;; root namespace (here #'root-handlers) is NOT reloaded with it.
      (load-sub! {:list (fn [_] {:type "text" :text "v2"})})
      (is (= "v2" (:text (root {:command "kanban list"})))
          "the root reaches the reloaded subdomain without being reloaded itself")
      (is (= "root" (:text (root {:command "info"})))))
    (finally (remove-ns sub))))

(deftest the-tree-keeps-its-shape
  (try
    (load-sub! (answers :v1))
    (let [tree (composite/lazy-resolve-handlers sym)]
      (is (= #{:list :two :nested} (set (keys tree))))
      (is (= #{:go} (set (keys (:nested tree)))))
      (is (fn? (:list tree))))
    (finally (remove-ns sub))))

(deftest a-leaf-keeps-the-metadata-of-its-var
  (try
    (load-sub! {:list #'answers})
    (is (= 'answers (:name (meta (:list (composite/lazy-resolve-handlers sym))))))
    (finally (remove-ns sub))))

(deftest an-unresolvable-symbol-is-an-empty-tree
  (is (= {} (composite/lazy-resolve-handlers 'no.such.namespace/handlers))))
