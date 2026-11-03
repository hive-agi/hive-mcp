(ns hive-mcp.dispatch.branches-test
  "Runtime branches, end to end through the same dispatcher a tool call reaches:
   composite/build-merged-handler over make-cli-handler. Stubs are plain fns."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-addon.registry.commands :as acmds]
            [hive-mcp.dispatch.branches :as branches]
            [hive-mcp.dispatch.verbs :as verbs]
            [hive-mcp.tools.cli :as cli]
            [hive-mcp.tools.composite :as composite]
            [clojure.walk :as walk]
            [hive-mcp.extensions.reactive :as reactive]
            [hive-mcp.extensions.registry :as ext]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tool "branch-test-tool")

(use-fixtures :each
  (fn [t]
    (branches/clear!)
    (verbs/clear!)
    (acmds/retract-all! :addon-a)
    (acmds/retract-all! :addon-b)
    (try (t)
         (finally
           (branches/clear!)
           (verbs/clear!)
           (acmds/retract-all! :addon-a)
           (acmds/retract-all! :addon-b)))))

(defn- answer [s] (fn [_] {:type "text" :text s}))

(def ^:private core-tree
  {:status (answer "core status")
   :belt   {:scan (answer "core belt scan")}})

(defn- call
  "The text a tool call of COMMAND answers, on TOOL with core-tree."
  ([command] (call command {}))
  ([command params]
   (:text ((composite/build-merged-handler tool core-tree)
           (assoc params :command command)))))

(defn- opaque
  "A leaf that routes its own remainder, as a subdomain owner does."
  [label]
  (fn [{:keys [command]}] {:type "text" :text (str label " got: " command)}))

;; =============================================================================
;; graft
;; =============================================================================

(deftest graft-adds-a-verb-beside-what-a-subtree-routes
  (is (:ok? (branches/graft! tool "belt hello" {:handler (answer "hi") :owner "x"})))
  (is (= "hi" (call "belt hello")))
  (is (= "core belt scan" (call "belt scan")) "the siblings keep routing"))

(deftest graft-creates-the-levels-it-needs
  (branches/graft! tool "a b c" {:handler (answer "deep") :owner "x"})
  (is (= "deep" (call "a b c"))))

(deftest graft-below-an-opaque-leaf-splits-it-and-the-leaf-keeps-its-own-verbs
  (acmds/contribute! tool :addon-a {"sub" {:handler (opaque "sub")}})
  (branches/graft! tool "sub extra" {:handler (answer "grafted") :owner "b"})
  (testing "the graft routes"
    (is (= "grafted" (call "sub extra"))))
  (testing "the leaf gets its bare route and every other verb, with the whole command"
    (is (= "sub got: sub" (call "sub")))
    (is (= "sub got: sub own verb" (call "sub own verb")))))

(deftest a-deep-graft-below-an-opaque-leaf-leaves-the-leafs-other-routes-alone
  (acmds/contribute! tool :addon-a {"sub" {:handler (opaque "sub")}})
  (branches/graft! tool "sub a b" {:handler (answer "grafted") :owner "b"})
  (is (= "grafted" (call "sub a b")))
  (is (= "sub got: sub a c" (call "sub a c")) "a sibling below the new level")
  (is (= "sub got: sub a" (call "sub a")) "the new level itself")
  (is (= "sub got: sub d" (call "sub d"))))

(deftest one-addon-extends-another-addons-subtree-and-survives-its-remount
  (acmds/contribute! tool :addon-a {"sub" {:handler {:one (answer "a one")}}})
  (branches/graft! tool "sub two" {:handler (answer "b two") :owner "addon-b"})
  (is (= "a one" (call "sub one")))
  (is (= "b two" (call "sub two")))
  (testing "A remounts"
    (acmds/retract-all! :addon-a)
    (acmds/contribute! tool :addon-a {"sub" {:handler {:one (answer "a one v2")}}})
    (is (= "a one v2" (call "sub one")))
    (is (= "b two" (call "sub two")) "B's branch was not touched")))

(deftest a-verb-map-grafts-leaf-by-leaf
  (branches/graft! tool "belt" {:handler {:hello (answer "hello")
                                          :bye   (answer "bye")}
                                :owner "x"})
  (is (= "hello" (call "belt hello")))
  (is (= "bye" (call "belt bye")))
  (is (= "core belt scan" (call "belt scan"))))

(deftest graft-never-shadows-a-route-and-says-so
  (branches/graft! tool "belt scan" {:handler (answer "hijack") :owner "x"})
  (is (= "core belt scan" (call "belt scan")))
  (is (= [{:kind :graft :path [:belt :scan] :owner "x" :reason :branch/occupied}]
         (branches/refusals tool))))

(deftest graft-refuses-a-verb-an-opaque-root-declares
  (acmds/contribute! tool :addon-a {"sub" {:handler (opaque "sub") :verbs ["own"]}})
  (branches/graft! tool "sub own" {:handler (answer "hijack") :owner "x"})
  (is (= "sub got: sub own" (call "sub own")))
  (is (= :branch/shadows-verb (:reason (first (branches/refusals tool))))))

(deftest grafts-at-a-parent-and-its-child-compose-in-either-order
  (doseq [order [[["n" (answer "parent")] ["n m" (answer "child")]]
                 [["n m" (answer "child")] ["n" (answer "parent")]]]]
    (branches/clear!)
    (doseq [[p h] order] (branches/graft! tool p {:handler h :owner "x"}))
    (is (= "parent" (call "n")))
    (is (= "child" (call "n m")))))

(deftest registration-refuses-what-cannot-work
  (testing "no path"
    (is (= :branch/empty-path (:reason (branches/graft! tool "" {:handler (answer "x") :owner "o"})))))
  (testing "not invocable"
    (is (= :branch/not-invocable (:reason (branches/graft! tool "p" {:handler :kw :owner "o"})))))
  (testing "no owner"
    (is (= :branch/no-owner (:reason (branches/graft! tool "p" {:handler (answer "x")})))))
  (testing "another owner's leaf"
    (branches/graft! tool "p" {:handler (answer "first") :owner "o1"})
    (let [r (branches/graft! tool "p" {:handler (answer "second") :owner "o2"})]
      (is (= :branch/owned-by (:reason r)))
      (is (= "o1" (:owner r))))
    (is (= "first" (call "p"))))
  (testing "the same owner replaces"
    (is (:ok? (branches/graft! tool "p" {:handler (answer "newer") :owner "o1"})))
    (is (= "newer" (call "p")))))

;; =============================================================================
;; around
;; =============================================================================

(defn- suffixing [s]
  (fn [next-handler]
    (fn [params] (update (next-handler params) :text str s))))

(deftest around-wraps-one-verb
  (branches/around! tool "status" {:wrap (suffixing "!") :owner "x"})
  (is (= "core status!" (call "status")))
  (is (= "core belt scan" (call "belt scan")) "nothing else changes"))

(deftest around-a-subtree-covers-every-leaf-below-it
  (branches/graft! tool "belt hello" {:handler (answer "hi") :owner "y"})
  (branches/around! tool "belt" {:wrap (suffixing "!") :owner "x"})
  (is (= "core belt scan!" (call "belt scan")))
  (is (= "hi!" (call "belt hello")) "a graft is wrapped like anything else"))

(deftest around-the-empty-path-covers-the-whole-tool
  (branches/around! tool "" {:wrap (suffixing "!") :owner "x"})
  (is (= "core status!" (call "status")))
  (is (= "core belt scan!" (call "belt scan"))))

(deftest around-reaches-a-verb-only-an-opaque-leaf-knows
  (acmds/contribute! tool :addon-a {"sub" {:handler (opaque "sub")}})
  (branches/around! tool "sub verb" {:wrap (suffixing "!") :owner "x"})
  (is (= "sub got: sub verb!" (call "sub verb")))
  (is (= "sub got: sub other" (call "sub other"))))

(deftest around-reaches-a-verb-the-verb-seam-contributed
  (let [root (with-meta {:kill (answer "core shim")}
               {::verbs/root "r" ::verbs/provided-by {:kill "owner"}})
        h    (composite/build-merged-handler tool {:r root})]
    (verbs/contribute-verb! "r" :kill {:handler (answer "contributed") :owner "owner"})
    (branches/around! tool "r kill" {:wrap (suffixing "!") :owner "x"})
    (is (= "contributed!" (:text (h {:command "r kill"}))))))

(deftest around-can-replace-by-not-calling-next
  (branches/around! tool "status" {:wrap (fn [_] (answer "replaced")) :owner "x"})
  (is (= "replaced" (call "status"))))

(deftest lowest-order-runs-outermost-whatever-arrived-first
  (branches/around! tool "status" {:wrap (suffixing "-inner") :owner "a" :order 10})
  (branches/around! tool "status" {:wrap (suffixing "-outer") :owner "b" :order 1})
  (is (= "core status-inner-outer" (call "status"))))

(deftest around-with-nothing-to-wrap-is-refused
  (branches/around! tool "nowhere" {:wrap (suffixing "!") :owner "x"})
  (is (= "core status" (call "status")))
  (is (= :branch/no-target (:reason (first (branches/refusals tool))))))

(deftest hot-branch-cannot-be-wrapped-shut
  (doseq [path ["" "branch" "branch remove"]]
    (is (= :branch/self-locking
           (:reason (branches/around! "hot" path {:wrap (suffixing "!") :owner "x"})))))
  (is (:ok? (branches/around! "hot" "status" {:wrap (suffixing "!") :owner "x"}))))

;; =============================================================================
;; A broken branch stays contained
;; =============================================================================

(deftest a-branch-that-throws-names-itself-and-its-removal
  (branches/graft! tool "boom" {:handler (fn [_] (throw (ex-info "kaput" {}))) :owner "x"})
  (let [r ((composite/build-merged-handler tool core-tree) {:command "boom"})]
    (is (:isError r))
    (is (str/includes? (:text r) "kaput"))
    (is (str/includes? (:text r) "branch remove")))
  (is (= "core status" (call "status"))))

;; =============================================================================
;; Hot: nothing is captured
;; =============================================================================

(defn- reloaded-handler [_] {:type "text" :text "before"})

(deftest a-var-registered-branch-sees-a-reload
  (branches/graft! tool "v" {:handler #'reloaded-handler :owner "x"})
  (is (= "before" (call "v")))
  (let [original reloaded-handler]
    (try
      (alter-var-root #'reloaded-handler (constantly (fn [_] {:type "text" :text "after"})))
      (is (= "after" (call "v")))
      (finally (alter-var-root #'reloaded-handler (constantly original))))))

(deftest changes-reach-a-handler-built-before-them
  (let [built (composite/build-merged-handler tool core-tree)]
    (branches/graft! tool "late" {:handler (answer "late") :owner "x"})
    (is (= "late" (:text (built {:command "late"}))))
    (branches/prune! tool "late" "x")
    (is (:isError (built {:command "late"})))))

(deftest retracting-an-owner-withdraws-everything-it-registered
  (branches/graft! tool "a" {:handler (answer "a") :owner :my/addon})
  (branches/around! "other-tool" "b" {:wrap (suffixing "!") :owner "my/addon"})
  (is (= 2 (branches/retract-owner! :my/addon)))
  (is (empty? (branches/of tool)))
  (is (empty? (branches/of "other-tool"))))

(deftest addon-shutdown-takes-its-branches-with-it
  (branches/graft! tool "a" {:handler (answer "a") :owner "addon-a"})
  (branches/graft! tool "b" {:handler (answer "b") :owner "addon-b"})
  (ext/retract-all-by-addon! :addon-a)
  (is (= ["addon-b"] (map :owner (branches/of tool)))))

(deftest listeners-hear-every-change-and-a-throwing-one-is-ignored
  (let [seen (atom [])]
    (branches/add-listener! ::record #(swap! seen conj (:type %)))
    (branches/add-listener! ::broken (fn [_] (throw (ex-info "no" {}))))
    (try
      (branches/graft! tool "a" {:handler (answer "a") :owner "x"})
      (branches/prune! tool "a" "x")
      (is (= [:branch :prune] @seen))
      (finally
        (branches/remove-listener! ::record)
        (branches/remove-listener! ::broken)))))

;; =============================================================================
;; Advertised surface
;; =============================================================================

(def ^:private core-tool-def
  {:name tool
   :description "Core."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string" :enum ["status" "belt scan"]}}}})

(deftest a-branch-reaches-the-advertised-schema
  (branches/graft! tool "belt hello" {:handler {:_handler (answer "x") :a (answer "y")}
                                      :owner "x"
                                      :summary "Says hello"
                                      :params {"name" {:type "string"}}})
  (branches/around! tool "status" {:wrap (suffixing "!") :owner "x" :summary "Loud"})
  (let [merged (composite/build-merged-tool core-tool-def)
        enum   (set (get-in merged [:inputSchema :properties "command" :enum]))]
    (is (contains? enum "belt hello"))
    (is (contains? enum "belt hello a"))
    (is (not-any? #(str/includes? % "_handler") enum))
    (is (= {:type "string"} (get-in merged [:inputSchema :properties "name"])))
    (is (str/includes? (:description merged) "belt hello"))
    (is (str/includes? (:description merged) "Says hello"))
    (is (str/includes? (:description merged) "[around] Loud"))))

(deftest a-branch-change-refreshes-the-live-surface
  (let [refreshes (atom 0)]
    (binding [reactive/*server-refresh* (fn []
                                          (swap! refreshes inc)
                                          {:surfaces [:test]})]
      (reactive/install!)
      (try
        (branches/graft! tool "a" {:handler (answer "a") :owner "x"})
        (is (= 1 @refreshes) "a graft")
        (branches/prune! tool "a" "x")
        (is (= 2 @refreshes) "a prune")
        (finally (reactive/uninstall!))))))

(deftest no-branches-leaves-a-tool-def-alone
  (is (= core-tool-def (composite/build-merged-tool core-tool-def))))

(deftest no-branches-leaves-the-tree-alone
  (is (= (set (keys core-tree))
         (set (keys (composite/effective-handlers tool core-tree))))))

;; =============================================================================
;; Properties
;; =============================================================================

(def ^:private gen-seg (gen/elements [:a :b :c :d]))
(def ^:private gen-path (gen/vector gen-seg 1 4))

(defn- leaf-paths
  "Every route in TREE, as a path to its leaf."
  [tree]
  (mapv first (branches/flatten-graft [] tree)))

(defspec folding-never-loses-a-route 200
  (prop/for-all [existing (gen/vector gen-path 0 6)
                 grafts   (gen/vector gen-path 0 6)]
    (let [tree   (reduce (fn [t p]
                           (if (try (get-in t p) (catch Exception _ nil))
                             t
                             (try (assoc-in t p :leaf-marker) (catch Exception _ t))))
                         {} existing)
          tree   (walk/postwalk #(if (= :leaf-marker %) (answer "core") %) tree)
          before (leaf-paths tree)
          folded (branches/fold tree
                                (for [[i p] (map-indexed vector (distinct grafts))]
                                  {:kind :graft :path p :owner (str "o" i) :handler (answer "g")})
                                tool {})]
      (every? (fn [p]
                (let [{:keys [handler]} (cli/resolve-handler folded (vec (remove #{:_handler} p)))]
                  (= "core" (:text (handler {})))))
              before))))

(defspec every-unrefused-graft-routes 200
  (prop/for-all [grafts (gen/vector gen-path 1 6)]
    (let [gs       (for [[i p] (map-indexed vector (distinct grafts))]
                     {:kind :graft :path p :owner (str "o" i) :handler (answer (str i))})
          folded   (branches/fold {} gs tool {})
          refused  (set (map :path (::branches/refusals (meta folded))))]
      (every? (fn [{:keys [path handler]}]
                (or (contains? refused path)
                    (let [h (:handler (cli/resolve-handler folded path))]
                      (= (:text (handler {})) (:text (h {}))))))
              gs))))
