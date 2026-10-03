(ns hive-mcp.dispatch.verbs-test
  "Trifecta for the verb-level contribution seam: the pure resolution fn
   (golden + property + mutants), plus the boundary (registry, dispatch
   through make-cli-handler, schema fold)."
  (:require [clojure.test :refer [deftest testing is use-fixtures]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.dispatch.verbs :as verbs]
            [hive-mcp.tools.cli :as cli]
            [hive-mcp.tools.composite :as composite]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Handlers are keywords in the pure cases: resolve-verb never invokes them,
;; and keywords keep the golden file readable.

(defn- static-map
  [entries provided]
  (with-meta entries {::verbs/root "r" ::verbs/provided-by provided}))

(defn resolve-case
  "1-arg adapter for the trifecta: [static contributions verb] -> resolution."
  [[static contributions verb]]
  (verbs/resolve-verb static contributions verb))

(def golden-cases
  {:static-only      [(static-map {:status :core-status} {}) {} :status]
   :contributed-only [(static-map {:status :core-status} {})
                      {:kill {:handler :addon-kill :owner "hive.agent"}} :kill]
   :same-owner-shim  [(static-map {:kill :core-shim} {:kill "hive.agent"})
                      {:kill {:handler :addon-kill :owner "hive.agent"}} "kill"]
   :conflict         [(static-map {:status :core-status} {})
                      {:status {:handler :addon-status :owner "hive.rogue"}} :status]
   :declared-other   [(static-map {:kill :core-shim} {:kill "hive.agent"})
                      {:kill {:handler :addon-kill :owner "hive.rogue"}} :kill]
   :missing          [(static-map {} {:kill "hive.agent"}) {} :kill]
   :unknown-verb     [(static-map {:status :core-status} {}) {} :nope]})

;; ---- mutants ---------------------------------------------------------------

(defn- mutant-static-first
  "Consults the static map before contributions."
  [[static contributions verb]]
  (let [v (verbs/verb-key verb)]
    (if-let [s (get static v)]
      {:source :static :verb v :handler s}
      (verbs/resolve-verb static contributions verb))))

(defn- mutant-conflict-silently-wins
  "Lets any contribution shadow the core entry."
  [[static contributions verb]]
  (let [v (verbs/verb-key verb)]
    (if-let [c (get contributions v)]
      {:source :contributed :verb v :handler (:handler c) :owner (:owner c)}
      (verbs/resolve-verb static contributions verb))))

;; ---- generators -------------------------------------------------------------

(def gen-verb (gen/elements [:kill :status :spawn :nope]))
(def gen-owner (gen/elements ["hive.agent" "hive.rogue"]))

(def gen-case
  (gen/let [static-ks  (gen/set gen-verb)
            provided   (gen/map gen-verb gen-owner {:max-elements 2})
            contrib    (gen/map gen-verb
                                (gen/fmap (fn [o] {:handler (keyword (str "h-" o)) :owner o})
                                          gen-owner)
                                {:max-elements 3})
            verb       gen-verb]
    [(static-map (zipmap static-ks (map #(keyword (str "core-" (name %))) static-ks))
                 provided)
     contrib verb]))

(defn- invariant?
  "A resolution always names a source, and an admitted contribution's handler
   is the contribution's own."
  [out]
  (and (map? out)
       (#{:contributed :refused :static :missing :unknown} (:source out))))

(deftrifecta resolve-verb-contract
  hive-mcp.dispatch.verbs-test/resolve-case
  {:golden-path "test/golden/dispatch/resolve-verb.edn"
   :cases       golden-cases
   :gen         gen-case
   :pred        invariant?
   :num-tests   300
   :mutations   [["static-first" mutant-static-first]
                 ["conflict-silently-wins" mutant-conflict-silently-wins]]})

(defspec resolution-is-deterministic 300
  (prop/for-all [c gen-case]
    (= (resolve-case c) (resolve-case c))))

(defspec admitted-contribution-never-falls-through-to-static 300
  (prop/for-all [[static contribs verb :as c] gen-case]
    (let [out (resolve-case c)
          v   (verbs/verb-key verb)
          ctb (get contribs v)
          declared (get (verbs/provided-by static) v)
          admitted? (and ctb (if declared (= declared (:owner ctb)) (nil? (get static v))))]
      (if admitted?
        (and (= :contributed (:source out)) (= (:handler ctb) (:handler out)))
        (not= :contributed (:source out))))))

(defspec a-core-entry-is-never-shadowed-by-a-stranger 300
  (prop/for-all [[static contribs verb :as c] gen-case]
    (let [out (resolve-case c)
          v   (verbs/verb-key verb)]
      (if (and (contains? static v) (nil? (get (verbs/provided-by static) v)))
        (= (get static v) (:handler out))
        true))))

;; ---- boundary ---------------------------------------------------------------

(use-fixtures :each (fn [f] (verbs/clear!) (try (f) (finally (verbs/clear!)))))

(def ^:private core-tree
  (with-meta {:status (fn [_] {:type "text" :text "core-status"})}
    {::verbs/root "vt" ::verbs/provided-by {:kill "hive.agent"}}))

(deftest missing-verb-names-the-addon
  (let [h   (cli/make-cli-handler core-tree)
        out (h {:command "kill"})]
    (is (:isError out))
    (is (re-find #"hive\.agent" (:text out)))))

(deftest contributed-verb-dispatches-before-static
  (is (:ok? (verbs/contribute-verb! "vt" "kill"
                                     {:handler (fn [p] {:type "text" :text (str "killed " (:agent_id p))})
                                      :owner "hive.agent"
                                      :params {"cascade" {:type "boolean"}}})))
  (let [h (cli/make-cli-handler core-tree)]
    (is (= "killed a1" (:text (h {:command "kill" :agent_id "a1"}))))
    (is (= "core-status" (:text (h {:command "status"}))))))

(deftest nested-root-sees-contribution
  (verbs/contribute-verb! "vt" :kill {:handler (fn [_] {:type "text" :text "nested"}) :owner "hive.agent"})
  (let [h (cli/make-cli-handler {:agent core-tree})]
    (is (= "nested" (:text (h {:command "agent kill"}))))))

(deftest conflict-is-refused-and-core-keeps-dispatching
  (verbs/contribute-verb! "vt" :status {:handler (fn [_] {:type "text" :text "rogue"}) :owner "hive.rogue"})
  (let [h (cli/make-cli-handler core-tree)]
    (is (= "core-status" (:text (h {:command "status"}))))
    (is (= [:status] (mapv :verb (::verbs/refusals (meta (verbs/effective core-tree))))))))

(deftest second-owner-registration-is-refused
  (is (:ok? (verbs/contribute-verb! "vt" :kill {:handler identity :owner "hive.agent"})))
  (let [r (verbs/contribute-verb! "vt" :kill {:handler identity :owner "hive.rogue"})]
    (is (false? (:ok? r)))
    (is (= :verb/owned-by (:reason r)))))

(deftest non-invocable-handler-is-refused
  (is (= :verb/not-invocable (:reason (verbs/contribute-verb! "vt" :kill {:handler :kw :owner "o"})))))

(deftest retract-owner-withdraws-everything
  (verbs/contribute-verb! "vt" :kill {:handler identity :owner "hive.agent"})
  (verbs/contribute-verb! "vt" :kill-batch {:handler identity :owner "hive.agent"})
  (is (= 2 (count (verbs/retract-owner! "hive.agent"))))
  (is (= {} (verbs/contributions "vt"))))

(deftest contributed-params-fold-into-advertised-schema
  (verbs/contribute-verb! "vt" :kill {:handler identity :owner "hive.agent"
                                      :params {"cascade" {:type "boolean"}}})
  (let [td  {:name "vt" :consolidated true
             :inputSchema {:type "object"
                           :properties {"command" {:type "string" :enum ["status"]}}}}
        out (composite/build-merged-tool td)]
    (is (= {:type "boolean"} (get-in out [:inputSchema :properties "cascade"])))
    (is (= ["kill" "status"] (get-in out [:inputSchema :properties "command" :enum]))))
  (testing "a domain tool folds a subdomain root through :verb-roots"
    (let [out (composite/build-merged-tool
               {:name "dom" :consolidated true :verb-roots ["vt"]
                :inputSchema {:type "object" :properties {"command" {:type "string"}}}})]
      (is (= {:type "boolean"} (get-in out [:inputSchema :properties "cascade"]))))))
