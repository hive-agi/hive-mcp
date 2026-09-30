(ns hive-mcp.tools.agent.reconcile-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.agent.helpers :as helpers]
            [hive-mcp.tools.agent.reconcile :as reconcile]))

(defn- recording-probe
  "Stub ILivenessEvidence answering from `answers`, recording every id asked."
  [answers]
  (let [asked (atom [])]
    {:asked asked
     :probe (reify reconcile/ILivenessEvidence
              (evidence [_ agent-id]
                (swap! asked conj agent-id)
                (get answers agent-id :none)))}))

(defn- orphan [name]
  {:slave/id (str "swarm-" name "-orphan") :slave/name name :slave/status :orphan :slave/depth 1})

(def ^:private live-ds-row
  {:slave/id "td-precompact" :slave/status :working :slave/depth 1})

(deftest phantom-name-prefers-name-then-unwrapped-id
  (is (= "td-grid-hint" (reconcile/phantom-name (orphan "td-grid-hint"))))
  (is (= "td-grid-hint" (reconcile/phantom-name {:slave/id "swarm-td-grid-hint-orphan"})))
  (is (= "plain" (reconcile/phantom-name {:slave/id "plain"}))))

(deftest phantom-of-a-registered-agent-is-dropped
  (let [{:keys [probe asked]} (recording-probe {"td-precompact" :registered})]
    (is (nil? (reconcile/reconcile-row probe (orphan "td-precompact"))))
    (is (= "td-precompact" (first @asked)))))

(deftest orphan-the-hivemind-has-heard-is-unregistered-not-orphan
  (let [{:keys [probe]} (recording-probe {"td-grid-hint" :heard})]
    (is (= :unregistered (:slave/status (reconcile/reconcile-row probe (orphan "td-grid-hint")))))))

(deftest orphan-with-no-evidence-stays-orphan
  (let [{:keys [probe]} (recording-probe {})]
    (is (= :orphan (:slave/status (reconcile/reconcile-row probe (orphan "gone")))))))

(deftest non-orphan-rows-are-never-probed
  (let [{:keys [probe asked]} (recording-probe {})
        row {:slave/id "x" :slave/status :idle}]
    (is (= row (reconcile/reconcile-row probe row)))
    (is (empty? @asked))))

(deftest dead-orphans-hidden-unless-stale-requested
  (let [{:keys [probe]} (recording-probe {"alive" :heard})
        rows [(orphan "gone") (orphan "alive") {:slave/id "reg" :slave/status :idle}]]
    (is (= ["swarm-alive-orphan" "reg"]
           (mapv :slave/id (reconcile/reconcile probe rows {}))))
    (is (= ["swarm-gone-orphan" "swarm-alive-orphan" "reg"]
           (mapv :slave/id (reconcile/reconcile probe rows {:include-stale? true}))))))

(deftest known-agents-adapter-ranks-registry-over-hivemind
  (let [probe (reconcile/->known-agents ["a"] ["a" "b"])]
    (is (= :registered (reconcile/evidence probe "a")))
    (is (= :heard (reconcile/evidence probe "b")))
    (is (= :none (reconcile/evidence probe "c")))))

(deftest status-merge-dedupes-live-ling-with-its-phantom
  (testing "the card's case: live DS row plus elisp phantom yields one row"
    (let [probe (reconcile/->known-agents ["td-precompact"] [])
          merged (helpers/merge-with-elisp-lings
                  [live-ds-row]
                  {:probe probe
                   :elisp-lings [(orphan "td-precompact") (orphan "stale-other-session")]})]
      (is (= ["td-precompact"] (mapv :slave/id merged))))))

(deftest status-merge-shows-dead-orphans-for-diagnostics
  (let [probe (reconcile/->known-agents ["td-precompact"] [])
        merged (helpers/merge-with-elisp-lings
                [live-ds-row]
                {:probe probe
                 :include-stale? true
                 :elisp-lings [(orphan "td-precompact") (orphan "stale-other-session")]})]
    (is (= ["td-precompact" "swarm-stale-other-session-orphan"] (mapv :slave/id merged)))
    (is (= :orphan (:slave/status (second merged))))))
