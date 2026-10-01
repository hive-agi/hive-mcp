(ns hive-mcp.channel.piggyback.sources-test
  "The piggyback source registry: core reads with zero, one or many sources,
   a failing source is rescued on its own, and a source registered by var is
   re-read on every call (Capture-by-Var).

   Every collaborator is a stub behind IPiggybackSource: fns, vars, atoms,
   reify and a record, plus a RECORDING decorator (which sources were read)
   and a FAULT-INJECTING decorator (a source that throws). No with-redefs."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.properties :as prop]
            [malli.core :as m]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.channel.piggyback :as pb]
            [hive-mcp.channel.piggyback.sources :as sources]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Stubs and decorators — all behind the port
;; =============================================================================

(defn- row [agent-id ts & {:as extra}]
  (merge {:agent-id agent-id :event-type :progress :message (str agent-id "@" ts)
          :timestamp ts :project-id "p" :parent-id "coordinator"}
         extra))

(defrecord FixedSource [rows]
  sources/IPiggybackSource
  (-messages [_] rows))

(defn- failing
  "Fault-injecting source: every read throws."
  [msg]
  (reify sources/IPiggybackSource
    (-messages [_] (throw (ex-info msg {:injected true})))))

(defn- recording
  "Recording decorator: reads through to SOURCE and logs ID into LOG."
  [log id source]
  (reify sources/IPiggybackSource
    (-messages [_]
      (swap! log conj id)
      (sources/-messages source))))

;; A source reached THROUGH its var. The capture-by-var test rebinds its root
;; (what a reload does) and restores it; nothing here redefines a concretion.
(defn reloadable-source [] [(row "before-reload" 1)])

;; =============================================================================
;; Fixture — isolate the process-wide default slot and any test registrations
;; =============================================================================

(def ^:private test-ids [::extra ::broken ::by-var ::by-value])

(use-fixtures :each
  (fn [f]
    (let [slot @pb/message-source-fn]
      (pb/reset-all-cursors!)
      (pb/clear-backbone-buffer!)
      (try (f)
           (finally
             (run! sources/deregister! test-ids)
             (pb/register-message-source! slot)
             (pb/reset-all-cursors!)
             (pb/clear-backbone-buffer!))))))

;; =============================================================================
;; Promote — fold-outcomes (golden + property + mutation)
;; =============================================================================

(def ^:private ok-a {:source-id :s/a :result {:ok [{:agent-id "a" :timestamp 1}]}})
(def ^:private ok-b {:source-id :s/b :result {:ok [{:agent-id "b" :timestamp 2}
                                                   {:agent-id "b" :timestamp 3}]}})
(def ^:private err-c {:source-id :s/c :result {:error :piggyback.source/failed
                                              :message "boom"}})

(def ^:private gen-outcome
  (gen/let [id (gen/elements [:s/a :s/b :s/c :s/d])
            ok? gen/boolean
            rows (gen/vector (gen/let [a (gen/elements ["a" "b" "c"])
                                       ts gen/nat]
                               {:agent-id a :timestamp ts})
                             0 4)]
    {:source-id id
     :result (if ok? {:ok rows} {:error :piggyback.source/failed :message "x"})}))

(deftrifecta fold-outcomes-contract
  hive-mcp.channel.piggyback.sources/fold-outcomes
  {:golden-path "test/golden/channel/piggyback-sources-fold.edn"
   :cases       {:zero-sources   []
                 :one-healthy    [ok-a]
                 :two-healthy    [ok-a ok-b]
                 :one-failing    [err-c]
                 :failing-middle [ok-a err-c ok-b]}
   :gen         (gen/vector gen-outcome 0 6)
   :pred        #(m/validate sources/Collected %)
   :num-tests   200
   :mutations   [["drops failures — a broken source vanishes silently"
                  (fn [os] {:messages (vec (mapcat #(get-in % [:result :ok]) os))
                            :failures []})]
                 ["one failure sinks the read"
                  (fn [os] (if (some #(contains? (:result %) :error) os)
                             {:messages [] :failures []}
                             {:messages (vec (mapcat #(get-in % [:result :ok]) os))
                              :failures []}))]
                 ["first source only"
                  (fn [os] {:messages (vec (get-in (first os) [:result :ok]))
                            :failures []})]]
   :assert      (fn []
                  (is (= {:messages [] :failures []} (sources/fold-outcomes []))
                      "zero sources is an empty read, not an error")
                  (let [{:keys [messages failures]} (sources/fold-outcomes [ok-a err-c ok-b])]
                    (is (= ["a" "b" "b"] (mapv :agent-id messages))
                        "healthy sources on both sides of a failure still contribute")
                    (is (= [:s/c] (mapv :source-id failures))
                        "the failure is reported, named by its source")))})

(defspec fold-outcomes-conserves-every-row 200
  (prop/for-all [os (gen/vector gen-outcome 0 8)]
    (let [{:keys [messages failures]} (sources/fold-outcomes os)
          oks  (filter #(contains? (:result %) :ok) os)
          errs (remove #(contains? (:result %) :ok) os)]
      (and (= (count messages) (reduce + 0 (map #(count (get-in % [:result :ok])) oks)))
           (= (map :source-id failures) (map :source-id errs))))))

;; =============================================================================
;; Boundary — messages-from over injected sources
;; =============================================================================

(deftest zero-sources-is-an-empty-read-test
  (is (= [] (sources/messages-from {})))
  (is (= [] (sources/messages-from nil))))

(deftest every-implementation-of-the-port-is-substitutable-test
  (testing "LSP: fn, var, atom slot, reify and record all read the same rows"
    (let [rows [(row "x" 1)]
          f    (fn [] rows)]
      (doseq [[label source] {:fn     f
                              :var    #'reloadable-source
                              :atom   (atom f)
                              :reify  (reify sources/IPiggybackSource (-messages [_] rows))
                              :record (->FixedSource rows)
                              :empty-slot (atom nil)}]
        (is (vector? (sources/messages-from {::s source})) (str label))
        (when-not (#{:var :empty-slot} label)
          (is (= rows (sources/messages-from {::s source})) (str label))))
      (is (= [] (sources/messages-from {::s (atom nil)}))
          "an empty slot yields no rows"))))

(deftest a-failing-source-is-rescued-on-its-own-test
  (let [log  (atom [])
        good (recording log ::good (->FixedSource [(row "good" 5)]))
        bad  (recording log ::bad (failing "source down"))]
    (is (= [(row "good" 5)] (sources/messages-from {::bad bad ::good good}))
        "the healthy source's rows survive a throwing neighbour")
    (is (= #{::good ::bad} (set @log))
        "every source was still read exactly once")
    (is (= 2 (count @log)))))

(deftest a-source-registered-by-var-sees-a-reload-test
  (let [original (var-get #'reloadable-source)]
    (try
      (sources/register! ::by-var #'reloadable-source)
      (sources/register! ::by-value reloadable-source)
      (let [ids-of (fn [] (->> (sources/messages-from (select-keys (sources/registered)
                                                                    [::by-var ::by-value]))
                               (map :agent-id)
                               frequencies))]
        (is (= {"before-reload" 2} (ids-of)))
        ;; What a reload does: rebind the var's root.
        (alter-var-root #'reloadable-source (constantly (fn [] [(row "after-reload" 2)])))
        (is (= {"after-reload" 1 "before-reload" 1} (ids-of))
            "the var registration sees the reload; the captured value does not"))
      (finally
        (alter-var-root #'reloadable-source (constantly original))))))

;; =============================================================================
;; The reader — piggyback/get-messages over the registry
;; =============================================================================

(deftest core-reads-with-nothing-registered-test
  (pb/register-message-source! nil)
  (is (nil? (pb/get-messages "coordinator" :project-id "p"))
      "no swarm, no sources: an empty read, never an exception"))

(deftest a-registered-source-reaches-the-reader-beside-the-default-slot-test
  (pb/register-message-source! (fn [] [(row "from-slot" 10)]))
  (sources/register! ::extra (->FixedSource [(row "from-addon" 11)]))
  (is (= #{"from-slot" "from-addon"}
         (set (map :a (pb/get-messages "coordinator" :project-id "p"))))))

(deftest a-broken-source-never-fails-the-read-test
  (pb/register-message-source! (fn [] [(row "from-slot" 20)]))
  (sources/register! ::broken (failing "addon exploded"))
  (is (= ["from-slot"] (map :a (pb/get-messages "coordinator" :project-id "p")))))

(deftest a-throwing-default-slot-is-rescued-test
  (testing "the pre-fix NPE shape: the slot source derefs a nil registry"
    (pb/register-message-source! (fn [] (throw (NullPointerException. "agent-registry is nil"))))
    (is (nil? (pb/get-messages "coordinator" :project-id "p")))))
