(ns hive-mcp.hot.core-test
  "Core hot-reload: the interlock is derived from the live image, the plan is
   a projection, and the reload drives its ports in order. hive-hot is never
   touched; every effect arrives through a recording stub."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.hot.core :as core]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This namespace is its own fixture: a private defonce makes it a state
;; holder, a record makes it a record definer, and it defines no protocol.
(defonce ^:private probe-state (atom 0))

(defrecord ProbeRecord [])

(def this-ns 'hive-mcp.hot.core-test)

(deftest the-interlock-is-derived-from-the-live-image
  (let [{:keys [protocol state record]} (core/classify "hive-mcp.hot.core-test")]
    (is (zero? @probe-state) "the fixture atom is live, so the class is read off a real root")
    (is (contains? state this-ns) "a private defonce atom makes a state holder")
    (is (contains? record this-ns) "map->ProbeRecord makes a record definer")
    (is (not (contains? protocol this-ns)) "no defprotocol here"))
  (testing "a namespace under the default prefix that holds nothing is in no class"
    (let [{:keys [protocol state record]} (core/classify "hive-mcp.hot.core")]
      (is (not (some #{'hive-mcp.hot.core} (concat protocol state record)))))))

(deftest only-a-protocol-definer-is-pinned
  (is (= {:no-reload #{'a 'b}} (core/interlock {:protocol #{'a 'b} :state #{'b 'c}}))
      "state is kept across the pass, never pinned")
  (is (= ['c 'd] (core/affected {:cascade ['a 'c 'd]} {:protocol #{'a}}))))

(deftest a-file-maps-to-the-namespace-it-conventionally-defines
  (let [root (str (io/file "/r/src"))]
    (is (= "hive-mcp.foo-bar.baz"
           (core/file->ns [root] (io/file root "hive_mcp/foo_bar/baz.clj"))))
    (is (= "hive-mcp.x" (core/file->ns [root] (io/file root "hive_mcp/x.cljc"))))
    (is (nil? (core/file->ns [root] (io/file "/elsewhere/hive_mcp/x.clj"))))))

(deftest core-runs-from-a-directory-in-this-jvm
  (let [root (core/ns-root 'hive-mcp.hot.core)]
    (is (some? root) "the anchor resolves to a source directory, not a jar")
    (is (.exists (io/file root "hive_mcp/hot/core.clj")))
    (is (= [root] (core/core-roots 'hive-mcp.hot.core)))))

(deftest the-plan-is-a-projection-of-the-hive-hot-plan
  (let [root    (str (io/file "/r/src"))
        other   (str (io/file "/o/src"))
        ns-of   (partial core/file->ns [root other])
        classes {:protocol #{'hive-mcp.p} :state #{'hive-mcp.s 'hive-mcp.p} :record #{'hive-mcp.rec}}
        sp      {:want    [(io/file root "hive_mcp/a.clj") (io/file root "hive_mcp/a.clj")]
                 :dragged [(io/file other "o/b.clj")]
                 :skipped [(io/file other "o/c.clj")]
                 :cascade ['hive-mcp.a 'hive-mcp.p 'hive-mcp.s 'hive-mcp.rec 'o.b]}
        report  (core/plan-report sp [root] classes ns-of)]
    (is (= ["hive-mcp.a"] (:pending report)) "duplicates collapse")
    (is (= ["o.b"] (:dragged report)))
    (is (= ["o.c"] (:skipped report)))
    (is (= ["hive-mcp.p"] (:pinned report)))
    (is (= ["hive-mcp.s"] (:kept-state report)) "the pinned namespace is not also kept")
    (is (= ["hive-mcp.rec"] (:records-redefined report)))
    (is (= {:no-reload 1} (:interlock report)))
    (is (false? (:unchanged? report)))
    (is (true? (:unchanged? (core/plan-report (assoc sp :want []) [root] classes ns-of))))))

(defn- recording-ports
  "Stub ports that record every call in `log`, answering `reload-result`.
   `pass` is what prepare-pass! answers."
  ([log reload-result] (recording-ports log reload-result
                                        {:forced ['hive-mcp.stuck] :orphans []
                                         :kept {'hive-mcp.hot.core-test ['probe-state]}}))
  ([log reload-result pass]
   (let [rec (fn [k] (fn [& args] (swap! log conj [k (vec args)]) nil))]
     {:hot/ensure-init!      (fn [opts] (swap! log conj [:ensure-init! [opts]]) {:initialized? true :fresh? false})
      :hot/status            (fn [] {:dirs ["/r/src"]})
      :hot/scope-plan        (fn [roots] (swap! log conj [:scope-plan [roots]])
                               {:want [(io/file "/r/src/hive_mcp/other.clj")]
                                :cascade ['hive-mcp.hot.core-test 'hive-mcp.other]})
      :hot/repair-preview    (fn [prefix] (swap! log conj [:repair-preview [prefix]]) {:stuck [] :dead-links {} :orphans []})
      :hot/prepare-pass!     (fn [prefix nses] (swap! log conj [:prepare-pass! [prefix nses]]) pass)
      :hot/reload-scoped!    (fn [roots] (swap! log conj [:reload-scoped! [roots]]) reload-result)
      :hot/reload-pending!   (fn [] (swap! log conj [:reload-pending! []]) reload-result)
      :host/remount!         (fn [loaded] (swap! log conj [:remount! [loaded]]) {:remounted (count loaded)})
      :host/refresh-tools!   (fn [] (swap! log conj [:refresh-tools! []]) 42)
      :host/refresh-surface! (rec :refresh-surface!)})))

(deftest a-reload-drives-hive-hot-under-the-interlock-then-repairs-the-host
  (let [log    (atom [])
        loaded ['hive-mcp.hot.core-test 'hive-mcp.other]
        ports  (recording-ports log {:success true :loaded loaded :unloaded ['hive-mcp.other]
                                     :ms 7 :skipped ["x.y"] :dragged [] :unchanged? false})
        report (core/reload! {:ports ports :roots ["/r/src"]})]
    (testing "hive-hot is extended with core's root and the protocol interlock"
      (let [[k [opts]] (first @log)]
        (is (= :ensure-init! k))
        (is (= ["/r/src"] (:dirs opts)))
        (is (set? (:no-reload opts)))
        (is (not (contains? opts :no-unload)) "state is kept, not pinned")
        (is (pos-int? (:since opts)) "the baseline is the JVM start, not the first call")))
    (testing "the pass is planned, prepared over the affected namespaces under the prefix, then run scoped to the root"
      (is (= [:scope-plan :prepare-pass! :reload-scoped!] (map first (take 3 (rest @log)))))
      (is (= ["hive-mcp." ['hive-mcp.hot.core-test 'hive-mcp.other]] (second (nth @log 2))))
      (is (= [["/r/src"]] (second (nth @log 3)))))
    (testing "the repairs run after the reload, remount first, on what was loaded"
      (is (= [:remount! :refresh-tools! :refresh-surface!] (map first (drop 4 @log))))
      (is (= [(mapv str loaded)] (second (nth @log 4)))))
    (testing "the report"
      (is (true? (:ok? report)))
      (is (= :scoped (:pass report)))
      (is (= ["hive-mcp.hot.core-test" "hive-mcp.other"] (:loaded report)))
      (is (= ["hive-mcp.other"] (:unloaded report)))
      (is (= {'hive-mcp.hot.core-test ['probe-state]} (:kept-vars report)))
      (is (= ["hive-mcp.stuck"] (:forced-unload report)))
      (is (= ["hive-mcp.hot.core-test"] (:kept-state report)) "kept-state = loaded ∩ state holders")
      (is (= ["hive-mcp.hot.core-test"] (:records-redefined report)))
      (is (= {:remounted 2} (:remount report)))
      (is (= 42 (:tools-refreshed report)))
      (is (= 7 (:ms report)))
      (is (= ["x.y"] (:skipped report)))
      (is (= {:initialized? true :fresh? false} (:hive-hot report))))))

(deftest a-failed-reload-repairs-nothing-and-says-why
  (let [log    (atom [])
        ports  (recording-ports log {:success false :failed 'hive-mcp.bad :error "boom" :loaded [] :unloaded []})
        report (core/reload! {:ports ports :roots ["/r/src"]})]
    (is (false? (:ok? report)))
    (is (= "hive-mcp.bad" (:failed report)))
    (is (= "boom" (:error report)))
    (is (= [:ensure-init! :scope-plan :prepare-pass! :reload-scoped!] (map first @log)) "no repair ran")
    (is (nil? (:remount report)))))

(deftest an-unchanged-tree-with-nothing-queued-runs-no-pass
  (let [log    (atom [])
        ports  (-> (recording-ports log {:success true :loaded [] :unloaded []}
                                    {:forced [] :orphans [] :kept {}})
                   (assoc :hot/scope-plan (fn [_] {:want [] :cascade []})))
        report (core/reload! {:ports ports :roots ["/r/src"]})]
    (is (true? (:ok? report)))
    (is (= :none (:pass report)))
    (is (true? (:unchanged? report)))
    (is (= [:ensure-init! :prepare-pass!] (map first @log)) "neither reloader ran, no repair ran")))

(deftest an-unchanged-tree-with-repairs-queued-runs-the-pending-pass
  (let [log    (atom [])
        ports  (-> (recording-ports log {:success true :loaded ['hive-mcp.gone] :unloaded [] :pending-pass? true}
                                    {:forced ['hive-mcp.holder] :orphans ['hive-mcp.gone]
                                     :dead-links {'hive-mcp.holder ['hive-mcp.gone]} :kept {}})
                   (assoc :hot/scope-plan (fn [_] {:want [] :cascade []})))
        report (core/reload! {:ports ports :roots ["/r/src"]})]
    (is (true? (:ok? report)))
    (is (= :pending (:pass report)))
    (is (= [:ensure-init! :prepare-pass! :reload-pending! :remount! :refresh-tools! :refresh-surface!]
           (map first @log))
        "the pending pass replaces the scoped one and is repaired like any other")
    (is (= ["hive-mcp.holder"] (:forced-unload report)))
    (is (= ["hive-mcp.gone"] (:orphans report)))
    (is (= {'hive-mcp.holder ['hive-mcp.gone]} (:dead-links report)))))

(deftest a-jar-backed-core-refuses-with-restart-required
  (let [log (atom [])]
    (is (= {:error :restart-required :ok? false}
           (dissoc (core/reload! {:ports (recording-ports log {}) :roots []}) :message)))
    (is (= {:error :restart-required}
           (dissoc (core/plan {:ports (recording-ports log {}) :roots []}) :message)))
    (is (empty? @log) "hive-hot is not even touched")))

(deftest a-throwing-repair-is-folded-into-the-report
  (let [log    (atom [])
        ports  (assoc (recording-ports log {:success true :loaded ['hive-mcp.x] :unloaded []})
                      :host/refresh-tools! (fn [] (throw (ex-info "table gone" {}))))
        report (core/reload! {:ports ports :roots ["/r/src"]})]
    (is (true? (:ok? report)) "a repair failure does not unmake the reload")
    (is (= {:error "table gone"} (:tools-refreshed report)))
    (is (str/blank? (str (:error report))))))
