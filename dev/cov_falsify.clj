(ns cov-falsify
  "Falsify malli contracts: instrument an explicit var set, run the EXISTING
   tests for the namespace, report each failing test by name.

   (falsify! 'hive-mcp.agent.budget-router '[hive-mcp.agent.budget-router-test])

   Instrumentation is narrowed with -filter-var over the namespace's own
   contracted vars, never -filter-ns (seen instrumenting hive-schemas itself)."
  (:require [clojure.test :as t]
            [malli.core :as m]
            [malli.instrument :as mi]))

(defn contracted-vars
  "Vars of `ns-sym` that carry an m/=> contract."
  [ns-sym]
  (->> (m/function-schemas)
       (#(get % ns-sym))
       keys
       (map #(ns-resolve ns-sym %))
       (remove nil?)
       set))

(defn falsify!
  "Instrument the contracts of `ns-sym`, run `test-nss`, unstrument.
   Returns {:vars n :pass :fail :error :failures [...]}."
  [ns-sym test-nss]
  (let [vs (contracted-vars ns-sym)
        failures (atom [])
        report t/report]
    (mi/instrument! {:filters [(mi/-filter-var vs)]})
    (try
      (doseq [n test-nss] (require n))
      (binding [t/report
                (fn [m]
                  (when (#{:fail :error} (:type m))
                    (swap! failures conj
                           {:test (some-> t/*testing-vars* first meta :name)
                            :type (:type m)
                            :msg (let [a (:actual m)]
                                   (subs (str (if (instance? Throwable a) (ex-message a) (pr-str a))
                                              (when (:message m) (str " " (:message m))))
                                         0 300))}))
                  (report m))]
        (let [res (apply t/run-tests test-nss)]
          (assoc (select-keys res [:test :pass :fail :error])
                 :vars (count vs) :failures @failures)))
      (finally (mi/unstrument! {:filters [(mi/-filter-var vs)]})))))
