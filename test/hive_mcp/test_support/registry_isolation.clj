(ns hive-mcp.test-support.registry-isolation
  "Fail-closed fixtures for tests that mutate the process-wide memory registry.
   Run these tests in a dedicated test JVM, never a server nREPL."
  (:require [hive-mcp.addons.core :as addons]
            [hive-mcp.protocols.memory :as memory]))

(defn safe-test-registry?
  "True only when the addon registry reports no installed or active addons."
  [status]
  (and (map? status)
       (every? #(and (integer? %) (zero? %))
               (map status [:total :active :registered :error]))))

(defn refuse-live-registry!
  "Abort before a fixture can replace stores in an addon server or shared JVM."
  []
  (when-not (safe-test-registry? (addons/registry-status))
    (throw (ex-info "Refusing tests in a live addon registry"
                    {:reason :live-addon-registry})))
  (when (seq (memory/registered-stores))
    (throw (ex-info "Refusing tests with registered memory stores"
                    {:reason :shared-memory-registry}))))

(defn guard-live-registry-fixture
  "Place before every fixture that writes the global memory registry."
  [f]
  (refuse-live-registry!)
  (f))

(defn restore-stores!
  "Restore the registry snapshot after a test, without a global reset.
   Only use after refuse-live-registry! in a dedicated test JVM."
  [prior]
  (doseq [key (keys (memory/registered-stores))]
    (memory/unregister-store! key))
  (doseq [[key store] prior]
    (memory/register-store! key store)))

(defn with-isolated-store-registry
  "Run f with a blank registry and restore its prior snapshot on teardown.
   Refuses a nonempty registry rather than clobbering a live store."
  [f]
  (refuse-live-registry!)
  (let [prior (memory/registered-stores)]
    (try
      (f)
      (finally
        (restore-stores! prior)))))
