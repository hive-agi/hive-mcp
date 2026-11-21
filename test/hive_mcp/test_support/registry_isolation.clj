(ns hive-mcp.test-support.registry-isolation
  "Fail-closed fixtures for tests that mutate the process-wide memory registry.
   Run these tests in a dedicated test JVM, never a server nREPL."
  (:require [hive-mcp.addons.core :as addons]
            [hive-mcp.protocols.memory :as memory]))

(def ^:private fixture-lock (Object.))

(defn safe-test-registry?
  "True only when the addon registry reports no installed or active addons."
  [status]
  (and (map? status)
       (every? #(and (integer? %) (zero? %))
               (map status [:total :active :registered :error]))))

(defn refuse-live-addons!
  "Abort when the addon registry reports a running server (or is malformed).
   The live-nREPL footgun: a fixture that swaps memory stores inside a JVM that
   hosts addons would clobber the stores those addons are using. With no
   argument it reads the live registry status; with STATUS it judges that map."
  ([] (refuse-live-addons! (addons/registry-status)))
  ([status]
   (when-not (safe-test-registry? status)
     (throw (ex-info "Refusing tests in a live addon registry"
                     {:reason :live-addon-registry})))))

(defn refuse-live-registry!
  "Abort before a fixture can replace stores in an addon server or shared JVM."
  []
  (refuse-live-addons!)
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
   Only use after refuse-live-registry! in a dedicated test JVM.
   Replace the slot map in one atomic operation; do not unregister and
   register individual keys (which exposes a partially restored registry)."
  [prior]
  (let [slot (var-get (ns-resolve 'hive-spi.memory.registry 'store-slot))]
    (when-not (and (map? prior) (instance? clojure.lang.IAtom (:state slot)))
      (throw (ex-info "Memory registry slot unavailable for atomic restore"
                      {:reason :missing-memory-slot})))
    (swap! (:state slot) (constantly prior))))

(defn with-isolated-store-registry
  "Run f with a blank registry and restore its prior snapshot on teardown.
   Refuses a nonempty registry rather than clobbering a live store. Serializes
   cooperating fixtures; run this only in a dedicated sequential test JVM."
  [f]
  (locking fixture-lock
    (refuse-live-registry!)
    (let [prior (memory/registered-stores)]
      (try
        (f)
        (finally
          (restore-stores! prior))))))

(defn with-restored-store-registry
  "Run f against the live registry slot map, then put the exact prior map back
   in one atomic swap. Unlike with-isolated-store-registry it accepts prior
   stores, so shared stub fixtures can nest (a per-ns :each fixture around a
   per-test stub). It still refuses a running addon server before f can touch
   a store, and it never calls the global reset-registry! on teardown.
   Takes no lock: a body that waits on a future which enters this fixture
   must not deadlock. Run it in a dedicated sequential test JVM."
  [f]
  (refuse-live-addons!)
  (let [prior (memory/registered-stores)]
    (try
      (f)
      (finally
        (restore-stores! prior)))))

(def isolated-fixture-report
  "What fixture-isolation-report answers for a fixture that cannot clobber a
   live registry."
  {:rethrew?                 true
   :restored-after-throw?    true
   :live-refused?            true
   :live-body-ran?           false
   :live-registry-unchanged? true})

(defn fixture-isolation-report
  "Drive FIXTURE (a clojure.test fixture fn) twice and report what it did to
   the process-wide memory registry. First around a body that leaks a sentinel
   slot and throws: an isolated fixture rethrows and leaves the exact prior
   slot map. Then with the addon registry reporting a running server: an
   isolated fixture refuses with :live-addon-registry before the body runs and
   writes nothing. The prior slot map is put back whatever FIXTURE did, so a
   leaky fixture under test cannot poison the tests after it.
   Compare the answer with isolated-fixture-report."
  [fixture]
  (let [prior     (memory/registered-stores)
        leak!     #(restore-stores! (assoc (memory/registered-stores) ::sentinel ::leaked))
        rethrew?  (try (fixture (fn [] (leak!) (throw (ex-info "body failed" {::body true}))))
                       false
                       (catch clojure.lang.ExceptionInfo e (boolean (::body (ex-data e)))))
        thrown    (memory/registered-stores)
        _         (restore-stores! prior)
        ran?      (atom false)
        refusal   (with-redefs [addons/registry-status
                                (constantly {:total 1 :active 1 :registered 1 :error 0})]
                    (try (fixture (fn [] (reset! ran? true) (leak!)))
                         nil
                         (catch clojure.lang.ExceptionInfo e (:reason (ex-data e)))))
        live      (memory/registered-stores)]
    (restore-stores! prior)
    {:rethrew?                 rethrew?
     :restored-after-throw?    (= prior thrown)
     :live-refused?            (= :live-addon-registry refusal)
     :live-body-ran?           @ran?
     :live-registry-unchanged? (= prior live)}))
