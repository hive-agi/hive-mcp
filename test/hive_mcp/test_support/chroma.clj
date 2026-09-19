(ns hive-mcp.test-support.chroma
  "Shared live-Chroma availability support for test suites.

   The probe result is cached per JVM via a delay so suites with many tests
   pay at most one HTTP heartbeat, and the skip notice prints exactly once
   per label per JVM (tracked by the `announced` atom).")

(def ^:private reachable
  (delay
    (try
      (let [client (java.net.http.HttpClient/newHttpClient)
            req    (-> (java.net.http.HttpRequest/newBuilder
                        (java.net.URI. "http://localhost:8000/api/v2/heartbeat"))
                       (.timeout (java.time.Duration/ofMillis 2000))
                       (.GET)
                       (.build))
            resp   (.send client req (java.net.http.HttpResponse$BodyHandlers/ofString))]
        (= 200 (.statusCode resp)))
      (catch Exception _ false))))

(defn reachable?
  "True when the live Chroma server on localhost:8000 answered the heartbeat.
   Memoized: the HTTP probe runs at most once per JVM."
  []
  @reachable)

(def ^:private announced (atom #{}))

(defn skip-unless-reachable
  "Fixture wrapper: run the test when live Chroma is reachable; otherwise skip
   it, printing a single notice line for `label` (once per JVM, not once per
   test)."
  [label]
  (fn [f]
    (if (reachable?)
      (f)
      (when-not (contains? @announced label)
        (swap! announced conj label)
        (println (str "[" label "] live Chroma unreachable on localhost:8000 - skipping."))))))
