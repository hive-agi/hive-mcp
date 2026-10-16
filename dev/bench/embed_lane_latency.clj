(ns bench.embed-lane-latency
  "Latency of one interactive embed while background (:batch) embeds hold and
   queue on a 1-permit gate. Stub provider: 500 ms per call, 1 holder + 4
   queued batch callers. `before` calls outside tool ingress (:batch lane),
   `after` calls through wrap-handler-context (:interactive lane).
   (run) => {:before-ms [...] :after-ms [...]}"
  (:require [hive-mcp.embeddings.protocol :as proto]
            [hive-mcp.embeddings.shared-gate :as shared]
            [hive-mcp.server.routes.middleware :as mw]))

(defn- slow-provider [ms]
  (reify proto/EmbeddingProvider
    (embed-text [_ _] (Thread/sleep (long ms)) [1.0])
    (embed-batch [_ ts] (Thread/sleep (long ms)) (mapv (constantly [1.0]) ts))
    (embedding-dimension [_] 1)))

(defn- tool-call [f]
  (binding [mw/*identity-get-slave* (constantly nil)]
    ((mw/wrap-handler-context (fn [_] (f))) {})))

(defn trial
  "Milliseconds for one embed-text arriving behind a holder and 4 queued batches."
  [via-ingress?]
  (let [gate   (shared/new-gate 1 60000)
        port   (shared/gated-provider (slow-provider 500) gate)
        holder (future (proto/embed-batch port ["ingest"]))
        _      (Thread/sleep 50)
        queued (doall (for [i (range 4)]
                        (let [f (future (proto/embed-batch port [(str "ingest-" i)]))]
                          (shared/await-waiters gate :batch (inc i) 2000)
                          f)))
        t0     (System/nanoTime)
        call   #(proto/embed-text port "query")
        _      (if via-ingress? (tool-call call) (call))
        ms     (/ (- (System/nanoTime) t0) 1e6)]
    @holder
    (run! deref queued)
    (Math/round (double ms))))

(defn run []
  {:before-ms (vec (repeatedly 3 #(trial false)))
   :after-ms  (vec (repeatedly 3 #(trial true)))})
