(ns hive-mcp.tools.memory.crud.deferred
  "Durable local outbox for entries whose vectorization failed before the store write.
   An outbox write precedes acknowledgement; reembed drains it by id."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [malli.core :as m]))

(def ^:dynamic *queue-dir*
  (str (System/getProperty "user.home") "/.local/share/hive/pending-reembed"))

(defn reembed-tags
  "Pure tag transition for a deferred or successfully reembedded entry."
  [tags deferred?]
  (let [clean (vec (remove #{"pending-reembed"} tags))]
    (cond-> clean deferred? (conj "pending-reembed"))))

(def retryable-embed-errors
  "Embedder failures a later attempt can cure. :embedder/no-provider is the
   post-restart warmup window (the provider chain is not configured yet);
   :embedder/dim-mismatch is deliberately absent: retrying never fixes it, so
   it stays a loud error instead of a silent loop in the outbox."
  #{:embedder/embed-failed :embedder/chain-exhausted :embedder/gate-timeout
    :embedder/no-provider})

(defn embedding-failure?
  "Only a failed embed is deferrable; a failed store write must stay an error."
  [e]
  (let [data (ex-data e)
        result (:result data)]
    (or (= "embed-for-entry failed" (ex-message e))
        (contains? retryable-embed-errors (:error result))
        (contains? retryable-embed-errors (:error data)))))

(defn failure-of
  "Pure: the {:error kw :message str} a failed embed leaves on its outbox
   record, from either the thrown exception or an error-result map."
  [e-or-result]
  (if (instance? Throwable e-or-result)
    (let [data (ex-data e-or-result)]
      {:error (or (:error (:result data)) (:error data))
       :message (ex-message e-or-result)})
    {:error (:error e-or-result)
     :message (or (:message e-or-result) (pr-str e-or-result))}))

(defn with-failure
  "Pure: record one more failed embed attempt on a parked record. The first
   failure time is kept; the last error and the attempt count are updated, so
   a record stuck in the outbox says how long and why."
  [parked failure now-iso]
  (-> parked
      (update :deferred/attempts (fnil inc 0))
      (update :deferred/first-failed-at #(or % now-iso))
      (assoc :deferred/last-failed-at now-iso
             :deferred/last-error failure)))

(defn immediately-retryable?
  "Pure: a failure worth one more try before the caller is answered. A gate
   timeout already waited its full budget, so retrying it only stacks latency."
  [failure]
  (contains? #{:embedder/embed-failed :embedder/no-provider} (:error failure)))

(def default-retry
  "Immediate retry policy for a write whose embed failed: total tries and the
   pause between them. Short on purpose; the outbox owns the long retry."
  {:attempts 3 :delay-ms 250})

(defn call-with-embed-retry
  "Run THUNK (a store add). An embedding failure it raises or returns that is
   `immediately-retryable?` is retried up to (:attempts policy) times, pausing
   (:delay-ms policy) via the injected `sleep!` between tries. Answers
   {:ok result} on success, or {:failure {:error :message} :attempts n} when
   the failure is an embedding failure that outlasted the policy. A failure
   that is not an embedding failure propagates untouched, so a store error is
   never mistaken for a deferrable one."
  [thunk {:keys [attempts delay-ms] :or {attempts 3 delay-ms 250}} sleep!]
  (loop [n 1]
    (let [outcome (try
                    (let [result (thunk)]
                      (if (and (map? result)
                               (embedding-failure? (ex-info "Embedding result" {:result result})))
                        {:failure (failure-of result)}
                        {:ok result}))
                    (catch Exception e
                      (if (embedding-failure? e)
                        {:failure (failure-of e)}
                        (throw e))))]
      (if (and (:failure outcome)
               (< n attempts)
               (immediately-retryable? (:failure outcome)))
        (do (sleep! (* n delay-ms))
            (recur (inc n)))
        (cond-> outcome (:failure outcome) (assoc :attempts n))))))

(defn- path-for [id]
  ;; Hex-encode UTF-8 rather than trusting a caller-controlled id as a path.
  (io/file *queue-dir* (str (apply str (map #(format "%02x" (bit-and 0xff %))
                                             (.getBytes (str id) "UTF-8"))) ".edn")))

(defn park!
  "Atomically persist an unvectorized entry; throws if persistence fails.
   Keep the outbox private because entries can contain sensitive content."
  [entry]
  (let [target (path-for (:id entry))
        dir (.getParentFile target)
        _ (.mkdirs dir)
        _ (java.nio.file.Files/setPosixFilePermissions (.toPath dir)
            (java.nio.file.attribute.PosixFilePermissions/fromString "rwx------"))
        temp (java.io.File/createTempFile "pending-" ".edn" dir)]
    (try
      (spit temp (pr-str (update entry :tags reembed-tags true)))
      (java.nio.file.Files/setPosixFilePermissions (.toPath temp)
        (java.nio.file.attribute.PosixFilePermissions/fromString "rw-------"))
      (java.nio.file.Files/move (.toPath temp) (.toPath target)
                                (into-array java.nio.file.CopyOption
                                            [java.nio.file.StandardCopyOption/REPLACE_EXISTING
                                             java.nio.file.StandardCopyOption/ATOMIC_MOVE]))
      (:id entry)
      (finally (.delete temp)))))

(defn lookup
  "Read a parked entry by id, or nil."
  [id]
  (let [f (path-for id)]
    (when (.exists f)
      (edn/read-string (slurp f)))))

(defn remove!
  "Acknowledge an entry after the vector store accepted it."
  [id]
  (java.nio.file.Files/deleteIfExists (.toPath (path-for id))))

(defn with-store-key
  "Pure: record the IMemoryStore slot an entry must drain into. :default is
   left implicit so a record parked before this field drains as before."
  [entry store-key]
  (cond-> entry
    (and store-key (not= :default store-key)) (assoc :deferred/store-key store-key)))

(defn store-key-of
  "The slot a parked record drains into, or nil for the caller's store."
  [parked]
  (:deferred/store-key parked))

(defn ->store-entry
  "Pure: the entry a drain hands to add-entry!, without outbox bookkeeping
   (every :deferred/* key) and without the pending-reembed tag."
  [parked]
  (-> (into {} (remove (fn [[k _]] (and (keyword? k) (= "deferred" (namespace k)))))
            parked)
      (update :tags reembed-tags false)))

(defn amend!
  "Merge `fields` into a parked record (e.g. the :kg-outgoing edge ids a
   finalize created after parking) so the drain writes them too. No-op and nil
   when `id` is not parked."
  [id fields]
  (when-let [parked (lookup id)]
    (park! (merge parked fields))))

(defn record-failure!
  "Stamp one failed drain attempt onto the parked record for `id` (attempt
   count, first/last failure time, last error) and answer the updated record.
   Nil when `id` is not parked or the stamp itself fails (logged): the outbox
   record is never lost to bookkeeping, so a corrupt record just stays as is."
  [id e-or-result now-iso]
  (try
    (when-let [parked (lookup id)]
      (let [updated (with-failure parked (failure-of e-or-result) now-iso)]
        (park! updated)
        updated))
    (catch Exception stamp-error
      (log/warn "Could not stamp the failure on outbox record" id
                "-" (ex-message stamp-error))
      nil)))

(defn id-of-file-name
  "Pure inverse of path-for's name encoding: the entry id a queue file holds,
   or nil when the name is not one this outbox wrote (a temp file, a stray)."
  [file-name]
  (when (and (string? file-name) (str/ends-with? file-name ".edn"))
    (let [hex (subs file-name 0 (- (count file-name) 4))]
      (when (and (seq hex) (even? (count hex))
                 (every? #(Character/isLetterOrDigit (char %)) hex))
        (try
          (String. (byte-array (map #(unchecked-byte (Integer/parseInt (apply str %) 16))
                                    (partition 2 hex)))
                   "UTF-8")
          (catch NumberFormatException _ nil))))))

(defn pending-ids
  "Enumerate the outbox by FILE NAME, never by parsing contents: one corrupt
   record must not hide every other pending id from the drain. A corrupt
   record then fails its own reembed and stays parked, visible as an error."
  []
  (let [dir (io/file *queue-dir*)]
    (if (.isDirectory dir)
      (->> (.listFiles dir)
           (keep #(id-of-file-name (.getName ^java.io.File %)))
           sort
           vec)
      [])))

;;; =============================================================================
;;; Contracts
;;; =============================================================================

(def Tags
  "Entry tags as stored: a sequential or set of tag strings, or nil. The tag
   transition walks any seqable element-wise, and an existing trifecta feeds a
   bare string, so a string is admitted too (its chars become the elements)."
  [:maybe [:or [:sequential :string] [:set :string] :string]])

(def EntryId
  "A memory entry id, or nil when a caller passed none (the outbox then
   names the file after the empty string and finds nothing)."
  [:maybe :string])

(def StoreKey
  "IMemoryStore registry slot (:default, :kanban, ...)."
  :keyword)

(def ParkedEntry
  "A memory entry as parked in the outbox: any entry map with an :id; the
   pending-reembed tag and :deferred/store-key are outbox bookkeeping."
  [:map
   [:id :string]
   [:tags {:optional true} Tags]
   [:deferred/store-key {:optional true} StoreKey]])

(def Throwable_
  "Any Throwable; generates ex-info values carrying an embedder error."
  [:fn {:gen/schema [:enum :embedder/embed-failed :embedder/chain-exhausted
                     :embedder/gate-timeout :other]
        :gen/fmap (fn [k] (ex-info (name k) {:error k}))}
   #(instance? Throwable %)])

(def ^:private Char_
  "A character: what a bare-string Tags input contributes per element."
  [:fn {:gen/schema [:int {:min 97 :max 122}] :gen/fmap char} char?])

(def Failure
  "The {:error :message} a failed embed leaves on an outbox record. Either
   field may be nil (a Throwable with no message, an error with no keyword)."
  [:map
   [:error [:maybe :keyword]]
   [:message [:maybe :string]]])

(m/=> reembed-tags [:=> [:cat Tags :any] [:vector [:or :string Char_]]])

(m/=> embedding-failure? [:=> [:cat Throwable_] [:maybe [:or :boolean :keyword]]])

(m/=> park! [:=> [:cat ParkedEntry] :string])

(m/=> lookup [:=> [:cat EntryId] [:maybe :map]])

(m/=> remove! [:=> [:cat EntryId] :boolean])

(m/=> with-store-key [:=> [:cat :map [:maybe StoreKey]] :map])

(m/=> store-key-of [:=> [:cat [:maybe :map]] [:maybe StoreKey]])

(m/=> ->store-entry [:=> [:cat :map] [:map [:tags [:vector :string]]]])

(m/=> amend! [:=> [:cat EntryId [:maybe :map]] [:maybe :string]])

(m/=> id-of-file-name [:=> [:cat [:maybe :string]] [:maybe :string]])

(m/=> pending-ids [:=> [:cat] [:vector :string]])

(m/=> failure-of [:=> [:cat [:or Throwable_ :map]] Failure])

(m/=> with-failure [:=> [:cat ParkedEntry Failure :string] :map])

(m/=> immediately-retryable? [:=> [:cat [:maybe :map]] :boolean])

(def ^:private Thunk
  "A store add; generates thunks answering a plain stored-entry result."
  [:fn {:gen/schema [:enum :stored] :gen/fmap (fn [k] (constantly {:id (name k)}))}
   ifn?])

(def ^:private Sleep
  "The injected pause; generates a no-op."
  [:fn {:gen/schema [:enum :nop] :gen/fmap (fn [_] (constantly nil))} ifn?])

(def ^:private RetryPolicy
  [:maybe
   [:map
    [:attempts {:optional true} [:int {:min 1 :max 3}]]
    [:delay-ms {:optional true} [:int {:min 0 :max 5}]]]])

(m/=> call-with-embed-retry [:=> [:cat Thunk RetryPolicy Sleep] [:map]])

(m/=> record-failure! [:=> [:cat EntryId [:or Throwable_ :map] :string] [:maybe :map]])
