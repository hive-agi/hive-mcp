(ns hive-mcp.tools.memory.crud.deferred
  "Durable local outbox for entries whose vectorization failed before the store write.
   An outbox write precedes acknowledgement; reembed drains it by id."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:dynamic *queue-dir*
  (str (System/getProperty "user.home") "/.local/share/hive/pending-reembed"))

(defn reembed-tags
  "Pure tag transition for a deferred or successfully reembedded entry."
  [tags deferred?]
  (let [clean (vec (remove #{"pending-reembed"} tags))]
    (cond-> clean deferred? (conj "pending-reembed"))))

(defn embedding-failure?
  "Only a failed embed is deferrable; a failed store write must stay an error."
  [e]
  (let [data (ex-data e)
        result (:result data)]
    (or (= "embed-for-entry failed" (ex-message e))
        (#{:embedder/embed-failed :embedder/chain-exhausted :embedder/gate-timeout}
         (:error result))
        (#{:embedder/embed-failed :embedder/chain-exhausted :embedder/gate-timeout}
         (:error data)))))

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
   and without the pending-reembed tag."
  [parked]
  (-> parked
      (dissoc :deferred/store-key)
      (update :tags reembed-tags false)))

(defn amend!
  "Merge `fields` into a parked record (e.g. the :kg-outgoing edge ids a
   finalize created after parking) so the drain writes them too. No-op and nil
   when `id` is not parked."
  [id fields]
  (when-let [parked (lookup id)]
    (park! (merge parked fields))))

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
