(ns hive-mcp.tools.memory.crud.deferred
  "Durable local outbox for entries whose vectorization failed before the store write.
   An outbox write precedes acknowledgement; reembed drains it by id."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]))

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
        (#{:embedder/embed-failed :embedder/chain-exhausted}
         (:error result))
        (#{:embedder/embed-failed :embedder/chain-exhausted}
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

(defn pending-ids
  "Enumerate the outbox. Only entries successfully parsed are returned."
  []
  (let [dir (io/file *queue-dir*)]
    (if (.isDirectory dir)
      (->> (.listFiles dir)
           (filter #(.endsWith (.getName %) ".edn"))
           (map #(edn/read-string (slurp %)))
           (map :id)
           vec)
      [])))
