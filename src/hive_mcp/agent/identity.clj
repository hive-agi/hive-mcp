(ns hive-mcp.agent.identity
  "Wiring for verified spawn identity: key custody, the signer adapter, the
   mint at spawn, and the verification at the request boundary.

   The credential DOMAIN (claims, canonical bytes, mint/verify, the
   :observe/:enforce policy) is pure and lives in the hive-agent addon,
   `hive-agent.identity.credential`. hive-mcp does not depend on hive-agent,
   so it is reached by soft resolution, as the grant domain is. Without it:
     - nothing is minted (a child starts with no credential, as before),
     - a presented credential cannot be verified, so it is never identity,
     - the policy falls back to :observe behaviour: nothing is refused.

   Effects here, each behind one seam a test can rebind:
     *key-file*   where the signing key lives (XDG state dir, 0600)
     *clock*      0-arg fn, epoch ms
     *mode*       0-arg fn, :observe | :enforce, from
                  [:services :agent :identity :mode] (default :observe)

   Signer port (`signer`): a map {:kid :sign :verify} over hive-system's
   Ed25519 DER adapter (`hive-system.crypto.ed25519-der`, JDK provider). The
   key backend can change behind it without touching the domain or the
   middleware. No crypto primitive is written here.

   The key is generated on first use and persisted, never logged. An
   in-memory key would change on restart and invalidate every live ling's
   credential."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-system.crypto.ed25519-der :as ed]
            [hive-dsl.result :as r]
            [taoensso.timbre :as log])
  (:import [java.nio.file Files LinkOption StandardCopyOption]
           [java.nio.file.attribute FileAttribute PosixFilePermissions]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def credential-env
  "Environment variable that carries a child's credential. bb-mcp forwards it,
   unchanged and opaque, as `_caller_credential`."
  "HIVE_AGENT_CREDENTIAL")

(def slave-id-env "CLAUDE_SWARM_SLAVE_ID")

(def reserved-env
  "Child env names only the server sets. A caller's `env-extra` never
   overrides them."
  #{credential-env slave-id-env})

(def credential-arg
  "Request argument the transport carries the credential in. Stripped at the
   boundary before any handler, log line or piggyback sees the args."
  :_caller_credential)

(def default-ttl-ms
  "Credential lifetime: a week, longer than any ling run observed."
  (* 7 24 60 60 1000))

;; =============================================================================
;; Domain (soft-resolved from hive-agent)
;; =============================================================================

(defn- soft [sym]
  (try (requiring-resolve sym) (catch Throwable _ nil)))

(defn domain
  "The credential domain fns, or nil when the hive-agent addon is absent."
  []
  (let [fs {:claims (soft 'hive-agent.identity.credential/claims)
            :mint   (soft 'hive-agent.identity.credential/mint)
            :verify (soft 'hive-agent.identity.credential/verify)
            :decide (soft 'hive-agent.identity.credential/decide)}]
    (when (every? some? (vals fs)) fs)))

(def ^:dynamic *domain*
  "0-arg port: the credential domain. Rebound by tests."
  domain)

;; =============================================================================
;; Effects: clock, mode, key custody
;; =============================================================================

(def ^:dynamic *clock* (fn [] (System/currentTimeMillis)))

(defn parse-mode
  "A configured mode as :observe or :enforce. Anything unknown is :observe."
  [v]
  (let [k (cond (keyword? v) v
                (string? v) (keyword (str/replace (str/trim v) #"^:" ""))
                :else nil)]
    (if (= :enforce k) :enforce :observe)))

(defn configured-mode
  "[:services :agent :identity :mode] from config, default :observe."
  []
  (parse-mode
   (when-let [get-in-config (soft 'hive-mcp.config.core/get-in-config)]
     (try (get-in-config [:services :agent :identity :mode])
          (catch Throwable _ nil)))))

(def ^:dynamic *mode* configured-mode)

(defn configured-ttl-ms []
  (or (when-let [get-in-config (soft 'hive-mcp.config.core/get-in-config)]
        (try (let [v (get-in-config [:services :agent :identity :ttl-ms])]
               (when (and (integer? v) (pos? v)) v))
             (catch Throwable _ nil)))
      default-ttl-ms))

(defn default-key-file
  "$XDG_STATE_HOME/hive-mcp/identity/spawn-signing-key.edn
   (XDG_STATE_HOME defaults to ~/.local/state)."
  []
  (let [state (or (not-empty (System/getenv "XDG_STATE_HOME"))
                  (str (System/getProperty "user.home") "/.local/state"))]
    (io/file state "hive-mcp" "identity" "spawn-signing-key.edn")))

(def ^:dynamic *key-file* default-key-file)

(defn- b64 ^String [^bytes b] (.encodeToString (java.util.Base64/getEncoder) b))
(defn- un-b64 ^bytes [^String s] (.decode (java.util.Base64/getDecoder) s))

(defn- posix? [^java.io.File f]
  (contains? (.supportedFileAttributeViews (.getFileSystem (.toPath f))) "posix"))

(defn- key-id
  "Key id: the first 16 hex digits of SHA-256 over the X.509 public key."
  [^bytes public]
  (let [d (.digest (java.security.MessageDigest/getInstance "SHA-256") public)]
    (subs (apply str (map #(format "%02x" (bit-and (long %) 0xff)) d)) 0 16)))

(defn- write-key-file!
  "Write `content` to `f` atomically, created 0600 in a 0700 directory."
  [^java.io.File f ^String content]
  (let [dir (.getParentFile f)]
    (.mkdirs dir)
    (when (posix? dir)
      (Files/setPosixFilePermissions (.toPath dir) (PosixFilePermissions/fromString "rwx------")))
    (let [attrs (if (posix? dir)
                  (into-array FileAttribute [(PosixFilePermissions/asFileAttribute
                                              (PosixFilePermissions/fromString "rw-------"))])
                  (make-array FileAttribute 0))
          tmp   (Files/createTempFile (.toPath dir) ".key" ".tmp" attrs)]
      (spit (.toFile tmp) content)
      (Files/move tmp (.toPath f) (into-array java.nio.file.CopyOption
                                              [StandardCopyOption/ATOMIC_MOVE])))))

(defn load-or-create-key!
  "The signing key at `f`: read when present, else generated and persisted.
   -> {:kid :private (PKCS#8 bytes) :public (X.509 bytes)}. A present file
   that group or others may access is refused rather than used."
  [^java.io.File f]
  (when (and (.exists f) (posix? f))
    (let [perms (Files/getPosixFilePermissions (.toPath f) (make-array LinkOption 0))]
      (when (some #(re-find #"^(GROUP|OTHERS)_" (str %)) perms)
        (throw (ex-info "spawn signing key file is accessible to group or others; refusing it"
                        {:file (str f)})))))
  (if (.exists f)
    (let [{:keys [private public]} (edn/read-string (slurp f))
          pub (un-b64 public)]
      {:kid (key-id pub) :private (un-b64 private) :public pub})
    (let [{:keys [private public]} (ed/generate-keypair)]
      (write-key-file! f (pr-str {:alg "ed25519" :private (b64 private) :public (b64 public)}))
      (log/info "identity: generated the spawn signing key" {:file (str f) :kid (key-id public)})
      {:kid (key-id public) :private private :public public})))

(defn ->signer
  "The signer port over a key {:kid :private :public}, through hive-system's
   Ed25519 DER adapter."
  [{:keys [kid private public]}]
  {:kid    kid
   :sign   (fn [^bytes payload]
             (let [res (ed/sign {:crypto/key private :crypto/data payload})]
               (if (r/ok? res)
                 (:crypto/signature (:ok res))
                 (throw (ex-info "spawn credential signing failed" {:error (:error res)})))))
   :verify (fn [^bytes payload ^bytes sig]
             (let [res (ed/verify {:crypto/pubkey public :crypto/data payload :crypto/signature sig})]
               (and (r/ok? res) (true? (:crypto/valid? (:ok res))))))})

(defonce ^:private signer-cache (atom nil))

(defn signer
  "The signer for the current `*key-file*`, loaded once per file."
  []
  (let [f (*key-file*)
        k (.getAbsolutePath ^java.io.File f)]
    (or (get @signer-cache k)
        (let [s (->signer (load-or-create-key! f))]
          (swap! signer-cache assoc k s)
          s))))

(def ^:dynamic *signer* signer)

;; =============================================================================
;; Mint (spawn side)
;; =============================================================================

(defn mint-for-child
  "A credential token for a child about to be spawned, or nil when the
   domain is absent or minting fails (the spawn then proceeds without one,
   as before this change; the failure is logged without key material)."
  [{:keys [agent-id parent-id depth grant]}]
  (when-let [{:keys [claims mint]} (*domain*)]
    (try
      (let [s (*signer*)]
        (mint s (claims {:kid (:kid s) :agent-id agent-id :parent-id parent-id
                         :depth depth :grant grant :issued-at-ms (*clock*)
                         :ttl-ms (configured-ttl-ms)})))
      (catch Throwable t
        (log/warn "identity: could not mint a spawn credential"
                  {:agent-id agent-id :error (ex-message t)})
        nil))))

(defn mint-for-slave
  "A credential for the registered slave `slave-id`, bound to the parent,
   depth and grant its registry row records (the row is written before any
   backend starts the child). nil when there is no row or no domain."
  [get-slave slave-id]
  (when-let [row (try (get-slave slave-id) (catch Throwable _ nil))]
    (let [p (:slave/parent row)
          p (if (map? p) (:slave/id p) p)]
      (mint-for-child {:agent-id  slave-id
                       :parent-id p
                       :depth     (:slave/depth row)
                       :grant     (:slave/grant row)}))))

(defn child-env
  "Merge a caller's `env-extra` with the server's identity vars: reserved
   names are dropped from `env-extra` and set from the server, so a caller
   cannot override them. Keys of `env-extra` may be keywords or strings."
  [env-extra agent-id token]
  (cond-> (into {}
                (keep (fn [[k v]] (let [n (name k)]
                                    (when-not (contains? reserved-env n) [n (str v)]))))
                env-extra)
    agent-id (assoc slave-id-env (str agent-id))
    token    (assoc credential-env token)))

;; =============================================================================
;; Verify (request side)
;; =============================================================================

(defn claimant
  "What the registry says `caller-id` is: :coordinator for a coordinator lane,
   :spawned when it (or its agent part) has a registry row with a parent or
   a positive depth, else :unknown. Pure over `get-slave`."
  [get-slave caller-id]
  (let [id    (some-> caller-id str not-empty)
        agent (some-> id (str/split #":" 2) first)]
    (cond
      (nil? id) :unknown
      (str/starts-with? agent "coordinator") :coordinator
      :else (let [row (some #(try (get-slave %) (catch Throwable _ nil))
                            (distinct [id agent]))]
              (if (and row (or (some? (:slave/parent row))
                               (pos? (long (or (:slave/depth row) 0)))))
                :spawned
                :unknown)))))

(defonce ^{:doc "Anomaly counters: {[:missing|:invalid] n}."} anomalies (atom {}))

(defn anomaly-counts [] @anomalies)

(defn resolve-request
  "Verify the request's credential and decide what its identity is worth.
   Pure over its inputs.

   -> {:args      args without the credential (always stripped)
       :identity  {:caller-id :verified? :claims}  for the request context
       :decision  the domain decision, or a fallback when the domain is absent}"
  [{:keys [dom signer get-slave now-ms mode]} args]
  (let [token     (get args credential-arg)
        caller-id (some-> (:_caller_id args) str not-empty)
        stripped  (dissoc args credential-arg "_caller_credential")
        verification (when (and (string? token) (not (str/blank? token)))
                       (if (and dom signer)
                         ((:verify dom) signer token caller-id now-ms)
                         {:error :unverifiable :message "no credential domain loaded"}))
        who       (claimant get-slave caller-id)
        decision  (if dom
                    ((:decide dom) {:mode mode :claimant who :verification verification})
                    {:verified? false :allow? true
                     :anomaly (when verification :invalid)})]
    {:args     stripped
     :identity (cond-> {:caller-id caller-id
                        :claimant  who
                        :verified? (true? (:verified? decision))}
                 (:verified? decision) (assoc :claims (:ok verification)))
     :decision (assoc decision :mode mode)}))

(defn refusal-text
  [{:keys [reason]} caller-id]
  (str "REFUSED by identity - the caller id " (pr-str caller-id)
       " belongs to a spawned agent and the request carries no valid credential for it"
       (when reason (str " (" reason ")"))
       ". Identity mode is :enforce. A spawned agent's credential is set by the "
       "server in HIVE_AGENT_CREDENTIAL at spawn; a process that does not hold it "
       "cannot speak as that agent."))

(defn note-anomaly!
  "Log and count an anomalous identity decision. Never logs the credential."
  [{:keys [anomaly reason mode allow?]} tool-name caller-id]
  (when anomaly
    (swap! anomalies update anomaly (fnil inc 0))
    (log/warn "identity: unverified caller" {:tool tool-name :caller caller-id
                                             :anomaly anomaly :reason reason
                                             :mode mode :refused? (not allow?)})))

(defn verify-request!
  "The request-boundary entry: `resolve-request` with the live ports, plus
   anomaly logging. -> the same map as `resolve-request`."
  [args tool-name get-slave]
  (let [dom (*domain*)
        token? (some? (get args credential-arg))
        s   (when (and dom token?)
              (try (*signer*) (catch Throwable t
                                (log/warn "identity: signer unavailable" {:error (ex-message t)})
                                nil)))
        res (resolve-request {:dom dom :signer s :get-slave get-slave
                              :now-ms (*clock*) :mode (*mode*)}
                             args)]
    (note-anomaly! (:decision res) tool-name (get-in res [:identity :caller-id]))
    res))
