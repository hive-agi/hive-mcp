(ns hive-mcp.await.sources.secret-refs
  "Secret REFS for await sources: a ref NAMES a secret, it never is one.

   Grammar: <scheme>:<id>. Each scheme has a resolver in a registry (OCP):
     k8s:<namespace>/<secret>/<key>   a key of a Kubernetes Secret, read with
                                      `kubectl get secret` (registered here,
                                      on the process port, bounded)
     vault:<row-id>                   a hive vault row; contributed by the
                                      hive.cloak addon, not by core
   An addon registers more with register-resolver!. A ref whose scheme has
   no resolver is refused at create time, naming the scheme.

   Resolution is BOUNDED: a resolver runs under `resolve-timeout-ms`, so a
   hung lookup (a prompt for a passphrase, an unreachable API server) fails
   one poll instead of holding a shared poll thread. A resolved value is
   never logged, persisted or put into an event or an error."
  (:require [clojure.string :as str]
            [hive-mcp.await.sources.boundary :as b]
            [malli.core :as m])
  (:import (java.util Base64)
           (java.util.concurrent TimeUnit TimeoutException)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def resolve-timeout-ms
  "The longest one secret resolution may take."
  5000)

(def ^:private dns-label #"[a-z0-9]([-a-z0-9]{0,61}[a-z0-9])?")
(def ^:private dns-sub #"[a-z0-9]([-a-z0-9.]{0,251}[a-z0-9])?")
(def ^:private secret-key #"[-._A-Za-z0-9]{1,253}")
(def ^:private row-id #"[A-Za-z0-9][-_A-Za-z0-9.]{0,127}")

(def Ref
  [:map [:scheme :string] [:id :string] [:ref :string]])

(defn parse-ref
  "{:scheme :id :ref} for a ref string, or nil when it is not one of
   <scheme>:<id> with a lower-case scheme and no whitespace. Pure."
  [s]
  (when (string? s)
    (let [t (str/trim s)]
      (when-let [[_ scheme id] (re-matches #"([a-z][a-z0-9-]{0,15}):(\S{1,512})" t)]
        (when-not (or (str/starts-with? id "//") (str/includes? id "://") (str/includes? id "@"))
          {:scheme scheme :id id :ref t})))))

(m/=> parse-ref [:=> [:cat :any] [:maybe Ref]])

(defn parse-k8s-id
  "{:namespace :secret :key} for the id of a k8s: ref, or nil. Pure."
  [id]
  (let [[ns' secret k :as parts] (str/split (str id) #"/")]
    (when (and (= 3 (count parts))
               (re-matches dns-label ns') (re-matches dns-sub secret) (re-matches secret-key k)
               (not (str/includes? k "..")))
      {:namespace ns' :secret secret :key k})))

(m/=> parse-k8s-id [:=> [:cat :any] [:maybe [:map [:namespace :string] [:secret :string] [:key :string]]]])

(defn valid-vault-id?
  "True for a vault: row id. Pure."
  [id]
  (boolean (and (string? id) (re-matches row-id id) (not (str/includes? id "..")))))

(defn k8s-argv
  "The kubectl argv that reads the k8s ref `parsed`, under `context`. Pure."
  [{:keys [namespace secret key]} context]
  (cond-> ["kubectl"]
    context (into ["--context" context])
    true (into ["get" "secret" secret "-n" namespace "-o"
                (str "jsonpath={.data." (str/replace key "." "\\.") "}")])))

(m/=> k8s-argv [:=> [:cat :map [:maybe :string]] [:vector :string]])

;; =============================================================================
;; Registry
;; =============================================================================

(defn- refusal [message data]
  (ex-info (str "await: " message) (merge {:error :await/invalid-subscription} data)))

(defn- missing [ref]
  (ex-info (str "await: secret " ref " did not resolve")
           {:error :await/secret-missing :ref ref}))

(def ^:dynamic *process*
  "The IProcessRunner the k8s: resolver reads through, at resolve time.
   nil: the process adapter, resolved late."
  nil)

(def ^:dynamic *k8s-context*
  "The kubectl context the k8s: resolver uses, or nil for the current one."
  nil)

(defn- k8s-resolve [{:keys [id ref]}]
  (let [p   (parse-k8s-id id)
        run (or *process* @(requiring-resolve 'hive-mcp.await.adapters.process/default-runner))
        {:keys [exit out timed-out?]} (b/run-process! run (k8s-argv p *k8s-context*)
                                                      {:timeout-ms resolve-timeout-ms :max-bytes 65536})]
    (when (or timed-out? (not (zero? (long exit))) (str/blank? out))
      (throw (missing ref)))
    (try (String. (.decode (Base64/getDecoder) (str/trim out)) "UTF-8")
         (catch IllegalArgumentException _ (throw (missing ref))))))

(defonce ^:private resolvers
  ;; scheme -> {:validate (fn [id] bool) :resolve (fn [ref-map] value) :addon s}
  (atom {"k8s" {:validate (comp some? parse-k8s-id) :resolve k8s-resolve :addon "core"}}))

(def known-providers
  "Who contributes the schemes core does not: named in a refusal."
  {"vault" "hive.cloak"})

(defn register-resolver!
  "Register the resolver for `scheme`: `resolve` takes {:scheme :id :ref}
   and answers the value or throws; `validate` takes the id. Answers scheme."
  [scheme {:keys [validate resolve addon]}]
  (when-not (and (string? scheme) (ifn? resolve))
    (throw (ex-info "await secret-refs: a scheme and a resolve fn are required"
                    {:error :await/invalid-resolver})))
  (swap! resolvers assoc scheme {:validate (or validate (constantly true))
                                 :resolve resolve :addon addon})
  scheme)

(defn unregister-resolver!
  [scheme]
  (vec (sort (keys (swap! resolvers dissoc scheme)))))

(defn schemes [] (vec (sort (keys @resolvers))))

(defn check-ref!
  "Throw a refusal unless `ref` is a ref of one of `allowed` schemes that
   has a registered resolver and a well-formed id. Answers the parsed ref."
  [source ref allowed]
  (let [p (parse-ref ref)]
    (when-not p
      (throw (refusal (str (name source) ": a connection must be a secret ref <scheme>:<id> ("
                           (str/join ", " (sort allowed)) "); an inline URL, DSN or password is never accepted")
                      {:source source})))
    (when-not (contains? (set allowed) (:scheme p))
      (throw (refusal (str (name source) ": secret scheme " (:scheme p) " is not accepted here; use "
                           (str/join " or " (sort allowed)))
                      {:source source :scheme (:scheme p)})))
    (let [r (get @resolvers (:scheme p))]
      (when-not r
        (throw (ex-info (str "await: secret scheme " (:scheme p) " has no resolver"
                             (when-let [a (known-providers (:scheme p))] (str "; load addon " a)))
                        {:error :await/source-adapter-missing :source source :port :secret-resolver
                         :scheme (:scheme p) :hint (some->> (known-providers (:scheme p)) (str "load addon "))})))
      (when-not ((:validate r) (:id p))
        (throw (refusal (str (name source) ": malformed " (:scheme p) ": ref")
                        {:source source :scheme (:scheme p)}))))
    p))

(defn resolve-ref
  "The value of `ref`, resolved under resolve-timeout-ms; throws
   :await/secret-missing (naming the ref, never the value) otherwise."
  [ref]
  (let [p (or (parse-ref ref) (throw (missing ref)))
        r (or (get @resolvers (:scheme p)) (throw (missing ref)))
        bindings (get-thread-bindings)
        fut (future (with-bindings bindings ((:resolve r) p)))
        v (try (.get ^java.util.concurrent.Future fut resolve-timeout-ms TimeUnit/MILLISECONDS)
               (catch TimeoutException _
                 (future-cancel fut)
                 (throw (ex-info (str "await: secret " ref " timed out")
                                 {:error :await/secret-timeout :ref ref})))
               (catch java.util.concurrent.ExecutionException _
                 (throw (missing ref))))]
    (if (and (string? v) (not (str/blank? v))) v (throw (missing ref)))))

(defrecord RegistryResolver []
  b/ISecretResolver
  (resolve-secret! [_ ref] (resolve-ref ref)))

(def default-resolver
  "The process resolver: the scheme registry."
  (->RegistryResolver))
