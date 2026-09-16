(ns hive-mcp.protocols.vector
  "The named-vector-collection port, as this host sees it.

   A subsystem that owns a collection of its own (the plan index, the preset
   index) depends on THIS, never on a vendor client. The vendor is chosen once,
   at the composition root, and installed with `set-store!`.

   ## Why this namespace exists rather than a direct hive-spi require

   `hive-spi.vector.ports` is the real home of this contract and already
   declares it (hive-spi ed07f20). This host cannot require it yet: deps.edn
   pins hive-spi 1.1.1, the `:local/root` in local.deps.edn is a DEV override,
   and hive-spi releases on a quarterly cadence with a 90-day floor
   (freeze-policy.edn) because its fan-in is 14. Writing against the unreleased
   port would compile here and fail in CI.

   So this is the same contract, declared locally, with identical method names
   and arities. When hive-spi ships the port and deps.edn moves to that
   version, this namespace becomes a re-export of `hive-spi.vector.ports` and
   every caller below stays exactly as written, the way
   `hive-mcp.protocols.memory` is already a re-export of
   `hive-spi.memory.ports`.

   Reload-safety: `defprotocol` is not idempotent, so the declaration is
   guarded. That guard is CORRECT here for the reason a registration guard
   usually is not: re-running `defprotocol` does not re-register a handler, it
   mints a new interface and orphans every existing implementation.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private -ivectorcollectionstore-defined? (atom false))

(when (compare-and-set! -ivectorcollectionstore-defined? false true)
  (defprotocol IVectorCollectionStore
    "A backend holding named collections of embedded records."

    (-configure [this opts]
      "Apply backend-level configuration. Returns THIS.")

    (-get-collection [this coll-name]
      "A HANDLE for the collection named COLL-NAME, or nil when it does not
       exist.

       The handle is opaque to callers except for one guarantee: when the
       backend records collection metadata, the handle carries it under
       `:metadata`. Callers rely on that to detect an embedding-dimension
       change, so a backend that returned a bare name would silently recreate
       the collection on every read.")

    (-create-collection [this coll-name opts]
      "Create COLL-NAME and return its handle. `opts` may carry :metadata and
       :get-or-create?; with :get-or-create? true an existing collection is
       returned rather than an error.")

    (-delete-collection [this coll]
      "Delete COLL. Returns nil.")

    (-add [this coll records opts]
      "Add RECORDS to COLL. Each record is a map of :id, :embedding,
       :document and :metadata. Returns nil.")

    (-get [this coll opts]
      "Records of COLL selected by `opts` (:ids, :where, :limit), in the same
       shape `-add` takes.")

    (-query [this coll embedding opts]
      "Records of COLL nearest to EMBEDDING. `opts` may carry :n-results and
       :where. Returns records NEAREST FIRST, each carrying :distance, an
       ASCENDING distance and never a similarity.")

    (-delete [this coll opts]
      "Delete records of COLL selected by `opts` (:ids, :where). Returns nil.")

    (-update [this coll records]
      "Update RECORDS of COLL, matched by :id. Returns nil.")))

(defonce ^:private -store (atom nil))

(defn set-store!
  "Install STORE as the active vector-collection store. Returns STORE.
   Called from the composition root, which is the one place allowed to name a
   concrete backend."
  [store]
  (when-not (satisfies? IVectorCollectionStore store)
    (throw (ex-info "Not an IVectorCollectionStore"
                    {:reason ::not-a-store :got (type store)})))
  (reset! -store store)
  store)

(defn get-store
  "The active vector-collection store, or nil when none is installed."
  []
  @-store)

(defn store-set?
  "True iff a vector-collection store is installed."
  []
  (some? @-store))

(defn clear-store!
  "Remove the active store. Returns nil."
  []
  (reset! -store nil)
  nil)

(defn require-store
  "The active store, or a loud failure naming what is missing. Callers that
   cannot proceed without a backend use this instead of `get-store`, so an
   unwired host fails at the call with a reason rather than on a nil."
  []
  (or @-store
      (throw (ex-info "No vector-collection store installed"
                      {:reason ::no-store
                       :hint "the composition root installs one via protocols.vector/set-store!"}))))
