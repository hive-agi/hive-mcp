(ns hive-mcp.schema.type-token
  "Safety and shared markers for a memory TYPE TOKEN, as opposed to the
   taxonomy of types.

   The split matters, and it is the answer to \"who owns the type vocabulary\":

   - the TAXONOMY (which types exist, their abstraction level, duration,
     catchup priority) is the memory domain's, and stays in
     `hive-mcp.memory.type-registry` until it leaves with hive-memory;
   - the TOKEN's safety is the KERNEL's, because the hazards are the kernel's:
     a type string flows into vector-DB metadata and filter expressions, into
     EDN config persistence, and into `(keyword ...)` interning. Filter
     injection, keyword-intern growth and EDN key pollution are storage and
     transport hazards that exist whether or not any memory domain is mounted.

   The same reasoning covers `requested-type-of`: the `requested-type:` tag is
   a MARKER two sides agree on (the gate that parks an entry writes it, catchup
   reads it back), and a vocabulary shared between the kernel and a domain
   belongs to the kernel, since a domain may depend on the kernel and not the
   reverse.

   `hive-mcp.memory.type-registry` call-throughs to everything here, so an
   existing caller keeps its name."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Type-name sanitization (security boundary)
;; =============================================================================

(def ^:const max-type-length
  "Upper bound on a sanitized type token (chars). Bounds keyword interning
   and config growth; long enough for any legitimate type name."
  64)

(def ^:private safe-type-re
  "A safe type token: starts with a letter, then letters/digits/_/-.
   Lowercase-only by construction (sanitize-type lowercases first)."
  #"[a-z][a-z0-9_-]*")

(defn sanitize-type
  "Normalize a raw type (string or keyword) to its canonical token form:
   trimmed + lowercased. Returns nil when the input is not a non-blank
   string/keyword. Does NOT enforce the safe charset — see safe-type?."
  [t]
  (when (or (string? t) (keyword? t))
    (let [s (-> (if (keyword? t) (name t) t) str/trim str/lower-case)]
      (when-not (str/blank? s) s))))

(defn safe-type?
  "True when `t` reduces to a safe type token: non-blank, length <=
   max-type-length, matching ^[a-z][a-z0-9_-]*$ after sanitization. Rejects
   whitespace, quotes, EDN/reader chars, path separators, filter-expression
   operators, and oversized input. This is the security gate that replaced
   the old closed-enum membership check."
  [t]
  (boolean
   (when-let [s (sanitize-type t)]
     (and (<= (count s) max-type-length)
          (re-matches safe-type-re s)))))

;; =============================================================================
;; The requested-type marker
;; =============================================================================

(def requested-type-tag-prefix
  "Tag prefix carrying the type a parked entry originally asked for. Written
   by whatever gates a type, read back by catchup."
  "requested-type:")

(defn requested-type-tag
  "The marker tag for TYPE, or nil when TYPE is not a SAFE token.

   Safety, not mere sanitization: the tag goes into an entry's tags, and tags
   reach vector-DB filter expressions the same way a type does. A marker built
   out of arbitrary text would carry that text straight past the gate this
   namespace exists to be."
  [type]
  (when (safe-type? type)
    (str requested-type-tag-prefix (sanitize-type type))))

(defn requested-type-of
  "Read back the gated type a parked entry originally requested, from its
   tags. Returns nil when no requested-type marker is present."
  [tags]
  (some (fn [t]
          (let [s (str t)]
            (when (str/starts-with? s requested-type-tag-prefix)
              (subs s (count requested-type-tag-prefix)))))
        tags))
