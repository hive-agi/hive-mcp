(ns hive-mcp.hot.self
  "Collect stratum: which of hive-mcp's OWN namespaces must never be reloaded.

   The hot-reload watcher has always watched core's `src` and has always
   called `routes/refresh-tools!` when a reload succeeds. What it never
   supplied was the protocol interlock, and `init-with-watcher!` has accepted
   a `:no-reload` set the whole time. This namespace computes that set.

   Reloading a namespace that defines a protocol orphans every `reify` and
   `defrecord` instance built against the OLD protocol object: `satisfies?`
   answers false for a class that plainly implements it, and the failure reads
   as a defect in the implementation rather than as a reload artifact (axiom
   20260822010805-57856ae1). Protecting the DEFINING namespaces is the whole
   interlock, because it keeps the protocol object stable while the
   implementors around it reload freely.

   DERIVED, never listed. Thirty-seven core namespaces define protocols today
   and a hand-written list of them is precisely the hand-maintained copy the
   Single-Source Lever forbids (20260817195814-7eebeaf4): it would be correct
   on the day it was written and silently wrong the first time somebody adds,
   moves or deletes a protocol."
  (:require [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-prefix
  "Namespaces this process owns and may therefore reload. Addons reach
   hive-hot through their own plan and are not this namespace's business."
  "hive-mcp.")

(defn- protocol-method?
  "Does `v` look like a method of a `defprotocol`?

   Read off METADATA and never by dereferencing. A var may hold a delay, a
   future or a promise, and `deref` would force, block on, or realize it as a
   side effect of asking a question about the namespace. `defprotocol` stamps
   `:protocol` on each method var it interns, which answers the same question
   for free."
  [v]
  (boolean (:protocol (meta v))))

(defn defines-protocol?
  "Does `ns-sym` intern the methods of a protocol it defines?

   `extend-protocol` and `extend-type` in some OTHER namespace intern nothing,
   so an implementor is correctly not counted: implementors are exactly what
   this interlock leaves free to reload."
  [ns-sym]
  (boolean (some protocol-method? (vals (ns-publics ns-sym)))))

(defn protocol-namespaces
  "The LOADED namespaces under `prefix` that define a protocol.

   Computed from the live image rather than from the source tree, which makes
   it exact for everything loaded and blind to everything not. In practice the
   watcher starts after the server has built its tool surface, so the
   namespaces that matter are loaded by then.

   The gap is real and worth naming: a protocol namespace loaded LAZILY after
   the watcher started is not in this set and is therefore reloadable, with
   the orphaning that implies. Re-running the watcher init recomputes it. A
   source scan would close the gap at the cost of parsing every ns form to map
   file to namespace, which is a trade worth making only once the gap is
   observed rather than imagined."
  ([] (protocol-namespaces default-prefix))
  ([prefix]
   (into #{}
         (comp (map ns-name)
               (filter #(str/starts-with? (str %) prefix))
               (filter defines-protocol?))
         (all-ns))))
