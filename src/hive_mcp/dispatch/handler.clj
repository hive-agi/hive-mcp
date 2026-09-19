(ns hive-mcp.dispatch.handler
  "Collect stratum, pure leaf: what counts as an INVOCABLE HANDLER.

   One fact, stated once. Before this namespace it was stated SIX times as
   `fn?`: in the tool-def spec, the hook validator, the addon wrap seam and
   three batch dispatchers. Six copies of one predicate is the smell the
   Single-Source Lever names (20260817195814-7eebeaf4), where a
   hand-maintained second copy is a latent defect that fails only on the path
   whose author forgot it.

   `fn?` was also the WRONG fact, and that is why hive-mcp core cannot be
   hot-reloaded. Capture-by-Var (20260817195749-0d407e9c) lists `MCP tool
   handlers` among its known instances: a handler folded into the tool map by
   VALUE never sees a namespace reload, because the value froze at wiring
   time. Its prescribed fix is to register by VAR (`#'handler`) so the seam
   resolves through the var at call time. A var is IFn and dereferences on
   invoke, so it is a perfectly good handler. But `fn?` is FALSE for a var, so
   every gate spelled `fn?` rejected the fix, and three of them rejected it
   SILENTLY by taking an else branch rather than throwing.

   No protocol here, deliberately. Cardinality Decides the Construct
   (20260817195936-5446b912): the members carry exactly ONE operation between
   them, invoke, and a protocol over a one-operation set is ceremony that buys
   a seam nobody needs.

   Pure and dependency-free on purpose. It sits BELOW the tool surface, the
   hook registry and the addon bridge, all of which must reach it without
   taking on each other's dependencies (ISP). That is also why it is not in
   `hive-mcp.tools.core`: that namespace is response helpers and coercion, and
   making the hook registry depend on it to borrow a predicate would buy a
   dependency that says something false about what hooks needs."
  (:require [clojure.spec.alpha :as s]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- code?
  "Is `v` invocable CODE, as opposed to a data structure that happens to be
   invocable?

   Functions and multimethods. Not keywords, maps, sets or vectors: those are
   all `ifn?` and none of them is a handler, so `ifn?` is the wrong predicate
   here even though it is the obvious one.

   Multimethods are in deliberately, and this WIDENS what the six `fn?` gates
   accepted before this namespace existed. `clojure.lang.MultiFn` does not
   implement the `Fn` marker interface, so `fn?` is false for it and a
   multimethod handler was silently refused. That is backwards for this
   codebase: Cardinality Decides the Construct (20260817195936-5446b912) says
   an OPEN command space dispatches by multimethod, so a gate that refuses one
   is refusing the house's own extension construct."
  [v]
  (or (fn? v)
      (instance? clojure.lang.MultiFn v)))

(def ^:private max-var-hops
  "How many times `current` will follow a var before giving up.

   Not a tuning knob. Real chains in this codebase are two hops (table entry
   -> alias -> function); sixteen is far past anything an author would write
   and still bounded, which is the only property that matters here."
  16)

(defn current
  "The callable `x` denotes RIGHT NOW: follow vars to a FIXED POINT, or return
   `x` itself.

   For the caller that must hold a function rather than merely something
   invocable: introspection, an identity comparison, a test double, or a
   consumer that will mis-handle a var. Read it at CALL time and never at
   wiring time. Reading it early is precisely the value-capture this namespace
   exists to undo, and a `current` hoisted into a `let` at build time
   reintroduces the defect with this function's blessing.

   FIXED POINT, not one hop. A single `deref` was enough while only dispatch
   TABLES held vars. It stops being enough the moment an alias is also a var:

     (def has-plan? #'pred/has-plan?)     ; the alias re-export
     {:has-plan? #'has-plan?}             ; the table entry

   is a var whose value is a var, and one hop hands the next reader a var it
   will mis-handle. Both seams are worth having — the alias so a reload of the
   predicate namespace reaches the alias, the table so a reload of the alias
   namespace reaches the table — so the chain is a consequence of doing the
   thing correctly twice, not an abuse to be forbidden.

   Bounded rather than trusting. A var chain is finite in every non-pathological
   case, but `(alter-var-root #'a (constantly #'a))` is expressible, and a seam
   that HANGS is worse than one that refuses: exhausting the bound returns the
   var still un-dereferenced, so `handler?` answers false and the caller's own
   gate reports an uninvocable handler instead of spinning."
  [x]
  (loop [v x, hops 0]
    (if (and (var? v) (< hops max-var-hops))
      (recur (deref v) (inc hops))
      v)))

(defn handler?
  "Is `x` invocable as a handler?

   Invocable code, or a var (or chain of vars) whose current value is invocable
   code. The var arm is the whole point: it is the rebind seam, and it is what
   lets a reload of the defining namespace reach dispatch.

   Stated THROUGH `current` rather than beside it, so the two cannot disagree
   about which values are callable. An earlier draft spelled the arms
   separately — the direct arm `fn?` and the var arm `ifn?` — so a bare
   multimethod was refused while a var holding that same multimethod was
   accepted. A predicate that disagrees with itself about one value depending
   on how it is wrapped is not a predicate, it is two. The test
   `a-plain-function-is-a-handler` is what caught it; asking `current` is what
   stops it recurring, because there is no longer a second arm to forget when
   `current` learns a new way to hold a callable."
  [x]
  (code? (current x)))

(s/def ::handler handler?)
