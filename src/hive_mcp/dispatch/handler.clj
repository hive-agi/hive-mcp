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

(defn handler?
  "Is `x` invocable as a handler?

   Invocable code, or a VAR whose current value is invocable code. The var arm
   is the whole point: it is the rebind seam, and it is what lets a reload of
   the defining namespace reach dispatch.

   The two arms ask the SAME question of `x` and of `@x`, which is the part an
   earlier draft of this function got wrong. It spelled the direct arm `fn?`
   and the var arm `ifn?`, so a bare multimethod was refused while a var
   holding that same multimethod was accepted. A predicate that disagrees with
   itself about one value depending on how it is wrapped is not a predicate,
   it is two. The test `a-plain-function-is-a-handler` is what caught it."
  [x]
  (or (code? x)
      (and (var? x) (code? (deref x)))))

(s/def ::handler handler?)

(defn current
  "The callable `x` denotes RIGHT NOW: a var's current value, or `x` itself.

   For the caller that must hold a function rather than merely something
   invocable: introspection, an identity comparison, a test double. Read it at
   CALL time and never at wiring time. Reading it early is precisely the
   value-capture this namespace exists to undo, and a `current` hoisted into a
   `let` at build time reintroduces the defect with this function's blessing."
  [x]
  (if (var? x) (deref x) x))
