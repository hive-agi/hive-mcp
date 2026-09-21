;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.swarm.adapters.soft
  "Late binding for the host namespaces a swarm adapter delegates to.

   The adapters sit in the kernel (segment `swarm` in
   resources/hive-mcp/kernel.edn), while several host functions they wrap
   live in namespaces the census marks for extraction (hive-memory,
   hive-workflows, hive-observability, hive-agent). A static require from an
   adapter to one of those is a kernel -> extract edge, and it breaks the
   adapter the day the namespace leaves. So an adapter names such a function
   by symbol and resolves it on each call:

   - the namespace loads     -> the host function runs, and whatever it
                                returns or throws reaches the caller unchanged;
   - the namespace is absent -> the adapter answers what the port's own Noop
                                answers, so a kernel-only build degrades the
                                same way a process with no adapter does.

   Kernel namespaces (events.core, hooks.core, agent.context, ...) stay
   ordinary static requires.")

(def ^:dynamic *resolve*
  "How a host symbol becomes something callable. `requiring-resolve` in a
   running host. A test binds it to see an adapter on a classpath the host
   namespace has already left (answer nil), or to stand a stub in for the
   host function, without redefining any host var."
  requiring-resolve)

(defn resolve-soft
  "The var (or fn) SYM names when its namespace loads, else nil."
  [sym]
  (try (*resolve* sym) (catch Throwable _ nil)))

(defn host-or
  "Call the host function SYM with ARGS when its namespace loads; otherwise
   call FALLBACK with no arguments. Only the resolution is guarded: a throw
   from the host function propagates."
  [sym fallback & args]
  (if-let [f (resolve-soft sym)]
    (apply f args)
    (fallback)))
