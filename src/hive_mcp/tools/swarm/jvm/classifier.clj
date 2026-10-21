(ns hive-mcp.tools.swarm.jvm.classifier
  "JVM process discovery, classification, and swarm environment detection.

   Compat shim: moved to hive-agent.swarm.tools.jvm.classifier in hive-agent. Every public var delegates there
   through requiring-resolve; hive-mcp does not compile-depend on hive-agent."
  (:require [hive-mcp.swarm.delegate :as delegate]
            [malli.core :as m]
            [malli.util :as mu]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- impl [sym]
  (delegate/resolve-var "hive-agent.swarm.tools.jvm.classifier" sym))

(defn- impl!
  "impl, after making sure the host adapters fill the swarm port slots.
   The adapters are resolved late: a static require would close a load
   cycle through the events adapter. A failure here (for instance a call
   made while that cycle is still loading) leaves the ports on their
   noops until a later call installs them."
  [sym]
  (try ((requiring-resolve 'hive-mcp.swarm.adapters.boot/ensure!))
       (catch Throwable _ nil))
  (impl sym))

;; Value schemas for the shim contracts. They transcribe the docstrings and
;; bodies of hive-agent.swarm.tools.jvm.classifier, where the behaviour lives.

(def JvmType
  "The process type `classify-type` reads off a command line."
  [:enum :shadow-cljs :hive-mcp :clojure-mcp :nrepl :leiningen :other])

(def Pid
  "A process id as ps prints it, or as a caller holds it."
  [:or :string :int])

(def ProcessRow
  "One `ps -eo pid,ppid,pcpu,pmem,etime,args` line, parsed."
  [:map
   [:pid :string]
   [:ppid :string]
   [:cpu :string]
   [:mem :string]
   [:etime :string]
   [:cmd :string]])

(def ParentInfo
  "What `get-all-process-parents` knows about one pid."
  [:map [:ppid :string] [:comm :string]])

(def ParentMap
  "pid -> its parent pid and command name."
  [:map-of :string ParentInfo])

(def SwarmInfo
  "The swarm environment read from /proc/<pid>/environ. Each key is nil when
   its variable is unset; the map itself is nil when all three are."
  [:map
   [:swarm-slave-id [:maybe :string]]
   [:swarm-master-id [:maybe :string]]
   [:swarm-depth [:maybe :int]]])

(def ClassifiableProcess
  "What `classify-jvm-process` reads: a command line to classify and a pid
   to look the swarm environment up by. Other keys pass through."
  [:map [:cmd :string] [:pid {:optional true} [:maybe Pid]]])

(def ClassifiedProcess
  "A process with its type and swarm status merged in."
  [:map
   [:type JvmType]
   [:swarm-spawned :boolean]
   [:swarm-slave-id {:optional true} [:maybe :string]]
   [:swarm-master-id {:optional true} [:maybe :string]]
   [:swarm-depth {:optional true} [:maybe :int]]])

(def ClassifiedRow
  "A discovered process row with its classification merged in."
  (mu/merge ProcessRow ClassifiedProcess))

(defn classify-jvm-process
  {:arglists '([{:keys [cmd pid], :as proc}])}
  [& args]
  (apply (impl! 'classify-jvm-process) args))

(m/=> classify-jvm-process [:=> [:cat ClassifiableProcess] ClassifiedProcess])

(defn classify-type
  {:arglists '([cmd])}
  [& args]
  (apply (impl! 'classify-type) args))

(m/=> classify-type [:=> [:cat :string] JvmType])

(defn discover-and-classify
  {:arglists '([])}
  [& args]
  (apply (impl! 'discover-and-classify) args))

(m/=> discover-and-classify
      [:=> [:cat]
       [:map
        [:processes [:sequential ClassifiedRow]]
        [:by-type [:map-of JvmType [:vector ClassifiedRow]]]
        [:parents ParentMap]]])

(defn find-jvm-processes
  {:arglists '([])}
  [& args]
  (apply (impl! 'find-jvm-processes) args))

(m/=> find-jvm-processes [:=> [:cat] [:sequential ProcessRow]])

(defn get-all-process-parents
  {:arglists '([])}
  [& args]
  (apply (impl! 'get-all-process-parents) args))

(m/=> get-all-process-parents [:=> [:cat] ParentMap])

(defn get-process-swarm-info
  {:arglists '([pid])}
  [& args]
  (apply (impl! 'get-process-swarm-info) args))

(m/=> get-process-swarm-info [:=> [:cat [:maybe Pid]] [:maybe SwarmInfo]])
