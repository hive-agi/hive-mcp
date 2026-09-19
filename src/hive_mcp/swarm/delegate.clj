(ns hive-mcp.swarm.delegate
  "Resolution for the swarm compat shims.

   The swarm lives in hive-agent, an optional addon. A shim resolves each var
   there on demand. When hive-agent is absent from the classpath, a function
   call raises ex-info with :swarm/addon-missing and a load-time value
   resolves to nil, so hive-mcp still loads and boots without the swarm."
  (:require [clojure.java.io :as io]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- ns->resource
  "Classpath resource path of namespace NS-STR's source, without extension."
  [ns-str]
  (-> ns-str (.replace "." "/") (.replace "-" "_")))

(defn available?
  "True when namespace NS-STR can be found on the classpath."
  [ns-str]
  (let [base (ns->resource ns-str)]
    (boolean (or (io/resource (str base ".clj"))
                 (io/resource (str base ".cljc"))
                 (io/resource (str base "__init.class"))))))

(defn addon-missing
  "The ex-info a shim raises when NS-STR's library is not on the classpath.
   The library is named after NS-STR's first segment (hive-agent,
   hive-datascript)."
  [ns-str sym]
  (let [lib (first (.split ^String ns-str "\\."))]
    (ex-info (str "The swarm needs " lib ", which is not on the classpath ("
                  ns-str "/" (name sym) ")")
             {:swarm/addon-missing true
              :addon lib
              :ns ns-str
              :var (name sym)})))

(defonce ^:private warned (atom #{}))

(defn- missing-var
  "Stand-in for NS-STR/SYM when its addon is absent. Deref yields nil, logged
   once per namespace; any call raises addon-missing."
  [ns-str sym]
  (proxy [clojure.lang.AFn clojure.lang.IDeref] []
    (deref []
      (when-not (contains? @warned ns-str)
        (swap! warned conj ns-str)
        (log/warn "swarm addon hive-agent absent; shim value is nil" {:ns ns-str}))
      nil)
    (throwArity [_]
      (throw (addon-missing ns-str sym)))))

(defn resolve-var
  "The var NS-STR/SYM, loading NS-STR on first use. When NS-STR is not on the
   classpath, a stand-in: deref yields nil and a call raises addon-missing."
  [ns-str sym]
  (if (available? ns-str)
    (requiring-resolve (symbol ns-str (name sym)))
    (missing-var ns-str sym)))
