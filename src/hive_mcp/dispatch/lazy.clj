(ns hive-mcp.dispatch.lazy
  "Resolve a handler tree by symbol, loading its namespace, and never freeze a
   failed load.

   The one path every lazy resolver of a handler tree goes through: a domain
   root folding a subdomain (hive-mcp.tools.composite/lazy-resolve-handlers)
   and the batch runners over a consolidated tool (memory, kg, swarm).

   Three outcomes:
     - the namespace loads: REALIZE applied to the resolved var.
     - the symbol's OWN namespace is absent from the classpath, or holds no
       such var: `{}`. Nothing appears there without new code.
     - the load THREW (a dependency missing, a compile error): a PLACEHOLDER
       tree. Its `:_handler` answers the failure with its cause, and its
       metadata carries ::load-error and the ::handler/refresh port. Every read
       through `hive-mcp.dispatch.handler/current` asks that port, which
       retries the load at most once per cooldown and answers the recovered
       tree once it loads, so the next call after the cause is fixed
       dispatches with no reload of the root."
  (:require [clojure.string :as str]
            [hive-mcp.dispatch.handler :as handler]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-cooldown-ms
  "Least time between two load attempts of one failed namespace."
  30000)

(defn absent-namespace?
  "Did T say that SYM's OWN namespace is not on the classpath (as opposed to a
   namespace it requires)?"
  [sym t]
  (let [path (-> (namespace sym) (str/replace "-" "_") (str/replace "." "/"))]
    (boolean (and (instance? java.io.FileNotFoundException t)
                  (str/includes? (str (ex-message t)) (str path "__init.class"))))))

(defn cause-text
  "T's class and message, followed by its root cause's when they differ."
  [t]
  (let [root (loop [c t] (if-let [n (ex-cause c)] (recur n) c))
        line (fn [x] (str (.getSimpleName (class x)) ": " (ex-message x)))]
    (if (identical? root t)
      (line t)
      (str (line t) " Caused by " (line root)))))

(defn load-error-text
  "What a call into the failed subdomain SYM answers."
  [sym msg]
  (str "Subdomain " sym " failed to load: " msg
       ". Fix the cause; a later call retries the load (no reload needed)."))

(defn load-error
  "The load-error message NODE carries, read through `handler/current`, or nil."
  [node]
  (let [cur (handler/current node)]
    (when (map? cur) (::load-error (meta cur)))))

(defn- placeholder
  [sym msg refresh]
  (with-meta
    {:_handler (fn load-error-handler [& _]
                 {:type "text" :isError true :text (load-error-text sym msg)})}
    {::load-error msg ::symbol sym ::handler/refresh refresh}))

(defn resolve-lazily
  "The handler tree SYM names, as REALIZE (applied to the resolved var) makes
   it; `{}` when SYM's namespace is absent or holds no such var; a placeholder
   that answers the failure and retries the load when loading threw.

   OPTS, all optional, are the seams a test injects:
     :require-fn   sym -> var or nil (default `requiring-resolve`)
     :now-fn       -> epoch ms (default System/currentTimeMillis)
     :cooldown-ms  least ms between two load attempts (default 30000)"
  ([sym realize] (resolve-lazily sym realize nil))
  ([sym realize {:keys [require-fn now-fn cooldown-ms]
                 :or   {require-fn  requiring-resolve
                        now-fn      #(System/currentTimeMillis)
                        cooldown-ms default-cooldown-ms}}]
   (let [attempt (fn []
                   (try {:value (some-> (require-fn sym) realize)}
                        (catch Throwable t {:error t})))
         {:keys [value error]} (attempt)]
     (cond
       (nil? error)                  (or value {})
       (absent-namespace? sym error) {}
       :else
       (let [state (atom nil)
             lock  (Object.)
             fail! (fn [refresh t]
                     (let [msg (cause-text t)
                           ph  (placeholder sym msg refresh)]
                       (log/error "subdomain failed to load; calls answer the error and retry"
                                  {:symbol sym :cause msg :retry-after-ms cooldown-ms})
                       (reset! state {:at (now-fn) :current ph})
                       ph))
             refresh (fn refresh []
                       (locking lock
                         (let [{:keys [at current recovered]} @state]
                           (cond
                             recovered recovered
                             (< (- (long (now-fn)) (long at)) (long cooldown-ms)) current
                             :else
                             (let [{v :value t :error} (attempt)]
                               (if t
                                 (fail! refresh t)
                                 (let [tree (or v {})]
                                   (log/info "subdomain loaded after an earlier failure" {:symbol sym})
                                   (swap! state assoc :recovered tree)
                                   tree)))))))]
         (fail! refresh error))))))

(m/=> absent-namespace? [:=> [:cat qualified-symbol? :any] :boolean])

(m/=> cause-text [:=> [:cat :any] :string])

(m/=> load-error-text [:=> [:cat :symbol :string] :string])

(m/=> load-error [:=> [:cat :any] [:maybe :string]])

(m/=> resolve-lazily
      [:function
       [:=> [:cat qualified-symbol? ifn?] :any]
       [:=> [:cat qualified-symbol? ifn? [:maybe :map]] :any]])
