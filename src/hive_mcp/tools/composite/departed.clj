(ns hive-mcp.tools.composite.departed
  "Dispatch across the gap a contributed subdomain leaves while its addon
   remounts.

   A remount retracts the addon's contributions, shuts the old instance down,
   then mounts the new one, which contributes again. A call whose root is
   absent from the current tree AND left it recently (hive-mcp.extensions.departures)
   waits for the root to come back and then runs on the tree that holds it.
   When it does not come back within the budget the call gets a retryable
   error naming the subdomain. A root that left long ago is answered as not
   loaded, without waiting. Every other call dispatches unchanged."
  (:require [clojure.string :as str]
            [hive-mcp.extensions.departures :as departures]
            [hive-mcp.tools.core :as core]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *return-wait*
  "How a call to a departed root waits for it: :budget-ms total wait,
   :poll-ms between looks, :reload-window-ms how recent a departure still
   counts as a reload in progress. Read per call."
  {:budget-ms 5000 :poll-ms 50 :reload-window-ms 60000})

(defn command-root
  "The first word of COMMAND (string or keyword), lower-cased, or nil."
  [command]
  (let [s (cond (keyword? command) (name command) (string? command) command)
        w (when s
            (apply str (take-while #(not (Character/isWhitespace ^char %))
                                   (str/triml s))))]
    (when-not (str/blank? w)
      (str/lower-case w))))

(defn departed-root
  "{:root r :addon id :at-ms t} when COMMAND's root is not a key of HANDLERS and
   left TOOL-NAME's contributions; nil otherwise."
  [tool-name handlers command]
  (when-let [root (command-root command)]
    (when-not (or (contains? handlers (keyword root)) (contains? handlers root))
      (some-> (departures/departure tool-name root) (assoc :root root)))))

(defn departed-error
  "The MCP error for COMMAND whose root GONE (a `departed-root` answer) is not
   in HANDLERS. RELOADING? selects the retryable reloading text over the
   not-loaded text. Both list the commands valid now."
  [command gone handlers reloading?]
  (let [{:keys [root addon at-ms]} gone
        valid (str/join ", " (sort (map name (keys handlers))))]
    (core/mcp-error
     (if reloading?
       (str "Subdomain '" root "' is reloading (addon " addon " remount in progress), so '"
            command "' is temporarily unregistered on this instance. Retryable: retry this"
            " call in a few seconds. If it persists, the subdomain is not loaded on this"
            " server. Valid commands now: " valid)
       (str "Subdomain '" root "' is not loaded on this server: it left with addon " addon
            " " (quot (- (System/currentTimeMillis) at-ms) 1000) "s ago and has not"
            " returned. Valid commands now: " valid)))))

(defn dispatch
  "Run PARAMS through RUN, called as (RUN handlers params) on the tree TREE-FN
   builds. When PARAMS' root departed TOOL-NAME within *return-wait*'s
   :reload-window-ms, TREE-FN is re-read every :poll-ms until the root is back
   (then RUN on that tree) or :budget-ms elapses (then a retryable
   `departed-error`). An older departure answers the not-loaded error at once."
  [tool-name tree-fn run params]
  (let [{:keys [budget-ms poll-ms reload-window-ms]} *return-wait*
        command  (:command params)
        handlers (tree-fn)
        gone     (departed-root tool-name handlers command)
        now      #(System/currentTimeMillis)]
    (cond
      (nil? gone)
      (run handlers params)

      (> (- (now) (:at-ms gone)) reload-window-ms)
      (departed-error command gone handlers false)

      :else
      (let [deadline (+ (now) budget-ms)]
        (loop []
          (Thread/sleep (long poll-ms))
          (let [handlers (tree-fn)
                gone'    (departed-root tool-name handlers command)]
            (cond
              (nil? gone')         (run handlers params)
              (>= (now) deadline)  (departed-error command gone' handlers true)
              :else                (recur))))))))

(def Gone
  "A departed root: its name, the addon it left with, and when."
  [:map [:root :string] [:addon :any] [:at-ms :int]])

(m/=> command-root [:=> [:cat :any] [:maybe :string]])

(m/=> departed-root [:=> [:cat :string [:maybe :map] :any] [:maybe Gone]])

(m/=> departed-error [:=> [:cat :any Gone [:maybe :map] :boolean] :map])

(m/=> dispatch [:=> [:cat :string fn? fn? :map] :any])
