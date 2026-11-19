(ns hive-mcp.tools.composite.load-error
  "What a consolidated tool serves for a subdomain whose namespace failed to
   load: a placeholder tree that names the load error, and heals itself.

   Before 2026-11-18 a failed load yielded `{}`. The domain root kept the key,
   so `project workflow catchup` answered `Unknown command: workflow` with
   `workflow` in its own Valid list, and the real cause (a missing
   hive-spi.projection.ports) was invisible.

   The placeholder is a `{:_handler f}` tree, so `cli/resolve-handler` routes
   every command under the subdomain to `f`. `f` answers the load error, and
   retries the load at most once per `retry-cooldown-ms`; once a retry yields a
   tree, every later call dispatches through it with no further retries."
  (:require [clojure.string :as str]
            [hive-mcp.tools.cli :as cli]
            [malli.core :as m]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defonce ^:private state
  ;; {sym {:error-msg str :last-attempt ms :tree healed-tree-or-nil}}
  (atom {}))

(defonce retry-cooldown-ms
  ;; Minimum ms between two load retries of one subdomain. An atom so a test
  ;; can lower it.
  (atom 30000))

(defn- subcommand
  "PARAMS with the subdomain word dropped from :command, as the healed tree
   expects: \"workflow catchup\" -> \"catchup\"."
  [params]
  (let [words (str/split (str/trim (str (:command params))) #"\s+" 2)]
    (assoc params :command (or (second words) ""))))

(defn- retry!
  "Call RESOLVE-TREE again when the cooldown for SYM has elapsed. The tree on
   success (kept in `state`), nil otherwise."
  [sym resolve-tree]
  (let [now (System/currentTimeMillis)]
    (when (>= (- now (or (get-in @state [sym :last-attempt]) 0)) @retry-cooldown-ms)
      (swap! state assoc-in [sym :last-attempt] now)
      (try
        (let [tree (resolve-tree)]
          (swap! state assoc-in [sym :tree] tree)
          tree)
        (catch Throwable e
          (log/warn e "Subdomain handlers still fail to load" {:sym sym})
          (swap! state assoc-in [sym :error-msg] (ex-message e))
          nil)))))

(defn placeholder
  "The tree to serve for SYM after its load threw E. RESOLVE-TREE is the
   zero-arg fn that loads SYM and returns its handler tree; it is retried
   lazily from the placeholder's handler. The tree carries
   :hive-mcp.tools.composite/load-error (the message) for inspectors."
  [sym resolve-tree ^Throwable e]
  (let [msg (ex-message e)]
    (log/error e "Subdomain handlers failed to load" {:sym sym})
    (swap! state assoc sym {:error-msg msg :last-attempt (System/currentTimeMillis)})
    {:_handler
     (fn load-error-handler [params]
       (if-let [tree (or (get-in @state [sym :tree]) (retry! sym resolve-tree))]
         ((cli/make-cli-handler tree) (subcommand params))
         {:type    "text"
          :isError true
          :text    (str "Subdomain " sym " failed to load: "
                        (get-in @state [sym :error-msg] msg)
                        ". Fix the cause, then reload the namespace.")}))
     :hive-mcp.tools.composite/load-error msg}))

(m/=> placeholder [:=> [:cat :symbol ifn? some?]
                    [:map [:_handler ifn?] [:hive-mcp.tools.composite/load-error [:maybe :string]]]])
