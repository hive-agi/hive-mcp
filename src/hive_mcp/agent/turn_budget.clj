(ns hive-mcp.agent.turn-budget
  "The `turn_budget` spawn param as the lease spec hive-agent reads.

   A ling's turns are a lease (hive-agent.loop.budget). The headless backend
   reads it from `:turn-budget` on the spawn ctx
   (hive-agent.loop.spawn/build-spawn-config). This ns normalizes the MCP
   form at the hive-mcp boundary, the same vocabulary as
   hive-agent.swarm.mcp-tool/normalize-turn-budget, without requiring the
   addon: snake or kebab keys, keyword or string keys, a JSON-object string,
   numeric strings for integers, and `:judge` as a keyword."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private shape
  (str "turn_budget must be an object {initial, hard_cap?, max_extensions?, judge?, "
       "wrap_up?, judge_model?, warn_pct?, escalation_ladder?, escalate_after?, "
       "ask_timeout_ms?}"))

(defn- refuse
  [msg v]
  (throw (ex-info msg {:param "turn_budget" :value v})))

(defn- decode
  "A JSON-object string parsed; anything else as is."
  [v]
  (if (string? v)
    (try (json/read-str v)
         (catch Exception _ (refuse "turn_budget arrived as a string that is not JSON" v)))
    v))

(defn- kget
  "Value under K in M, keyword or string keyed."
  [m k]
  (let [x (get m k)]
    (if (some? x) x (get m (name k)))))

(defn- ->long
  [field v]
  (cond
    (integer? v) (long v)
    (and (string? v) (parse-long (str/trim v))) (parse-long (str/trim v))
    :else (refuse (str "turn_budget." field " must be an integer") v)))

(defn- ->kw
  [v]
  (cond
    (keyword? v) v
    (string? v) (keyword (str/replace (str/trim v) #"^:" ""))
    :else (keyword (str v))))

(defn normalize
  "Coerce a turn_budget param into the lease spec
   {:initial :hard-cap :max-extensions :judge :wrap-up? :judge-model :warn-pct
    :escalation-ladder :escalate-after :ask-timeout-ms}, absent keys omitted.
   nil -> nil (the backend's defaults). A non-object or a non-integer count
   throws ex-info naming the field."
  [v]
  (when-some [m (decode v)]
    (when-not (map? m) (refuse shape v))
    (let [pick    (fn [& ks] (some (fn [k] (let [x (kget m k)] (when (some? x) x))) ks))
          initial (pick :initial :max_turns :max-turns)
          cap     (pick :hard_cap :hard-cap)
          maxext  (pick :max_extensions :max-extensions)
          judge   (pick :judge)
          wrap    (pick :wrap_up :wrap-up :wrap_up? :wrap-up?)
          jmodel  (pick :judge_model :judge-model)
          warn    (pick :warn_pct :warn-pct)
          escaft  (pick :escalate_after :escalate-after)
          ladder  (pick :escalation_ladder :escalation-ladder)
          ask-to  (pick :ask_timeout_ms :ask-timeout-ms)]
      (when (and (some? ladder) (not (sequential? ladder)))
        (refuse "turn_budget.escalation_ladder must be an array" v))
      (cond-> {}
        (some? initial) (assoc :initial (->long "initial" initial))
        (some? cap)     (assoc :hard-cap (->long "hard_cap" cap))
        (some? maxext)  (assoc :max-extensions (->long "max_extensions" maxext))
        (some? judge)   (assoc :judge (->kw judge))
        (some? wrap)    (assoc :wrap-up? (contains? #{true "true"} wrap))
        (some? jmodel)  (assoc :judge-model (str jmodel))
        (some? warn)    (assoc :warn-pct (->long "warn_pct" warn))
        (some? escaft)  (assoc :escalate-after (->long "escalate_after" escaft))
        (some? ladder)  (assoc :escalation-ladder (mapv str ladder))
        (some? ask-to)  (assoc :ask-timeout-ms (->long "ask_timeout_ms" ask-to))))))
