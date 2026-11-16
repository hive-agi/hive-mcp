(ns hive-mcp.swarm.prompt-watch
  "Prompt-stall detection that needs no Emacs.

   The Emacs path emits :prompt-shown and :prompt-stall on the channel bus,
   and hive-mcp.swarm.sync turns them into a hivemind forward and an urgent
   alert. This namespace emits the same two events from any other source of
   terminal output: a source is a 0-arg fn answering observations
   {:slave-id :tail [line ..] :last-output-at ms}. Shipped source: the stdout
   ring buffer of every headless ling. A tmux source is one more
   `register-source!`.

   Pure half:
     pending-prompt   the last non-blank line when it asks for an answer
     step             fold one observation into a ling's watch state, answer
                      the events to emit
   Effectful half:
     sweep-once!      observe every source, emit, keep state
     PromptWatch      the ISweepable that runs sweep-once! on the sweep
                      coordinator's cadence"
  (:require [clojure.string :as str]
            [hive-mcp.channel.core :as channel]
            [hive-mcp.protocols.lifecycle :as lifecycle]
            [hive-mcp.system.registry :as sys-reg]
            [malli.core :as m]
            [taoensso.timbre :as log]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def prompt-markers
  "Substrings that mark a line as a question waiting for an answer."
  ["(y/n)" "[y/n]" "[Y/n]" "[y/N]" "(yes/no)" "Do you want to" "Press Enter"
   "Allow this" "? (esc to cancel)"])

(def default-stall-after-ms 60000)

(def Observation
  [:map
   [:slave-id :string]
   [:tail [:vector :string]]
   [:last-output-at {:optional true} [:maybe :int]]])

(def WatchState
  [:maybe [:map [:prompt :string] [:since :int] [:stalled? :boolean]]])

(def Event
  [:map [:type [:enum :prompt-shown :prompt-stall]] [:slave-id :string]])

(defn pending-prompt
  "The last non-blank line of `tail` when it contains a prompt marker, else nil."
  [tail markers]
  (when-let [line (->> tail (map str/trim) (remove str/blank?) last)]
    (when (some #(str/includes? line %) markers) line)))

(m/=> pending-prompt [:=> [:cat [:sequential :string] [:sequential :string]] [:maybe :string]])

(defn- shown-event
  [slave-id prompt now]
  {:type :prompt-shown :slave-id slave-id :prompt prompt :timestamp now})

(defn- stall-event
  [slave-id prompt since now]
  {:type                :prompt-stall
   :slave-id            slave-id
   :idle-duration-secs  (/ (- now since) 1000.0)
   :prompt-preview      (subs prompt 0 (min 100 (count prompt)))
   :urgency             "high"})

(defn step
  "Fold one observation into `prev` (a WatchState). Answers {:state :events}.
   A prompt that appears emits :prompt-shown once; the same prompt still
   unanswered `stall-after-ms` later emits :prompt-stall once; new output
   below the prompt clears the state."
  [prev {:keys [slave-id tail]} now stall-after-ms markers]
  (let [prompt (pending-prompt tail markers)]
    (cond
      (nil? prompt)
      {:state nil :events []}

      (and prev (= prompt (:prompt prev)))
      (if (and (not (:stalled? prev)) (>= (- now (:since prev)) stall-after-ms))
        {:state  (assoc prev :stalled? true)
         :events [(stall-event slave-id prompt (:since prev) now)]}
        {:state prev :events []})

      :else
      {:state  {:prompt prompt :since now :stalled? false}
       :events [(shown-event slave-id prompt now)]})))

(m/=> step [:=> [:cat WatchState Observation :int :int [:sequential :string]]
            [:map [:state WatchState] [:events [:vector Event]]]])

(defonce ^:private sources (atom {}))
(defonce ^:private watch-states (atom {}))

(defn register-source!
  "Register `f` (0-arg, answers [Observation ..]) under `id`. Answers `id`."
  [id f]
  (swap! sources assoc id f)
  id)

(m/=> register-source! [:=> [:cat :keyword ifn?] :keyword])

(defn deregister-source!
  "Remove the source `id`. Idempotent."
  [id]
  (swap! sources dissoc id)
  nil)

(m/=> deregister-source! [:=> [:cat :keyword] :nil])

(defn headless-observations
  "Observations of every headless ling: the tail of its stdout ring buffer."
  []
  (let [list-fn (requiring-resolve 'hive-mcp.agent.headless/list-headless)
        buf-fn  (requiring-resolve 'hive-mcp.agent.headless/get-stdout-buffer)]
    (vec (for [{:keys [ling-id alive?]} (list-fn)
               :when alive?
               :let [buf   (buf-fn ling-id)
                     state (when buf @buf)]
               :when state]
           {:slave-id       ling-id
            :tail           (vec (take-last 5 (map str (:lines state))))
            :last-output-at (last (:timestamps state))}))))

(m/=> headless-observations [:=> [:cat] [:vector Observation]])

(defn- emit!
  [{:keys [type] :as event}]
  (channel/emit-event! type (dissoc event :type)))

(defn- observe-all
  [source-fns]
  (vec (mapcat (fn [f]
                 (try (f)
                      (catch Exception e
                        (log/warn "prompt-watch source failed" {:error (ex-message e)})
                        [])))
               source-fns)))

(defn sweep-once!
  "Observe every registered source once, emit the events `step` answers and
   forget lings no longer observed. opts: :now (ms), :stall-after-ms,
   :markers, :emit! (tests). Answers {:observed n :emitted [Event ..]}."
  ([] (sweep-once! nil))
  ([{:keys [now stall-after-ms markers emit-fn]
     :or   {stall-after-ms default-stall-after-ms
            markers        prompt-markers
            emit-fn        emit!}}]
   (let [now (or now (System/currentTimeMillis))
         obs (observe-all (vals @sources))
         out (reduce (fn [acc o]
                       (let [{:keys [state events]}
                             (step (get @watch-states (:slave-id o)) o now stall-after-ms markers)]
                         (-> acc
                             (assoc-in [:states (:slave-id o)] state)
                             (update :events into events))))
                     {:states {} :events []}
                     obs)]
     (reset! watch-states (into {} (filter val) (:states out)))
     (run! emit-fn (:events out))
     {:observed (count obs) :emitted (:events out)})))

(m/=> sweep-once! [:function
                   [:=> [:cat] [:map [:observed :int] [:emitted [:vector Event]]]]
                   [:=> [:cat [:maybe :map]] [:map [:observed :int] [:emitted [:vector Event]]]]])

(defn install!
  "Register the shipped headless source. Idempotent."
  []
  (register-source! :headless headless-observations))

(m/=> install! [:=> [:cat] :keyword])

(defrecord PromptWatch []
  lifecycle/ISweepable
  (sweep-interval-s [_] 15)
  (sweep-name [_] "swarm/prompt-watch")
  (sweep! [_ _ctx]
    (let [{:keys [observed emitted]} (sweep-once!)]
      {:swept observed :errors [] :emitted (count emitted)})))

(defonce ^:private -registered?
  (do (install!)
      (sys-reg/register-sweep! (->PromptWatch))
      true))
