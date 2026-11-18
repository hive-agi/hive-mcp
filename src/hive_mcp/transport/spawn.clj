(ns hive-mcp.transport.spawn
  "The :spawn TransportChain: which spawn mode a ling gets when the caller
   names none.

   Pure half:
     unavailable-reason   why a mode cannot serve right now, or nil

   Effectful half:
     resolve-mode!        run the :spawn chain of hive-mcp.transport.registry;
                          the Result names :via, :resolved-by and :skipped
                          (with the reason each skipped mode gave)

   An explicit spawn_mode never reaches this namespace: it stays strict, so a
   caller who asks for vterm with Emacs down gets an error, not a quiet
   headless ling."
  (:require [hive-dsl.result :as r]
            [hive-spi.swarm.spawn-modes :as modes]
            [hive-mcp.transport.registry :as reg]
            [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def Env
  "What availability reads: whether each mode is registered and needs Emacs,
   and whether Emacs answers (a 0-arg port)."
  [:map
   [:registered? [:=> [:cat :keyword] :boolean]]
   [:requires-emacs? [:=> [:cat :keyword] :boolean]]
   [:emacs-up? [:=> [:cat] :boolean]]])

(defn unavailable-reason
  "Why `mode` cannot serve a spawn: :unregistered when no registry knows it,
   :emacs-down when it needs Emacs and none answers, else nil. The Emacs
   probe is paid only for a registered Emacs-bound mode."
  [mode {:keys [registered? requires-emacs? emacs-up?]}]
  (cond
    (not (registered? mode))                       :unregistered
    (and (requires-emacs? mode) (not (emacs-up?))) :emacs-down
    :else                                          nil))

(m/=> unavailable-reason [:=> [:cat :keyword Env] [:maybe [:enum :unregistered :emacs-down]]])

(defn default-env
  "The live Env: the core and addon spawn-mode registry, and `emacs-up?`
   (memoised, so one resolution probes Emacs at most once)."
  [emacs-up?]
  (let [up (delay (boolean (emacs-up?)))]
    {:registered?     (fn [mode] (boolean (modes/valid-mode? mode)))
     :requires-emacs? (fn [mode] (boolean (modes/requires-emacs? mode)))
     :emacs-up?       (fn [] @up)}))

(m/=> default-env [:=> [:cat [:=> [:cat] :any]] Env])

(defn- mode-call
  "Chain call fn for `mode`: ok with the mode when it can serve under the
   op's :env, else an :unavailable error carrying the reason."
  [mode]
  (fn [{:keys [env]}]
    (if-let [why (unavailable-reason mode env)]
      (r/err :spawn/mode-unavailable
             {:error/kind :unavailable
              :mode       mode
              :reason     why
              :message    (str "spawn mode " (name mode) " unavailable: " (name why))})
      (r/ok mode))))

(defn- mode-probe
  "Probe fn for `mode`: availability against the live registry, Emacs counted
   as down, so only modes that need no Emacs recover by probe."
  [mode]
  (fn []
    ((mode-call mode) {:env (default-env (constantly false))})))

(defmethod reg/open-adapter [:spawn :headless] [_ _]
  {:call (mode-call :headless) :probe (mode-probe :headless)})

(defmethod reg/open-adapter [:spawn :tmux] [_ _]
  {:call (mode-call :tmux) :probe (mode-probe :tmux)})

(defmethod reg/open-adapter [:spawn :vterm] [_ _]
  {:call (mode-call :vterm)})

(defmethod reg/open-adapter [:spawn :claude] [_ _]
  {:call (mode-call :claude)})

(defn resolve-mode!
  "The spawn mode for a request that named none, through the :spawn chain.
   `env` is an Env. Answers a Result whose :ok is {:value mode :via mode
   :resolved-by :primary|:fallback :skipped [..]}; every skipped mode names
   its reason. No retry pause: availability does not heal in milliseconds."
  [env]
  (reg/call! :spawn {:env env} {:sleep-fn (fn [_] nil)}))

(m/=> resolve-mode! [:=> [:cat Env] :map])
