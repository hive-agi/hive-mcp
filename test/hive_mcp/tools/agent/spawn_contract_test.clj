(ns hive-mcp.tools.agent.spawn-contract-test
  "Contract facets for the spawn handler's pure readers: spawn-brief (task,
   else prompt, blank as absent, a disagreement refused), effective-parent
   (explicit parent, else the caller unless it is a sessionless coordinator
   lane), sandbox-decision (fails closed) and chat-point-refusal. A golden
   pins loop-opts, turn-budget-opt and the type refusal of handle-spawn."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.tools.agent.spawn :as sp]
            [hive-mcp.tools.agent.helpers :as h]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Brief [:enum "" " " "fix the bug" "write tests"])

(defn- refused
  "Run f; an ex-info is :refused (spawn-brief's disagreement)."
  [f]
  (try (f) (catch clojure.lang.ExceptionInfo _ :refused)))

;; Inputs never disagree (prompt is blank, absent or equal to task), so the
;; contracted var answers instead of throwing; the refusal is pinned in
;; spawn-readers-golden.
(deftrifecta-from-schema spawn-brief-prefers-task-and-refuses-disagreement
  #'sp/spawn-brief
  {:in [:cat [:map {:gen/fmap (fn [{:keys [task prompt] :as m}]
                                (if (and (not (str/blank? task)) (not (str/blank? prompt)))
                                  (assoc m :prompt task)
                                  m))}
              [:task {:optional true} Brief] [:prompt {:optional true} Brief]]]
   :out [:maybe :string]
   :rel (fn [[{:keys [task prompt]}] out]
          (= (or (when-not (str/blank? task) task)
                 (when-not (str/blank? prompt) prompt))
             out))
   :classify (fn [[{:keys [task]}] out]
               (cond (nil? out) :none (= task out) :task :else :prompt))
   :classify-domain #{:none :task :prompt}
   :classify-floor 10
   :golden-path "test/golden/hive-mcp/tools/agent/spawn-brief.edn"
   :num-tests 150})

(deftrifecta-from-schema effective-parent-follows-the-caller-lanes
  #'sp/effective-parent
  {:in [:cat [:map
              [:parent {:optional true} [:enum "" "  " "ling-p"]]
              [:_caller_id {:optional true} [:enum "coordinator" "coordinator-hive" "coordinator:s1" "ling-c"]]]]
   :out [:maybe :string]
   :rel (fn [[{:keys [parent _caller_id]}] out]
          (cond (not (str/blank? parent))       (= parent out)
                (= "coordinator:s1" _caller_id) (= _caller_id out)
                (= "ling-c" _caller_id)         (= _caller_id out)
                :else                           (nil? out)))
   :classify (fn [_ out] (if out :attributed :root))
   :classify-domain #{:attributed :root}
   :golden-path "test/golden/hive-mcp/tools/agent/spawn-effective-parent.edn"
   :num-tests 120})

(deftrifecta-from-schema chat-point-refusal-names-what-would-be-dropped
  #'sp/chat-point-refusal
  {:in [:cat [:enum :claude :headless :hive-agent]
        [:map {:gen/fmap (fn [m] (if (or (:resume m) (:chat-run-id m)) m (assoc m :resume {:run-id "r"})))}
         [:resume {:optional true} [:map [:run-id [:enum "r"]]]]
         [:chat-run-id {:optional true} [:enum "c"]]
         [:llm-retries {:optional true} [:enum 1]]]
        [:or [:= #{}] [:= #{:sandbox}] [:= #{:chat-points}] [:= #{:chat-points :sandbox}]]]
   :out [:maybe :string]
   :rel (fn [[_ opts caps] out]
          (if (and (or (contains? opts :resume) (contains? opts :chat-run-id))
                   (not (contains? caps :chat-points)))
            (and (string? out)
                 (= (contains? opts :resume) (str/includes? out "resume"))
                 (= (contains? opts :chat-run-id) (str/includes? out "chat_run_id")))
            (nil? out)))
   :classify (fn [_ out] (if out :refused :admitted))
   :classify-domain #{:refused :admitted}
   :golden-path "test/golden/hive-mcp/tools/agent/spawn-chat-point-refusal.edn"
   :num-tests 120})

(golden/deftest-golden spawn-readers-golden
  "test/golden/hive-mcp/tools/agent/spawn-readers.edn"
  {:brief-refused (refused #(sp/spawn-brief {:task "a" :prompt "b"}))
   :sandbox       (mapv sp/sandbox-decision [nil true false "true" "false" "darkmatter" {:backend "x"}])
   :normalize-sb  [(sp/normalize-sandbox "true") (refused #(sp/normalize-sandbox "darkmatter"))]
   :loop-opts     (sp/loop-opts {:llm_retries 2 :chat_run_id "c" :resume {:run_id "r" :at {:seq 1}}} nil)
   :loop-bad      (refused #(sp/loop-opts {:llm_retries -1} nil))
   :resume        [(sp/normalize-resume nil) (sp/normalize-resume {:run_id "r" :prompt "p"})]
   :turn-budget   [(sp/turn-budget-opt identity {:initial 3}) (sp/turn-budget-opt nil nil)
                   (refused #(sp/turn-budget-opt nil {:initial 3}))]
   :editor        [(sp/editor-preflight-refusal :headless (constantly false))
                   (some? (sp/editor-preflight-refusal :vterm (constantly false)))]
   :provider      (sp/provider-preflight-refusal :claude :anthropic)
   :grant-share   (sp/spawn-grant nil nil)
   :bad-type      (sp/handle-spawn {:type "nope"})})

(deftest ports-are-callable
  (is (m/validate [:maybe h/Port] (sp/turn-budget-normalizer)))
  (is (ifn? sp/*editor-reachable?*))
  (is (ifn? sp/*get-slave*)))
