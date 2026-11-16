(ns hive-mcp.agent.spawn-chain-test
  "The :spawn chain: an omitted spawn_mode resolves through it, Emacs-bound
   modes are skipped with a reason while Emacs is down, an explicit mode stays
   strict. Prompt watch and the collect default ride along."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.agent.spawn-chain :as sc]
            [hive-mcp.swarm.prompt-watch :as pw]
            [hive-mcp.tools.agent.spawn :as spawn]
            [hive-mcp.tools.swarm.collect :as collect]
            [hive-mcp.transport.registry :as reg]
            [hive-dsl.adt :refer [adt-variant]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each
  (fn [f]
    (reg/clear!)
    (reg/rebind! {:spawn {:chain [:headless :tmux :vterm]}})
    (try (f) (finally (reg/clear!)))))

(defn- env
  [registered emacs-bound emacs-up?]
  {:registered?     (fn [m] (contains? registered m))
   :requires-emacs? (fn [m] (contains? emacs-bound m))
   :emacs-up?       (constantly emacs-up?)})

(deftest unavailable-reason-arms
  (let [e (env #{:headless :vterm} #{:vterm} false)]
    (is (nil? (sc/unavailable-reason :headless e)))
    (is (= :emacs-down (sc/unavailable-reason :vterm e)))
    (is (= :unregistered (sc/unavailable-reason :tmux e)))
    (is (nil? (sc/unavailable-reason :vterm (env #{:vterm} #{:vterm} true))))))

(deftest emacs-absent-no-mode-lands-headless
  (let [m (spawn/resolve-spawn-mode nil (constantly false))]
    (is (= :headless (:mode m)))
    (is (= :primary (:resolved-by m)))))

(deftest headless-unregistered-falls-to-next-available-and-says-so
  (let [out (reg/call! :spawn {:env (env #{:vterm} #{:vterm} true)} {:sleep-fn (fn [_] nil)})
        {:keys [via resolved-by skipped]} (:ok out)]
    (is (= :vterm via))
    (is (= :fallback resolved-by))
    (is (= [:headless :tmux] (mapv :adapter skipped)))))

(deftest nothing-can-serve-names-every-skipped-mode
  (let [out (reg/call! :spawn {:env (env #{:vterm} #{:vterm} false)} {:sleep-fn (fn [_] nil)})]
    (is (contains? out :error))
    (is (= :unavailable (:error/kind out)))
    (is (seq (:skipped out)))))

(deftest explicit-mode-stays-strict
  (let [m (spawn/resolve-spawn-mode "vterm" (constantly false))]
    (is (= :vterm (:mode m)))
    (is (= :explicit (:resolved-by m)))
    (testing "the preflight then refuses it by name"
      (let [msg (spawn/editor-preflight-refusal :vterm (constantly false))]
        (is (string? msg))
        (is (re-find #"spawn_mode vterm needs a running Emacs" msg))))))

(deftest collect-defaults-to-jvm
  (is (= :strategy/jvm (adt-variant (collect/select-strategy nil (constantly true)))))
  (is (= :strategy/jvm (adt-variant (collect/select-strategy "emacs" (constantly false)))))
  (is (= :strategy/emacs (adt-variant (collect/select-strategy "emacs" (constantly true))))))

(deftest pending-prompt-reads-the-last-line
  (is (= "Allow this edit? (y/n)"
         (pw/pending-prompt ["working" "" "Allow this edit? (y/n)" ""] pw/prompt-markers)))
  (is (nil? (pw/pending-prompt ["Allow this edit? (y/n)" "done"] pw/prompt-markers)))
  (is (nil? (pw/pending-prompt [] pw/prompt-markers))))

(deftest prompt-step-shows-once-then-stalls-once
  (let [o    {:slave-id "s1" :tail ["Continue? (y/n)"]}
        s1   (pw/step nil o 1000 60000 pw/prompt-markers)
        s2   (pw/step (:state s1) o 2000 60000 pw/prompt-markers)
        s3   (pw/step (:state s2) o 62000 60000 pw/prompt-markers)
        s4   (pw/step (:state s3) o 90000 60000 pw/prompt-markers)
        s5   (pw/step (:state s4) {:slave-id "s1" :tail ["Continue? (y/n)" "ok"]} 95000 60000 pw/prompt-markers)]
    (is (= [:prompt-shown] (mapv :type (:events s1))))
    (is (empty? (:events s2)))
    (is (= [:prompt-stall] (mapv :type (:events s3))))
    (is (empty? (:events s4)))
    (is (nil? (:state s5)))))

(deftest sweep-once-emits-for-a-registered-source
  (let [emitted (atom [])]
    (pw/register-source! :test (fn [] [{:slave-id "s9" :tail ["Proceed? [y/N]"]}]))
    (try
      (is (= 1 (:observed (pw/sweep-once! {:now 1 :emit-fn #(swap! emitted conj %)}))))
      (is (= [:prompt-shown] (mapv :type @emitted)))
      (finally
        (pw/deregister-source! :test)
        ;; no source observes s9 any more, so its watch state is forgotten
        (pw/sweep-once! {:now 2 :emit-fn identity})))))
