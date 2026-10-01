(ns hive-mcp.tools.agent.spawn-editor-preflight-test
  "Emacs-bound spawn modes refuse up front, naming headless, when no Emacs answers."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.agent.spawn-mode-registry :as spawn-registry]
            [hive-mcp.tools.agent.spawn :as spawn]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- recording-probe
  "Stub reachability port answering UP?, counting its calls into CALLS."
  [up? calls]
  (fn [] (swap! calls inc) up?))

(deftest emacs-down-refuses-emacs-bound-modes-naming-headless
  (doseq [mode [:claude :vterm]]
    (let [msg (spawn/editor-preflight-refusal mode (constantly false))]
      (is (string? msg) (pr-str mode))
      (is (re-find (re-pattern (str "spawn_mode " (name mode) " ")) msg))
      (is (re-find #"spawn_mode=\"headless\"" msg)))))

(deftest emacs-up-lets-emacs-bound-modes-through
  (doseq [mode [:claude :vterm]]
    (is (nil? (spawn/editor-preflight-refusal mode (constantly true))))))

(deftest headless-modes-never-probe-emacs
  (doseq [mode [:headless :agent-sdk :no-such-mode]]
    (let [calls (atom 0)]
      (is (nil? (spawn/editor-preflight-refusal mode (recording-probe false calls))))
      (is (zero? @calls) (str (pr-str mode) " must not pay for an Emacs probe")))))

(deftest emacs-bound-modes-probe-exactly-once
  (let [calls (atom 0)]
    (spawn/editor-preflight-refusal :vterm (recording-probe true calls))
    (is (= 1 @calls))))

(deftest default-port-is-a-zero-arg-fn
  (is (fn? spawn/*editor-reachable?*)))

(defspec refusal-iff-emacs-bound-and-unreachable 100
  (prop/for-all [mode (gen/elements (vec (keys (spawn-registry/registry))))
                 up?  gen/boolean]
    (let [msg (spawn/editor-preflight-refusal mode (constantly up?))]
      (= (some? msg)
         (and (spawn-registry/requires-emacs? mode) (not up?))))))
