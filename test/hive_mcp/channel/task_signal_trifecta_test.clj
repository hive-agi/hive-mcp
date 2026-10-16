(ns hive-mcp.channel.task-signal-trifecta-test
  "Schema trifectas for task cue extraction: the allowlist, the deny list and
   the argv0 mode, over generated tool calls that reach each branch."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.channel.task-signal :as ts]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Word [:enum "carto" "drain-rank" "hive_mcp.channel" "x" "secretvalue" "ab12cd34ef56gh" "/usr/bin/git"])

(def ^:private Call
  "[tool-name args] over known, unknown and nil tool names."
  [:cat
   [:maybe [:enum "bash" "code" "memory" "BASH " "unknown" "clojure_eval"]]
   [:or
    [:map
     [:command {:optional true} [:enum "git status" "FOO=1 /usr/bin/git log" "search" ""]]
     [:query {:optional true} Word]
     [:content {:optional true} Word]
     [:tags {:optional true} [:vector {:gen/max 3} Word]]
     [:description {:optional true} Word]]
    [:enum nil "not-a-map"]]])

(defn- denied-only? [args]
  (and (map? args) (every? ts/denied-arg-keys (keys args))))

(def ^:private TokenShape [:re #"^[a-z][a-z0-9]*(?:[-_.][a-z0-9]+)*$"])

(deftrifecta-from-schema tool-call-tokens
  hive-mcp.channel.task-signal/task-tokens
  {:in Call
   :out ts/TaskTokens
   :rel (fn [[tool args] out]
          (and (<= (count out) ts/max-tokens)
               (every? #(m/validate TokenShape %) out)
               (if (or (not (map? args)) (denied-only? args)
                       (not (contains? ts/tool-arg-allowlist
                                       (some-> tool .trim .toLowerCase))))
                 (= #{} out)
                 true)
               (not (contains? out "secretvalue"))))
   :golden-path "test/golden/channel/task-signal-tokens.edn"
   :num-tests 300})

(deftest argv0-takes-the-first-shell-word-only
  (is (= #{"git"} (ts/task-tokens "bash" {:command "FOO=1 /usr/bin/git log --oneline"})))
  (is (= #{} (ts/task-tokens "bash" {:content "git status"})) "content is denied"))

(deftest cues-honour-the-switch
  (binding [ts/*enabled?* false]
    (is (false? (ts/enabled?)))
    (is (= #{} (ts/cues "memory" {:query "carto"}))))
  (binding [ts/*enabled?* true]
    (is (true? (ts/enabled?)))
    (is (= #{"carto"} (ts/cues "memory" {:query "carto"})))))

(deftest tool-args-arms
  (testing "map and non-map arguments both validate"
    (is (m/validate ts/ToolArgs {:query "x"}))
    (is (m/validate ts/ToolArgs nil))
    (is (m/validate ts/ToolArgs "str"))))
