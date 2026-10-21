(ns hive-mcp.server.permissions-test
  "Facets for hive-mcp.server.permissions (cov-facets f07). get-tier is pure and
   is driven from its contract with inputs built to reach every tier; escalate!
   is pinned only on its :allowed and :tier-1 arms. Its :tier-2 / :tier-3 arms
   call hivemind/ask! (a concretion, no injected port) and block for a
   decision, so they are not reachable without with-redefs: recorded as a
   missing seam."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.server.permissions :as perm]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ToolName
  [:enum "file_write" "file_edit" "clojure_edit" "bash" "magit_push"
   "swarm_kill" "memory_add" "kanban"])

(def ^:private PathArgs
  [:map-of {:gen/max 1} [:enum "file_path" "path" "file"]
   [:enum "test/foo.clj" "src/foo_test.clj" "src/core.clj" "lib/x.el"
    "elisp/hive.el" "a/generated/b.clj" "a/fixtures/c.edn" "x_mock.js"
    "doc/readme.md"]])

(def ^:private BashArgs
  [:map-of {:gen/max 1} [:= "command"]
   [:enum "ls -la" "rm foo" "psql -c 'DROP TABLE t'" "TRUNCATE x"
    "DELETE FROM t" "echo hi"]])

(def ^:private edit-tools #{"file_write" "file_edit" "clojure_edit"})
(def ^:private tier3-tools #{"bash" "magit_push" "swarm_kill"})

(defn- path-of [args] (some #(get args %) ["file_path" "path" "file"]))

(defn- test-path? [p]
  (boolean (and p (or (.endsWith ^String p "_test.clj") (.startsWith ^String p "test/")
                      (.contains ^String p "/generated/") (.contains ^String p "/fixtures/")
                      (.contains ^String p "_mock.")))))

(defn- src-path? [p]
  (boolean (and p (some #(.startsWith ^String p %) ["src/" "lib/" "elisp/"]))))

(deftrifecta-from-schema get-tier-contract perm/get-tier
  {:in  [:cat ToolName [:or :nil PathArgs BashArgs]]
   :out perm/Tier
   :rel (fn [[tool args] out]
          (let [p (path-of args)]
            (cond
              (tier3-tools tool)                       (= out :tier-3)
              (not (edit-tools tool))                  (= out :allowed)
              (test-path? p)                           (= out :tier-1)
              (src-path? p)                            (= out :tier-2)
              :else                                    (= out :allowed))))
   :num-tests 300
   :golden-path "test/golden/hive-mcp/f07-permissions-get-tier.edn"})

(deftrifecta-from-schema dangerous-tool-contract perm/dangerous-tool?
  {:in  [:cat [:or ToolName [:enum "eval_elisp" "preset_delete" "mcp_memory_cleanup_expired"]]]
   :out :boolean
   :rel (fn [[tool] out]
          (= out (contains? #{"bash" "magit_commit" "magit_push" "eval_elisp"
                              "cider_eval_explicit" "preset_delete" "swarm_kill"
                              "mcp_memory_cleanup_expired"} tool)))
   :golden-path "test/golden/hive-mcp/f07-permissions-dangerous.edn"})

(deftrifecta-from-schema requires-escalation-contract perm/requires-escalation?
  {:in  [:cat ToolName [:or :nil PathArgs]]
   :out :boolean
   :rel (fn [[tool args] out] (= out (not= :allowed (perm/get-tier tool args))))
   :golden-path "test/golden/hive-mcp/f07-permissions-requires-escalation.edn"})

(deftest-golden escalate-non-blocking-arms-golden
  "test/golden/hive-mcp/f07-permissions-escalate.edn"
  {:allowed  (perm/escalate! "ling-1" "memory_add" {"content" "x"})
   :tier-1   (perm/escalate! "ling-1" "file_edit" {"file_path" "test/a_test.clj"})
   :approved [(perm/approved? {:approved true}) (perm/approved? {:approved false})
              (perm/approved? nil)]})

(deftest bash-operations-are-tier-3
  (is (= :tier-3 (perm/get-tier "bash" {"command" "rm -rf x"})))
  (is (= :allowed (perm/get-tier "kanban" nil))))
