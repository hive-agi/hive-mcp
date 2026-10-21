(ns hive-mcp.swarm.claim.span-test
  "Facets for hive-mcp.swarm.claim.span, a compat shim over
   hive-agent.swarm.claim.span.

   Two fns carry host logic of their own: context-for and assess probe the
   loaded hive-agent for the worktree-aware vars and fall back otherwise.
   Without hive-agent on the unit classpath: context-for answers nil (no
   worktree identity), and assess takes the pre-worktree path, delegating to
   conflicts, with or without a ctx. The golden pins those answers and where
   every other var delegates (see hive-mcp.test.swarm-shim); the span algebra
   itself runs in test-swarm/hive_mcp/swarm/claim/span_test.clj."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.claim.span :as span]
            [hive-mcp.test.swarm-shim :as shim]
            [hive-test.golden :refer [deftest-golden]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest recorded-without-the-addon-test
  (is (shim/addon-absent? "hive-agent.swarm.claim.span")
      "the golden below is the unit-classpath answer"))

(def ^:private ctx {:resolver (constantly nil) :cwd "/tmp"})

(deftest-golden span-shim-golden
  "test/golden/hive-mcp/swarm/claim/span-shim.edn"
  {:context-for       [(span/context-for "/tmp") (span/context-for nil)]
   :assess-no-ctx     (shim/target span/assess nil [] "a.clj" "s1" nil)
   :assess-with-ctx   (shim/target span/assess nil [] "a.clj" "s1" ctx)
   :conflicts         (shim/target span/conflicts nil [] "a.clj" "s1")
   :conflicts?        (shim/target span/conflicts? nil "a.clj" "b.clj")
   :overlap           (shim/target span/overlap nil "a.clj" "b.clj")
   :explain           (shim/target span/explain {:file "a.clj"})
   :file-span         (shim/target span/file-span "a.clj")
   :form-span         (shim/target span/form-span "a.clj" "a/b")
   :key-of            (shim/target span/key-of "a.clj")
   :span              (shim/target span/span "a.clj")
   :widen             (shim/target span/widen :body :file)
   :load-time-values  [span/Span span/modes span/mode-rank]})

(deftest span-schemas-test
  (is (m/validate span/SpanShape {:span/file "a.clj" :span/mode :file}))
  (is (m/validate span/SpanLike "a.clj") "a bare path is a file claim")
  (is (not (m/validate span/SpanShape {:span/file "a.clj" :span/mode :line})))
  (is (m/validate span/Assessment {:conflicts [] :warnings []})))
