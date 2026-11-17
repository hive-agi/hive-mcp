(ns hive-mcp.tools.composite-late-bound-test.never-loaded
  "Fixture for composite-late-bound-test: a subdomain with a real source file,
   so a `require` on the call path WOULD load it. The test proves it does not.")

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def handlers
  {:list (fn [_] {:type "text" :text "never-loaded v1"})})
