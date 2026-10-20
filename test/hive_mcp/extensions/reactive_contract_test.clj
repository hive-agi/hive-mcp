(ns hive-mcp.extensions.reactive-contract-test
  "Contract facets for the reactive surface: a name outside the composite
   whitelist never rebuilds, and the listener id is stable."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.extensions.reactive :as reactive]
            [hive-mcp.extensions.registry :as ext]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema rebuild-composite-ignores-a-name-outside-the-whitelist
  #'reactive/rebuild-composite!
  {:in [:cat [:maybe [:enum "m11-not-a-composite" "memory" "kanban"]]], :out :nil, :rel (fn [[tool-name] out]
          (and (nil? out) (not (contains? reactive/composite-descriptions tool-name)))), :num-tests 50})

(defn- listening? []
  (contains? @@#'ext/contribution-listeners :reactive-surface))

(deftest install-and-uninstall-answer-the-listener-id
  (let [was (listening?)]
    (try
      (is (= :reactive-surface (reactive/install!)))
      (is (listening?))
      (is (= :reactive-surface (reactive/uninstall!)))
      (is (not (listening?)))
      (finally (when was (reactive/install!))))))

(deftest agent-tools-refresh-answers-a-report
  (let [out (reactive/refresh-agent-tools!)]
    (is (contains? #{true false} (:ok? out)))
    (is (if (:ok? out) (int? (:count out)) (contains? out :error)))))
