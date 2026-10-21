(ns hive-mcp.transport.websocket.facet-test
  "Facets for hive-mcp.transport.websocket (cov-facets f07), no-server state
   only. The client set is a private atom filled by the aleph handler, and
   start-server! binds a real port and writes a lockfile under ~/.claude/ide,
   so there is no seam to inject a stub client; the connected paths stay in
   websocket_test.clj. What is pinned here is the documented degradation:
   with no clients, notify! is a nil no-op for any JSON-writable params, and
   stop-server! with nothing running answers nil."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.transport.websocket :as ws]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema notify-without-clients-contract ws/notify!
  {:in  [:cat [:enum "notifications/progress" "tools/list_changed" "x"]
         [:or :nil [:map [:n :int]] [:vector {:gen/max 2} :string]]]
   :out :nil
   :rel (fn [_ out] (and (nil? out) (not (ws/connected?))))
   ;; :out is :nil, whose only schema mutants are non-nil constants: the
   ;; facet stays on (it catches a notify! that starts answering a value).
   })

(deftest-golden websocket-no-server-golden
  "test/golden/hive-mcp/f07-websocket-no-server.edn"
  {:connected?   (ws/connected?)
   :notify       (ws/notify! "notifications/progress" {:n 1})
   :stop-twice   [(ws/stop-server!) (ws/stop-server!)]
   :connected-after (ws/connected?)})

(deftest no-clients-before-any-server
  (is (false? (ws/connected?))))
