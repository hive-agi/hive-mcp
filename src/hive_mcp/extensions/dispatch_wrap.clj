(ns hive-mcp.extensions.dispatch-wrap
  "hive-mcp's adapter over hive-addon.hot.drain/dispatch-handler, the wrap every
   addon handler is dispatched through (composite command table and addon tool
   table). hive-mcp supplies only its own parts: the registered
   :addon/wrap-handler extension as the inner wrap, and the MCP error shape a
   call refused during an unmount answers with. With a hive-addon that predates
   the drain gate, only the inner wrap applies."
  (:require [hive-mcp.dns.result :refer [rescue]]
            [hive-mcp.extensions.registry :as ext]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *dispatcher*
  "0-arg fn answering hive-addon.hot.drain/dispatch-handler (its var), or nil
   when the hive-addon on the classpath has none. Read per call; a test binds it."
  (fn [] (rescue nil (requiring-resolve 'hive-addon.hot.drain/dispatch-handler))))

(defn refused-answer
  "Pure. The MCP error a call refused while its addon unmounts answers."
  [message _addon-id]
  {:type "text" :text message :isError true})

(defn wrap-addon-handler
  "HANDLER of addon ADDON-ID as it is dispatched."
  [addon-id handler]
  (let [inner (ext/get-extension :addon/wrap-handler)]
    (if-let [dispatch (*dispatcher*)]
      (dispatch addon-id handler {:inner inner :refused refused-answer})
      (if inner (inner addon-id handler) handler))))
