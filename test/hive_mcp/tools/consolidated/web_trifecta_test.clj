(ns hive-mcp.tools.consolidated.web-trifecta-test
  "Trifecta over web/delegate-to-standalone: the handler it returns forwards to
   a registered addon tool by name, or answers an mcp-error naming the absent
   tool. Driven through the extension registry (the port), never with-redefs."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.consolidated.web :as web]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn answer-of
  "Unary adapter: build the delegate for TOOL-NAME and call it once."
  [tool-name]
  ((web/delegate-to-standalone tool-name) {:url "https://example.invalid"}))

(deftrifecta delegate-to-standalone-contract
  hive-mcp.tools.consolidated.web-trifecta-test/answer-of
  {:golden-path "test/golden/tools/consolidated/web-delegate.edn"
   :cases       {:fetch-absent  "m27_web_fetch_absent"
                 :search-absent "m27_web_search_absent"}
   :gen         (gen/fmap #(str "m27_absent_" %) gen/string-alphanumeric)
   :pred        #(and (true? (:isError %)) (string? (:text %)))
   :num-tests   30
   :mutations   [["silent-nil" (constantly nil)]
                 ["success-shaped" (fn [n] {:type "text" :text n})]]})

(deftest delegate-forwards-to-a-registered-tool
  (let [seen (atom nil)
        tool {:name        "m27_web_probe"
              :description "probe tool for the web delegate test"
              :inputSchema {:type "object" :properties {"query" {:type "string"}}}
              :handler     (fn [p] (reset! seen p) {:type "text" :text "forwarded"})}]
    (ext/register-tool! tool)
    (try
      (is (= {:type "text" :text "forwarded"}
             ((web/delegate-to-standalone "m27_web_probe") {:query "q"})))
      (is (= {:query "q"} @seen))
      (finally (ext/deregister-tool! "m27_web_probe")))))
