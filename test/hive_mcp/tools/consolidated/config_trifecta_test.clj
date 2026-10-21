(ns hive-mcp.tools.consolidated.config-trifecta-test
  "Trifecta (golden + property + mutation) over the READ-ONLY config handlers.
   handle-set and handle-reload write or re-read ~/.config, so they are pinned
   only on their refusal path (no key), which never touches the store."
  (:require [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.config :as cfg]
            [hive-mcp.tools.core :as tc]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn shape
  "Stable projection of a config response: error flag plus the leading text,
   which names the refusal or the key read."
  [resp]
  {:isError (boolean (:isError resp))
   :head    (subs (:text resp) 0 (min 40 (count (:text resp))))})

(deftrifecta handle-get-contract
  hive-mcp.tools.consolidated.config/handle-get
  {:golden-path "test/golden/tools/consolidated/config-get.edn"
   :cases       {:no-key {} :blank-key {:key ""} :nil nil
                 :absent-key {:key "m27.no.such.key"}}
   :xf          shape
   :gen         (gen/fmap (fn [s] {:key (str "m27.absent." s)}) gen/string-alphanumeric)
   :pred        #(m/validate tc/McpResponse %)
   :num-tests   30
   :mutations   [["never-errors" (fn [p] (dissoc (cfg/handle-get p) :isError))]
                 ["always-refuses" (fn [_] (cfg/handle-get {}))]]})

(deftrifecta handle-validate-contract
  hive-mcp.tools.consolidated.config/handle-validate
  {:golden-path "test/golden/tools/consolidated/config-validate.edn"
   :cases       {:unknown-section {:key "m27-nope"}
                 :other-unknown   {:key "zzz"}}
   :xf          shape
   :gen         (gen/fmap (fn [s] {:key (str "m27-" s)}) gen/string-alphanumeric)
   :pred        #(true? (:isError %))
   :num-tests   30
   :mutations   [["accepts-unknown" (fn [p] (dissoc (cfg/handle-validate p) :isError))]
                 ["blank-message" (fn [_] {:type "text" :text "" :isError true})]]})

(deftest refusal-paths-never-touch-the-store
  (doseq [h [cfg/handle-set cfg/handle-get]]
    (let [r (h {:key ""})]
      (is (m/validate tc/McpResponse r))
      (is (true? (:isError r)))))
  (let [p (cfg/handle-path {})]
    (is (m/validate tc/McpResponse p))
    (is (not (:isError p)))))
