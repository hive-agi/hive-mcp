(ns hive-mcp.tools.consolidated.addon-trifecta-test
  "Trifecta (golden + property + mutation) over the addon tool's public
   handlers. The doctor port is never reached with a real addon: every case
   either has no addon_id (so it answers the boot readiness probe) or names an
   addon that is not discovered, whose report is projected to its stable
   verdict before the snapshot."
  (:require [clojure.data.json :as json]
            [clojure.test.check.generators :as gen]
            [hive-mcp.tools.consolidated.addon :as tool]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn verdict
  "Stable projection of an addon response. A readiness answer is projected to
   whether its error flag agrees with its ready? verdict (the roster itself
   depends on what else this JVM booted); a doctor report keeps its id and
   ok? verdict, which carry no timestamp."
  [resp]
  (let [body (json/read-str (:text resp) :key-fn keyword)]
    (if (contains? body :ready?)
      {:kind         :readiness
       :flag-agrees? (= (boolean (:isError resp)) (not (:ready? body)))}
      {:kind    :doctor
       :isError (boolean (:isError resp))
       :ok?     (:ok? body)
       :id      (:id body)})))

(deftrifecta handle-readiness-contract
  hive-mcp.tools.consolidated.addon/handle-readiness
  {:golden-path "test/golden/tools/consolidated/addon-readiness.edn"
   :cases       {:empty {} :nil nil :ignored-params {:addon_id "x"}}
   :xf          verdict
   :gen         (gen/elements [{} nil {:directory "/tmp"}])
   :pred        #(and (= "text" (:type %)) (string? (:text %)))
   :num-tests   20
   :mutations   [["inverted-flag" (fn [p] (update (tool/handle-readiness p) :isError not))]
                 ["doctor-shaped" (fn [_] {:type "text" :text "{\"ok?\":true}"})]]})

(deftrifecta handle-doctor-contract
  hive-mcp.tools.consolidated.addon/handle-doctor
  {:golden-path "test/golden/tools/consolidated/addon-doctor.edn"
   :cases       {:no-id          {}
                 :blank-features {:emacs_features nil}
                 :unknown-addon  {:addon_id "no.such.addon" :timeout_ms 50}
                 :kebab-id       {:addon-id "no.such.addon.kebab"}}
   :xf          verdict
   :gen         (gen/fmap (fn [id] {:addon_id (str "absent." id)}) gen/string-alphanumeric)
   :pred        #(and (= "text" (:type %)) (string? (:text %)))
   :num-tests   10
   :mutations   [["always-readiness" (fn [p] (tool/handle-readiness p))]
                 ["ignores-snake-id" (fn [p] (tool/handle-doctor (dissoc p :addon_id)))]]})
