(ns hive-mcp.tools.consolidated.workflow.ir-test
  "Facets for the HWF2 workflow verbs.

   The handlers reach their port, the hive-workflows facade, by
   requiring-resolve on `hive-workflows.mcp/<fn>`. When hive-workflows is off
   the classpath (the unit suite), this suite installs a recording stub
   namespace under that name for its own run and removes it afterwards: the
   stub answers what each facade fn was called with, or an {:error ...}
   Result for workflow id \"boom\", so every bridge arm is reached with no
   with-redefs. With the real facade present the stub is not installed and
   only the absent-independent facets run."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.tools.consolidated.workflow.ir :as ir]
            [hive-mcp.tools.core :as core]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private facade 'hive-workflows.mcp)

(defn- real-facade? []
  (some? (try (requiring-resolve 'hive-workflows.mcp/run) (catch Throwable _ nil))))

(def ^:private stubbed? (atom false))

(defn- install-stub! []
  (let [ns (create-ns facade)]
    (doseq [f '[list-workflows get-workflow describe-workflow author register run
                status cancel describe-method describe-vocabulary plan-goal]]
      (intern ns f (fn [& args]
                     (if (= "boom" (first args))
                       {:error :workflow/not-found :id "boom"}
                       {:called (name f) :args (vec args)}))))
    (reset! stubbed? true)))

(use-fixtures :once
  (fn [t]
    (if (real-facade?)
      (t)
      (do (install-stub!)
          (try (t) (finally (remove-ns facade) (reset! stubbed? false)))))))

(def ^:private Params
  [:maybe [:map
           [:workflow_id {:optional true} [:enum "wf-1" "boom"]]
           [:tags {:optional true} [:vector {:max 2} [:enum "ci" "nightly"]]]
           [:limit {:optional true} [:int {:min 1 :max 5}]]]])

(deftrifecta-from-schema handle-get-bridges-every-facade-answer
  hive-mcp.tools.consolidated.workflow.ir/handle-get
  {:in        Params
   :out       core/McpResponse
   ;; an {:error ...} Result is a loud error, anything else is JSON
   :rel       (fn [in out]
                (if @stubbed?
                  (= (boolean (:isError out)) (= "boom" (:workflow_id in)))
                  true))
   :num-tests 200})

(defn- answer [resp] (update resp :text #(str/replace % #"\s+" " ")))

(deftest-golden ir-verb-bridge-answers
  "test/golden/tools/consolidated/workflow/ir-verbs.edn"
  (if-not @stubbed?
    :real-facade-present
    {:list         (answer (ir/handle-list {:tags ["ci"] :limit 3}))
     :get          (answer (ir/handle-get {:workflow_id "wf-1"}))
     :get-error    (answer (ir/handle-get {:workflow_id "boom"}))
     :describe     (answer (ir/handle-describe {:workflow_id "wf-1"}))
     :author-map   (answer (ir/handle-author {:ast {:op :seq} :workflow_id "w" :predicates ["p"]}))
     :author-str   (answer (ir/handle-author {:ast "{:op :par}" :directory "/tmp/x"}))
     :author-noast (answer (ir/handle-author {}))
     :register     (answer (ir/handle-register {:ast {:op :seq}}))
     :run-default  (answer (ir/handle-run {:workflow_id "wf-1"}))
     :run-opts     (answer (ir/handle-run {:workflow_id "wf-1" :opts {:dry-run true}}))
     :status       (answer (ir/handle-status {:workflow_id "wf-1"}))
     :cancel       (answer (ir/handle-cancel {:workflow_id "wf-1"}))
     :method-all   (answer (ir/handle-describe-method {}))
     :method-one   (answer (ir/handle-describe-method {:method "seq"}))
     :vocab-all    (answer (ir/handle-describe-vocabulary {}))
     :vocab-one    (answer (ir/handle-describe-vocabulary {:verb "demo/scan"}))}))

(deftest absent-facade-is-loud
  (when @stubbed?
    (remove-ns facade)
    (try
      (let [r (ir/handle-status {:workflow_id "wf-1"})]
        (is (:isError r))
        (is (str/includes? (:text r) "hive-workflows not on the runtime classpath")))
      (finally (install-stub!)))))
