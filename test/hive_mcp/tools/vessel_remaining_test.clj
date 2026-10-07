(ns hive-mcp.tools.vessel-remaining-test
  "Magit, Projectile and the legacy memory import reach the editor only
   through the closed `:vessel :dispatch` capability (card E3-VESSEL-INFRA).

   The stub is the SPI port (hive-mcp.test.stub.swarm-host), never a concrete
   Emacs client. Op names and shapes match the translators hive-emacs
   registers in hive-emacs.{magit,projectile,memory}.translators."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.test.stub.swarm-host :as sh]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.tools.projectile :as projectile]
            [hive-mcp.tools.memory.migration.import :as import]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ok {:success true :result "{}" :timed-out false})

(def ^:private legacy-export
  {:success true
   :result (json/write-str {:notes [{:id "n1" :content "a"}]
                            :snippets []
                            :conventions [{:id "c1" :content "b"}]
                            :decisions []})
   :timed-out false})

(def ^:private invocations
  "One handler invocation per converted caller, keyed by a label."
  {:magit-status   #(magit/handle-magit-status {:directory "/tmp/repo"})
   :magit-push     #(magit/handle-magit-push {:directory "/tmp/repo" :remote " github "})
   :magit-fetch    #(magit/handle-magit-fetch {:directory "/tmp/repo" :remote "  "})
   :project-info   #(projectile/handle-projectile-info {:directory "/tmp/repo"})
   :project-files  #(projectile/handle-projectile-files {:pattern ""})
   :project-search #(projectile/handle-projectile-search {:pattern "defn"})
   :legacy-import  #(import/handle-import-json {:project-id "hive" :dry-run true})})

(defn- exercise
  "Run the handler labelled LABEL against a stub vessel answering every op
   with RESPONSE (the legacy export answers its own fixture). Returns the
   handler response and the [op timeout-ms] pairs the stub received."
  [{:keys [label response]}]
  (let [out (atom nil)]
    (ms/with-stub-store
      (fn []
        (sh/with-swarm-host
          [host (fn [op _t] (if (= :memory/legacy-export (:op op)) legacy-export response))]
          (reset! out {:response ((get invocations label))
                       :calls (sh/calls host)}))))
    @out))

(deftrifecta remaining-callers-use-vessel
  exercise
  {:gen (gen/hash-map :label (gen/elements (vec (keys invocations)))
                      :response (gen/return ok))
   :pred #(and (= 1 (count (:calls %)))
               (nil? (get-in % [:response :isError])))
   :num-tests 30
   :mutations [["no-port-call" (fn [_] {:response {:type "text" :text "{}"} :calls []})]
               ["swallowed-error" (fn [_] {:response {:isError true} :calls [[{} nil]]})]]
   :assert (fn []
             (let [op-of (fn [label] (ffirst (:calls (exercise {:label label :response ok}))))]
               (is (= {:op :magit/status :directory "/tmp/repo"} (op-of :magit-status)))
               (is (= {:op :magit/push :set-upstream false :remote "github" :directory "/tmp/repo"}
                      (op-of :magit-push))
                   "remote is trimmed")
               (is (= {:op :magit/fetch :remote nil :directory "/tmp/repo"} (op-of :magit-fetch))
                   "a blank remote is absent, never the empty string the op refuses")
               (is (= {:op :project/info :directory "/tmp/repo"} (op-of :project-info)))
               (is (= {:op :project/files :pattern nil} (op-of :project-files))
                   "a blank pattern lists every file")
               (is (= {:op :project/search :pattern "defn"} (op-of :project-search)))
               (is (= {:op :memory/legacy-export :project-id "hive"} (op-of :legacy-import)))))})

(deftest projectile-info-without-a-directory-never-sends-nil
  (testing ":project/info's :directory is optional but must be non-blank when present"
    (sh/with-swarm-host [host (fn [_ _] ok)]
      (projectile/handle-projectile-info {})
      (let [[[op]] (sh/calls host)]
        (is (= :project/info (:op op)))
        (is (or (not (contains? op :directory)) (string? (:directory op))))))))

(deftest projectile-keeps-its-text-envelope
  (sh/with-swarm-host [_ (fn [_ _] {:success true :result "[\"a.clj\"]"})]
    (is (= {:type "text" :text "[\"a.clj\"]"}
           (projectile/handle-projectile-files {:pattern "clj"}))))
  (sh/with-swarm-host [_ (fn [_ _] {:success false :error "projectile not loaded"})]
    (let [r (projectile/handle-projectile-recent {})]
      (is (true? (:isError r)))
      (is (str/includes? (:text r) "projectile not loaded")))))

(deftest legacy-import-reads-the-export-and-counts-by-type
  (let [{:keys [response]} (exercise {:label :legacy-import :response ok})
        body (json/read-str (:text response) :key-fn keyword)]
    (is (= {:dry-run true :would-import 2
            :by-type {:notes 1 :snippets 0 :conventions 1 :decisions 0}}
           body))))

(deftest legacy-import-reports-a-failed-export
  (ms/with-stub-store
    (fn []
      (sh/with-swarm-host [_ (fn [_ _] {:success false :error "no vessel"})]
        (let [r (import/handle-import-json {:project-id "hive" :dry-run true})]
          (is (str/includes? (:text r) "Failed to read JSON: no vessel")))))))
