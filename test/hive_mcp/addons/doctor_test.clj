(ns hive-mcp.addons.doctor-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-addon.protocol :as addon]
            [hive-dsl.result :as r]
            [hive-mcp.addons.doctor :as doctor]
            [hive-mcp.test.stub.swarm-host :as sh]
            [hive-spi.editor.services :as svc])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]
           [java.time Instant]))

(defn- fake-addon
  [id addon-type capabilities health]
  (reify addon/IAddon
    (addon-id [_] id)
    (addon-type [_] addon-type)
    (capabilities [_] capabilities)
    (initialize! [_ _] {:success? true})
    (shutdown! [_] nil)
    (tools [_] [])
    (schema-extensions [_] [])
    (health [_] health)
    (excluded-tools [_] #{})
    (hooks [_] {})))

(defn- spec
  ([] (spec #{}))
  ([features]
   (cond-> {:addon/id "hive.emacs"
            :addon/type :native
            :addon/init-ns "hive-emacs.addon"
            :addon/init-fn "addon-ctor"
            :addon/capabilities #{:tools :editor}
            :addon/dependencies #{}
            :addon/requires-capabilities #{}}
     (seq features) (assoc :addon/doctor {:emacs/features features}))))

(defn- healthy-ports
  [manifest]
  (let [instance (fake-addon "hive.emacs" :native
                             #{:tools :editor}
                             {:status :ok :details {:emacs-running? true}})]
    {:discover-fn #(hash-map :specs [manifest] :errors [])
     :resolve-constructor-fn (constantly (fn [_] instance))
     :scan-project-fn (constantly {:status :pass
                                   :evidence {:scan-errors []}})
     :get-entry-fn (fn [id]
                     (when (= id "hive.emacs")
                       {:addon instance
                        :state :active
                        :registered-at (Instant/parse "2026-07-21T00:00:00Z")
                        :init-time (Instant/parse "2026-07-21T00:00:01Z")
                        :init-result {:success? true}}))
     :now-fn #(Instant/parse "2026-07-21T00:00:02Z")}))

(defn- feature-host
  "A vessel stub answering every :editor/feature? probe with RESULT."
  [result]
  (sh/answering {:editor/feature? {:success true :result result :timed-out false}}))

(defn- stage-by-name
  [report stage-name]
  (first (filter #(= stage-name (:stage %)) (:stages report))))

(deftest healthy-doctor-report-is-complete-and-valid
  (sh/with-swarm-host [host (feature-host "t")]
    (let [manifest (spec #{"hive-mcp" "hive-mcp-cider"})
          result (doctor/run-doctor {:addon-id "hive.emacs"
                                     :directory "/tmp/hive-emacs"
                                     :timeout-ms 1500}
                                    (healthy-ports manifest))
          report (:ok result)]
      (is (r/ok? result))
      (is (doctor/report-valid? report))
      (is (:ok? report))
      (is (= 6 (count (:stages report))))
      (is (= {:pass 6 :fail 0 :skip 0} (:summary report)))
      (is (every? #(= :pass (:status %)) (:stages report)))
      (is (= ["hive-mcp" "hive-mcp-cider"]
             (get-in report [:request :emacs-features])))
      (testing "each feature is one closed :editor/feature? op with the input timeout"
        (is (= [[{:op :editor/feature? :feature "hive-mcp"} 1500]
                [{:op :editor/feature? :feature "hive-mcp-cider"} 1500]]
               (sh/calls host)))))))

(deftest findings-stay-in-a-successful-evidence-envelope
  (sh/with-swarm-host [_host (feature-host "nil")]
    (let [manifest (spec #{"hive-mcp"})
          instance (fake-addon "hive.emacs" :native #{:tools}
                               {:status :degraded})
          ports (assoc (healthy-ports manifest)
                       :get-entry-fn (constantly {:addon instance
                                                  :state :active
                                                  :init-result {:success? true}})
                       :scan-project-fn (constantly
                                         {:status :fail
                                          :evidence
                                          {:forbidden-dependencies
                                           [{:lib "io.github.hive-agi/hive-mcp"}]}}))
          result (doctor/run-doctor {:addon-id "hive.emacs"
                                     :directory "/tmp/hive-emacs"}
                                    ports)
          report (:ok result)]
      (testing "diagnostic findings are report data, not an MCP execution error"
        (is (r/ok? result))
        (is (false? (:ok? report))))
      (is (= :fail (:status (stage-by-name report :dependency-boundary))))
      (is (= :fail (:status (stage-by-name report :lifecycle-smoke))))
      (is (= :fail (:status (stage-by-name report :capability-comparison))))
      (is (= :fail (:status (stage-by-name report :emacs-features)))))))

(deftest absent-emacs-expectations-are-an-explicit-skip
  (let [manifest (spec)
        result (doctor/run-doctor {:addon-id "hive.emacs"}
                                  (healthy-ports manifest))
        report (:ok result)]
    (is (r/ok? result))
    (is (:ok? report) "skips do not make an otherwise healthy report fail")
    (is (= :skip (:status (stage-by-name report :emacs-features))))
    (is (= 1 (get-in report [:summary :skip])))))

(deftest explicit-emacs-features-override-manifest-hints
  (sh/with-swarm-host [host (feature-host "t")]
    (let [manifest (spec #{"manifest-feature"})
          result (doctor/run-doctor {:addon-id "hive.emacs"
                                     :emacs-features ["requested-feature"]}
                                    (healthy-ports manifest))]
      (is (r/ok? result))
      (is (= [[{:op :editor/feature? :feature "requested-feature"} 3000]]
             (sh/calls host))))))

(deftest no-vessel-registered-is-an-unreachable-finding
  (let [prior (get (svc/registered) sh/registry-key)]
    (svc/unregister-services! sh/registry-key)
    (try
      (let [report (:ok (doctor/run-doctor {:addon-id "hive.emacs"}
                                           (healthy-ports (spec #{"hive-mcp"}))))
            [probe] (get-in (stage-by-name report :emacs-features)
                            [:evidence :features])]
        (is (doctor/report-valid? report))
        (is (= :fail (:status (stage-by-name report :emacs-features))))
        (is (= {:feature "hive-mcp" :reachable? false :loaded? false}
               (dissoc probe :error)))
        (is (string? (:error probe)) "the unavailable envelope is the evidence"))
      (finally
        (sh/restore! prior)))))

(deftest a-vessel-without-the-editor-translators-degrades-to-a-finding
  (testing "an older vessel answers the unplannable op with a failure map"
    (sh/with-swarm-host
      [_host (sh/answering {:editor/feature? {:success false :result nil
                                              :error {:failure/reason :unsupported}
                                              :timed-out false}})]
      (let [report (:ok (doctor/run-doctor {:addon-id "hive.emacs"}
                                           (healthy-ports (spec #{"hive-mcp"}))))
            [probe] (get-in (stage-by-name report :emacs-features)
                            [:evidence :features])]
        (is (doctor/report-valid? report))
        (is (false? (:loaded? probe)))
        (testing "the vessel answered, so the finding names the missing op, not an unreachable Emacs"
          (is (true? (:reachable? probe)))
          (is (true? (:unsupported? probe))))
        (is (re-find #"unsupported" (:error probe)))))))

(deftest a-feature-name-over-the-translator-cap-is-invalid-not-unreachable
  ;; Refused either at the input boundary or as an invalid probe; what must
  ;; never happen is the name reaching the vessel and coming back "unreachable".
  (sh/with-swarm-host [host (feature-host "t")]
    (let [too-long (apply str (repeat 257 "a"))
          result (doctor/run-doctor {:addon-id "hive.emacs"
                                     :emacs-features [too-long]}
                                    (healthy-ports (spec)))]
      (when (r/ok? result)
        (let [[probe] (get-in (stage-by-name (:ok result) :emacs-features)
                              [:evidence :features])]
          (is (= "invalid Emacs feature symbol" (:error probe)))))
      (is (empty? (sh/calls host)) "an over-long name never reaches the vessel"))))

(deftest opaque-live-health-details-are-folded-to-json-safe-evidence
  (let [manifest (spec)
        instance (fake-addon "hive.emacs" :native #{:tools :editor}
                             {:status :ok :details {:opaque (Object.)}})
        ports (assoc (healthy-ports manifest)
                     :get-entry-fn
                     (constantly {:addon instance
                                  :state :active
                                  :init-result {:success? true}}))
        report (:ok (doctor/run-doctor {:addon-id "hive.emacs"} ports))]
    (is (:ok? report))
    (is (string? (get-in (stage-by-name report :lifecycle-smoke)
                         [:evidence :health :details :opaque])))))

(deftest live-functions-and-sets-fold-to-the-stable-wire-shape
  (let [manifest (spec)
        instance (fake-addon "hive.emacs" :native #{:tools :editor}
                             {:status :ok
                              :details {:callback (fn [] :called)
                                        :tags #{:b :a}}})
        ports (assoc (healthy-ports manifest)
                     :get-entry-fn
                     (constantly {:addon instance
                                  :state :active
                                  :init-result {:success? true}}))
        report (:ok (doctor/run-doctor {:addon-id "hive.emacs"} ports))
        details (get-in (stage-by-name report :lifecycle-smoke)
                        [:evidence :health :details])]
    (is (= "#fn" (:callback details)))
    (is (= [:a :b] (:tags details)))
    (is (= details
           (get-in (stage-by-name
                    (:ok (doctor/run-doctor {:addon-id "hive.emacs"} ports))
                    :lifecycle-smoke)
                   [:evidence :health :details])))))

(deftest invalid-input-is-a-domain-error
  (let [result (doctor/run-doctor {:addon-id ""
                                   :emacs-features ["bad feature"]})]
    (is (r/err? result))
    (is (= :addon-doctor/invalid-input (:error result)))))

(defn- delete-tree!
  [root]
  (doseq [f (reverse (file-seq root))]
    (.delete ^java.io.File f)))

(defn- with-temp-project
  [deps-content source-content f]
  (let [root (.toFile (Files/createTempDirectory
                       "addon-doctor-"
                       (make-array FileAttribute 0)))
        source-dir (java.io.File. root "src/demo")]
    (try
      (.mkdirs source-dir)
      (spit (java.io.File. root "deps.edn") deps-content)
      (spit (java.io.File. source-dir "addon.clj") source-content)
      (f root)
      (finally
        (delete-tree! root)))))

(deftest project-boundary-scan-proves-clean-and-host-coupled-projects
  (testing "host-neutral addon passes"
    (with-temp-project
      "{:deps {io.github.hive-agi/hive-addon {:mvn/version \"0.3.1\"}}}"
      "(ns demo.addon (:require [hive-addon.protocol :as addon]))"
      (fn [root]
        (let [result (doctor/scan-project-boundary (.getPath root))]
          (is (= :pass (:status result)))
          (is (empty? (get-in result [:evidence :scan-errors])))))))
  (testing "direct host coordinate and namespace are both evidence"
    (with-temp-project
      "{:deps {io.github.hive-agi/hive-mcp {:mvn/version \"1.0.0\"}}}"
      "(ns demo.addon (:require [hive-mcp.addons.core :as core]))"
      (fn [root]
        (let [result (doctor/scan-project-boundary (.getPath root))]
          (is (= :fail (:status result)))
          (is (= "io.github.hive-agi/hive-mcp"
                 (get-in result [:evidence :forbidden-dependencies 0 :lib])))
          (is (= "hive-mcp.addons.core"
                 (get-in result
                         [:evidence :forbidden-source-namespaces 0 :namespace]))))))))
