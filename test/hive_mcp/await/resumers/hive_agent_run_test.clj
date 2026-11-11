(ns hive-mcp.await.resumers.hive-agent-run-test
  "The :hive-agent-run resumer runs the same port conformance cases as the
   registry's stub, with the spawn path replaced by a recording double, and
   resumes a parked run through the registry on a manual fire."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.resumer-conformance :as conformance]
            [hive-mcp.await.resumers.hive-agent-run :as har]
            [hive-mcp.await.sources.manual :as manual]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private spawns (atom []))

(def owner {:kind :hive-agent-run :run-id "run-abc" :cwd "/tmp/w" :ling-id "ling-7"
            :model "sonnet" :parent "coord-1"})

(use-fixtures :each
  (fn [t]
    (reset! spawns [])
    (reset! har/spawn-fn (fn [p] (swap! spawns conj p) {:type "text" :text "{}"}))
    (try (t) (finally (reset! har/spawn-fn nil)))))

(defn- drain-spawns!
  "Recorded spawns as resume requests (the prompt the run resumes with)."
  []
  (let [[old _] (reset-vals! spawns [])]
    (mapv (fn [p] {:prompt (get-in p [:resume :prompt]) :spawn p}) old)))

(deftest real-resumer-conforms
  (conformance/run-cases! owner drain-spawns!))

(deftest spawn-params-resume-the-chat-point
  (let [p (har/spawn-params owner "0123456789abcdef" "go")]
    (is (= {:type "ling" :name "ling-7-w-01234567" :cwd "/tmp/w" :spawn_mode "hive-agent"
            :resume {:run_id "run-abc" :prompt "go"} :model "sonnet" :parent "coord-1"}
           p))))

(deftest owner-without-run-id-is-refused
  (is (= :await/invalid-owner
         (:error (ex-data (try (ports/open-resumer {:kind :hive-agent-run :cwd "/x"})
                               (catch clojure.lang.ExceptionInfo e e)))))))

(deftest parked-run-resumes-on-fire-through-registry
  (let [dir  (.toFile (java.nio.file.Files/createTempDirectory
                       "await-har" (make-array java.nio.file.attribute.FileAttribute 0)))
        path (str (io/file dir "registry.edn"))
        reg  (registry/make-registry path)]
    (try
      (registry/init! reg)
      (registry/create! reg {:id "parked-1" :owner owner :prompt "Review the CI verdict."
                             :subscription {:type :manual :filter {:sha "abc"}}})
      (manual/fire! {:sha "zzz"} "parked-1")
      (is (empty? @spawns))
      (manual/fire! {:sha "abc" :verdict "green"} "parked-1")
      (manual/fire! {:sha "abc" :verdict "green"} "parked-1")
      (testing "exactly one successor resumes the chat point with the payload"
        (is (= 1 (count @spawns)))
        (let [p (first @spawns)]
          (is (= "run-abc" (get-in p [:resume :run_id])))
          (is (.startsWith ^String (get-in p [:resume :prompt]) "Review the CI verdict."))
          (is (.contains ^String (get-in p [:resume :prompt]) "\"green\""))))
      (is (= {:ok? true :value {:spawned "ling-7-w-parked-1" :run-id "run-abc"}}
             (registry/resume-result reg "parked-1")))
      (finally (registry/shutdown! reg)))))
