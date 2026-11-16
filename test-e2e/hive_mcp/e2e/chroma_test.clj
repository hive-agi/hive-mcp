(ns hive-mcp.e2e.chroma-test
  "E2E: the default-tier Chroma suites rerun against a REAL Chroma.

   Sandbox: a throwaway chromadb/chroma container on a random 127.0.0.1 port
   (hive-mcp.e2e.sandbox/with-container), removed afterwards. Never the
   operator's Chroma on :8000.

   What it proves: hive-mcp.memory-chroma-test and
   hive-mcp.memory.store.chroma-contract-runner-test pass on the in-memory
   IChromaTransport in the default tier; here the same deftests run with
   *backend* rebound to the vendor transport against the container. Equal
   results on both are the evidence that the stub is faithful to the provider
   (axiom 20260729111910-400b5763). Measured 2026-11-11 against Chroma 1.0.0:
   memory-chroma 44 tests / 93 assertions, contract runner 46 / 54, 0 failures
   on both backends."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.chroma.client :as client]
            [hive-mcp.chroma.connection :as conn]
            [hive-mcp.e2e.sandbox :as sandbox]
            [hive-mcp.memory-chroma-test :as mc]
            [hive-mcp.memory.store.chroma-contract-runner-test :as runner]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- heartbeat? [port]
  (let [c   (java.net.http.HttpClient/newHttpClient)
        req (.build (java.net.http.HttpRequest/newBuilder
                     (java.net.URI. (str "http://127.0.0.1:" port "/api/v2/heartbeat"))))]
    (= 200 (.statusCode (.send c req (java.net.http.HttpResponse$BodyHandlers/discarding))))))

(def ^:private routed-collections
  ["hive-mcp-memory" "hive-mcp-memory-1024d" "hive-mcp-memory-4096d"])

(defn- wipe!
  "Delete the routed collections so each test starts from an empty Chroma."
  []
  (doseq [n routed-collections]
    (try (when-let [c (deref (client/get-collection n) 5000 nil)]
           (deref (client/delete-collection c) 5000 nil))
         (catch Throwable _ nil))))

(defn- real-backend [port]
  (fn []
    (client/set-transport! (client/->SoftVendorTransport))
    (conn/configure! {:host "127.0.0.1" :port port :collection-name "hive-mcp-memory"})
    (wipe!)
    {:transport (client/->SoftVendorTransport)
     :config    {:host "127.0.0.1" :port port :collection-name "hive-mcp-memory"}}))

(defn- counts-of [ns-sym]
  (let [r (binding [clojure.test/*test-out* (java.io.StringWriter.)]
            (clojure.test/run-tests ns-sym))]
    (select-keys r [:test :fail :error])))

(deftest ^:e2e chroma-suites-hold-on-a-real-chroma
  (sandbox/with-container
    {:image "chromadb/chroma:latest" :container-port 8000
     :ready? heartbeat? :name-prefix "hive-e2e-chroma"}
    (fn [port]
      (let [prior     (client/transport)
            prior-cfg (select-keys (conn/get-config) [:host :port :collection-name])]
        (try
          (doseq [[ns-sym backend-var] [['hive-mcp.memory-chroma-test #'mc/*backend*]
                                        ['hive-mcp.memory.store.chroma-contract-runner-test #'runner/*backend*]]]
            (testing (str ns-sym " against Chroma on 127.0.0.1:" port)
              (let [r (with-bindings {backend-var (real-backend port)} (counts-of ns-sym))]
                (is (pos? (:test r)) "the suite ran no tests")
                (is (zero? (:fail r)) (str ns-sym " failed on a real Chroma: " r))
                (is (zero? (:error r)) (str ns-sym " errored on a real Chroma: " r)))))
          (finally
            (client/set-transport! prior)
            (conn/configure! prior-cfg)
            (conn/reset-collection-cache!)))))))
