(ns hive-mcp.default-tier-isolation-test
  "GATE: the default test tier reaches no live resource.

   Convention 20261111105238-0ceaa524: unit and default-suite tests run on
   stubs and fakes; a test that needs a real service is e2e, lives under
   test-e2e/ (never on the default tier's path) and builds its own sandbox.

   The UNIVERSE is every .clj file under test/, read off the disk, never a
   hand list and never the set of files that already carry some tag
   (axiom 20260813235938-4a45db9c). Each file is scanned for the markers of
   the class this gate exists for:

     - a live endpoint: localhost/127.0.0.1 on a live-service port (Chroma
       8000, Milvus 19530, Qdrant 6333/6334, NATS 4222, Ollama 11434, the
       serving hive-mcp 7910/7911), or a nats:// URL;
     - a live backend config: a read of ~/.config/hive-mcp (user.home joined
       with .config/hive-mcp) or the server's real boot of the embedding
       provider without a bound config;
     - a reachability probe that decides whether a live test runs
       (skip-unless-reachable), the pattern that let memory-chroma-test write
       into whatever Chroma answered on :8000;
     - a docker-run sandbox (that is an e2e test by definition).

   A file that carries a marker for a reason that is NOT a live connection
   (a URL string in a pure origin/config parser test, a port in a golden)
   must be in `allowed` WITH the reason. The runtime half of the gate is the
   network tripwire hive-mcp.test-runner installs."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private markers
  {:live-endpoint   #"(?:localhost|127\.0\.0\.1):(?:8000|19530|6333|6334|4222|11434|7910|7911)\b"
   :nats-url        #"nats://"
   :live-config     #"user\.home[^\n]{0,80}\.config/hive-mcp|\(slurp[^\n]{0,80}\.config/hive-mcp"
   :reachability    #"skip-unless-reachable|test-support\.chroma"
   :docker-sandbox  #"\"docker\"\s+\"run\""})

(def allowed
  "Files that match a marker for a reason that is not a live connection.
   path (relative to the repo) -> {marker reason}."
  {"test/hive_mcp/default_tier_isolation_test.clj"
   {:live-endpoint "this gate's own pattern table and docstring"
    :nats-url "this gate's own pattern table and docstring"
    :live-config "this gate's own pattern table and docstring"
    :reachability "this gate's own pattern table and docstring"}
   "test/hive_mcp/memory_chroma_test.clj"
   {:live-endpoint "docstring recounts the old :8000 probe; the suite runs on the in-memory transport"}
   "test/hive_mcp/plans_test.clj"
   {:live-endpoint "docstring explains why it installs a stub store instead of Chroma on :8000"}
   "test/hive_mcp/presets_test.clj"
   {:live-endpoint "docstring explains why it installs a stub store instead of Chroma on :8000"}
   "test/hive_mcp/vectordb/memory_store.clj"
   {:live-endpoint "docstring of the in-memory store that replaced Chroma on :8000"}
   "test/hive_mcp/agent/provider_policy_test.clj"
   {:live-endpoint "an ollama :api-url literal in a provider-policy map; the policy is pure, nothing dials it"}
   "test/hive_mcp/agent/provider_preflight_test.clj"
   {:live-endpoint "an ollama :api-url literal; preflight is exercised with stubbed probes"}
   "test/hive_mcp/embeddings/env_config_test.clj"
   {:live-endpoint "asserts the parsed default ollama host string; no connection"}
   "test/hive_mcp/embeddings/provider_options_test.clj"
   {:live-endpoint "asserts provider option maps, including the default ollama host literal; the secret port is a stub map, nothing dials it"}
   "test/hive_mcp/server/config_reload_test.clj"
   {:live-endpoint "an ollama host literal inside a config map under reload; no connection"}
   "test/hive_mcp/tools/consolidated/config_test.clj"
   {:live-endpoint "asserts a parsed config value string; disk writes are stubbed"}
   "test/hive_mcp/channel/websocket_origin_test.clj"
   {:live-endpoint "Origin header values for the origin check; the server under test binds port 0"}
   "test/hive_mcp/transport/olympus_origin_test.clj"
   {:live-endpoint "Origin header value handed to a handler fn; no socket"}
   "test/hive_mcp/protocols/event_backbone_contract_test.clj"
   {:nats-url "asserts set-backbone! REJECTS a nats:// string; nothing connects"}
   "test/hive_mcp/logback_test_config_test.clj"
   {:live-config "docstring/fixture text naming the live log path this test proves the test config avoids"}})
   {:live-endpoint "asserts the parsed default ollama host string; no connection"}
   "test/hive_mcp/embeddings/provider_options_test.clj"
   {:live-endpoint "asserts provider option maps, including the default ollama host literal; the secret port is a stub map, nothing dials it"}

(defn- test-files []
  (->> (file-seq (io/file "test"))
       (filter #(.isFile ^java.io.File %))
       (filter #(str/ends-with? (.getName ^java.io.File %) ".clj"))
       (map #(.getPath ^java.io.File %))
       sort))

(defn- hits [path]
  (let [src (slurp path)]
    (into #{} (keep (fn [[k re]] (when (re-find re src) k))) markers)))

(defn- evidence
  "The first match of each marker K in PATH, with its line number."
  [path ks]
  (let [lines (str/split-lines (slurp path))]
    (into {}
          (for [k ks
                :let [i (first (keep-indexed (fn [i l] (when (re-find (markers k) l) i)) lines))]]
            [k (if i (str "L" (inc i) ": " (str/trim (nth lines i))) "(multi-line match)")]))))

(deftest the-universe-is-the-test-tree
  (let [files (test-files)]
    (testing "non-vacuity: the walk found the test tree (wrong cwd would pass silently)"
      (is (< 500 (count files)) (str "only " (count files) " test files found under test/"))
      (is (some #{"test/hive_mcp/default_tier_isolation_test.clj"} files)))))

(deftest no-default-tier-test-reaches-a-live-resource
  (let [offenders (for [f (test-files)
                        :let [found (remove (set (keys (get allowed f))) (hits f))]
                        :when (seq found)]
                    [f (evidence f found)])]
    (is (empty? offenders)
        (str "default-tier test files that reach (or probe for) a live resource. "
             "Move each onto a stub/fake of the port, or tag it ^:e2e, move it to "
             "test-e2e/ and give it an explicit sandbox; if the match is not a "
             "connection, add the file to `allowed` with its reason: "
             (vec offenders)))))

(deftest every-allowance-is-still-needed
  (doseq [[f reasons] allowed]
    (testing f
      (is (.exists (io/file f)) "allowed file no longer exists: drop the entry")
      (doseq [k (keys reasons)]
        (is (contains? (hits f) k)
            (str f " no longer matches " k ": drop the allowance"))))))

(deftest e2e-suites-stay-off-the-default-path
  (testing "nothing under test-e2e/ is on the default tier (a namespace there must not also exist under test/)"
    (let [e2e (->> (file-seq (io/file "test-e2e"))
                   (filter #(.isFile ^java.io.File %))
                   (map #(str/replace-first (.getPath ^java.io.File %) #"^test-e2e/" "test/")))]
      (is (seq e2e) "test-e2e/ is empty: the e2e tier lost its suites")
      (is (empty? (filter #(.exists (io/file %)) e2e))
          "an e2e suite is duplicated on the default path"))))

(deftest the-gate-catches-its-class
  (testing "each marker fires on a sample of the defect it names (the gate can go red)"
    (let [sample {:live-endpoint "(java.net.URI. \"http://localhost:8000/api/v2/heartbeat\")"
                  :nats-url "(Nats/connect \"nats://127.0.0.1:4222\")"
                  :live-config "(slurp (str (System/getProperty \"user.home\") \"/.config/hive-mcp/config.edn\"))"
                  :reachability "(use-fixtures :each (test-chroma/skip-unless-reachable \"x\"))"
                  :docker-sandbox "(sh/sh \"docker\" \"run\" \"-d\" \"nats\")"}]
      (doseq [[k src] sample]
        (is (re-find (markers k) src) (str k " does not match its own sample"))))))
