(ns hive-mcp.agent.identity-test
  "Boundary tests for verified spawn identity that need no hive-agent addon:
   env-extra cannot override the identity vars, key custody, the signer
   adapter, and the request boundary stripping the credential before any
   handler sees it. The policy itself (observe/enforce over a real
   credential) is covered in test-swarm, where the hive-agent domain loads."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string :as str]
            [hive-mcp.agent.identity :as agent-identity]
            [hive-mcp.agent.headless :as headless]
            [hive-mcp.context.request :as ctx]
            [hive-mcp.server.routes.middleware :as mw]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-key-file []
  (let [dir (.toFile (java.nio.file.Files/createTempDirectory
                      "hive-identity-test" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (java.io.File. dir "state/hive-mcp/identity/key.edn")))

;; =============================================================================
;; env-extra cannot override the identity vars
;; =============================================================================

(deftest child-env-reserves-identity-vars
  (testing "string and keyword keys alike are dropped and set by the server"
    (doseq [extra [{"CLAUDE_SWARM_SLAVE_ID" "evil" "HIVE_AGENT_CREDENTIAL" "forged" "X" "1"}
                   {:CLAUDE_SWARM_SLAVE_ID "evil" :HIVE_AGENT_CREDENTIAL "forged" :X 1}]]
      (let [env (agent-identity/child-env extra "ling-a" "real-token")]
        (is (= "ling-a" (get env "CLAUDE_SWARM_SLAVE_ID")))
        (is (= "real-token" (get env "HIVE_AGENT_CREDENTIAL")))
        (is (= "1" (get env "X")) "other env-extra entries pass through")))))

(deftest build-child-env-cannot-be-overridden-by-env-extra
  (let [env (headless/build-child-env
             "ling-a" {:cwd "/tmp"
                       :env-extra {"CLAUDE_SWARM_SLAVE_ID" "coordinator"
                                   "HIVE_AGENT_CREDENTIAL" "forged"
                                   "SOMETHING" "kept"}
                       :credential "server-minted"})]
    (is (= "ling-a" (get env "CLAUDE_SWARM_SLAVE_ID")))
    (is (= "server-minted" (get env "HIVE_AGENT_CREDENTIAL")))
    (is (= "kept" (get env "SOMETHING"))))
  (testing "with no credential minted, the slave id is still the server's"
    (let [env (headless/build-child-env "ling-b" {:env-extra {:CLAUDE_SWARM_SLAVE_ID "x"}})]
      (is (= "ling-b" (get env "CLAUDE_SWARM_SLAVE_ID")))
      (is (not (contains? env "HIVE_AGENT_CREDENTIAL"))))))

;; =============================================================================
;; Key custody and signer
;; =============================================================================

(deftest key-is-persisted-0600-and-reused
  (let [f  (temp-key-file)
        k1 (agent-identity/load-or-create-key! f)
        k2 (agent-identity/load-or-create-key! f)
        perms (java.nio.file.Files/getPosixFilePermissions
               (.toPath f) (make-array java.nio.file.LinkOption 0))]
    (is (.exists f))
    (is (= "rw-------" (java.nio.file.attribute.PosixFilePermissions/toString perms)))
    (is (= (:kid k1) (:kid k2)) "a restart reads the same key")
    (is (java.util.Arrays/equals ^bytes (:public k1) ^bytes (:public k2)))
    (testing "a key file others can read is refused"
      (java.nio.file.Files/setPosixFilePermissions
       (.toPath f) (java.nio.file.attribute.PosixFilePermissions/fromString "rw-r--r--"))
      (is (thrown? clojure.lang.ExceptionInfo (agent-identity/load-or-create-key! f))))))

(deftest signer-port-round-trips-through-hive-system
  (let [s   (agent-identity/->signer (agent-identity/load-or-create-key! (temp-key-file)))
        msg (.getBytes "payload" "UTF-8")
        sig ((:sign s) msg)]
    (is (true? ((:verify s) msg sig)))
    (is (false? ((:verify s) (.getBytes "payloaD" "UTF-8") sig)))
    (is (false? ((:verify (agent-identity/->signer (agent-identity/load-or-create-key! (temp-key-file))))
                 msg sig))
        "another key does not verify")))

(deftest parse-mode-defaults-to-observe
  (is (= :enforce (agent-identity/parse-mode :enforce)))
  (is (= :enforce (agent-identity/parse-mode ":enforce")))
  (doseq [v [nil :observe "observe" :bogus 42]]
    (is (= :observe (agent-identity/parse-mode v)))))

;; =============================================================================
;; Request boundary: the credential is stripped before any handler
;; =============================================================================

(def ^:private secret "hac1.SECRET-PAYLOAD.SECRET-SIG")

(deftest credential-is-stripped-before-handlers
  (let [seen (atom nil)
        handler (fn [args]
                  (reset! seen {:args args :identity (ctx/current-identity)})
                  (str "echo " (pr-str args)))
        chain (mw/build-middleware-chain handler "probe" nil)]
    (binding [mw/*identity-get-slave* (constantly nil)
              mw/*grant-get-slave* (constantly nil)]
      (doseq [k ["_caller_credential" :_caller_credential]]
        (let [resp (chain {k secret "_caller_id" "ling-a:i1" "command" "x"})
              text (str/join "\n" (keep :text (:content resp)))]
          (is (not (contains? (:args @seen) :_caller_credential)))
          (is (not (contains? (:args @seen) "_caller_credential")))
          (is (not (str/includes? (pr-str @seen) "SECRET")) "the handler never holds it")
          (is (not (str/includes? text "SECRET")) "nor does the response")
          (is (= "ling-a:i1" (:_caller_id (:args @seen))) "the caller id is untouched"))))))

(deftest unverifiable-credential-is-not-identity
  (let [seen (atom nil)
        chain (mw/build-middleware-chain (fn [_] (reset! seen (ctx/current-identity)) "ok")
                                         "probe" nil)]
    (binding [mw/*identity-get-slave* (constantly nil)
              mw/*grant-get-slave* (constantly nil)
              agent-identity/*key-file* temp-key-file]
      (chain {"_caller_credential" secret "_caller_id" "ling-a:i1"})
      (is (false? (:verified? @seen)))
      (is (= "ling-a:i1" (ctx/session-agent-id nil (:caller-id @seen)))))))

(deftest modes-through-the-middleware-with-a-real-credential
  ;; Needs the hive-agent credential domain on the classpath (soft-resolved);
  ;; without it there is nothing to mint and the test says so.
  (if-not (agent-identity/domain)
    (is true "hive-agent.identity.credential not on the classpath; mode test skipped")
    (let [kf    (temp-key-file)
          rows  {"ling-a" {:slave/id "ling-a" :slave/parent "coordinator:s1" :slave/depth 1}}
          seen  (atom nil)
          chain (mw/build-middleware-chain
                 (fn [_] (reset! seen {:identity (ctx/current-identity)
                                       :caller (ctx/current-caller-id)})
                   "ran")
                 "probe" nil)
          call  (fn [mode args]
                  (reset! seen nil)
                  (binding [mw/*identity-get-slave* rows
                            mw/*grant-get-slave* rows
                            agent-identity/*key-file* (constantly kf)
                            agent-identity/*mode* (constantly mode)]
                    (let [resp (chain args)]
                      {:ran? (some? @seen)
                       :seen @seen
                       :text (str/join "\n" (keep :text (:content resp)))})))
          token (binding [agent-identity/*key-file* (constantly kf)]
                  (agent-identity/mint-for-child {:agent-id "ling-a" :parent-id "coordinator:s1"
                                                  :depth 1 :grant nil}))
          before (get (agent-identity/anomaly-counts) :missing 0)]
      (is (string? token))
      (testing ":observe - a spawned agent's id with no credential runs, counted"
        (let [r (call :observe {"_caller_id" "ling-a:i1"})]
          (is (:ran? r))
          (is (false? (get-in r [:seen :identity :verified?])))
          (is (= (inc before) (get (agent-identity/anomaly-counts) :missing 0)))))
      (testing ":enforce - the same request is refused"
        (let [r (call :enforce {"_caller_id" "ling-a:i1"})]
          (is (not (:ran? r)))
          (is (str/includes? (:text r) "REFUSED by identity"))))
      (testing ":enforce - a valid credential for that id runs, verified"
        (let [r (call :enforce {"_caller_id" "ling-a:i1" "_caller_credential" token})]
          (is (:ran? r))
          (is (true? (get-in r [:seen :identity :verified?])))
          (is (= "ling-a:i1" (get-in r [:seen :caller])))))
      (testing ":enforce - ling-a's credential does not let another spawned id speak"
        (let [r (call :enforce {"_caller_id" "ling-b:i1" "_caller_credential" token})]
          (is (false? (get-in r [:seen :identity :verified?] false)))))
      (testing ":enforce - a coordinator session without a credential stays allowed"
        (let [r (call :enforce {"_caller_id" "coordinator:s1"})]
          (is (:ran? r))))
      (testing "a tampered credential is never identity, in either mode"
        (doseq [mode [:observe :enforce]]
          (let [r (call mode {"_caller_id" "ling-a:i1"
                              "_caller_credential" (str (subs token 0 (- (count token) 3)) "AAA")})]
            (is (not (true? (get-in r [:seen :identity :verified?]))))
            (when (= :enforce mode) (is (not (:ran? r))))))))))
