(ns hive-mcp.agent.ling.start-preflight-test
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.agent.ling.start-preflight :as sp]
            [hive-addon.terminal :as term]
            [hive-mcp.agent.ling.spawn :as spawn]
            [hive-mcp.agent.ling.terminal-registry :as terminal-registry]))

(defrecord StubTrust [roots checkouts]
  sp/IFolderTrust
  (trusted-roots [_] roots)
  (checkout-root [_ dir] (get checkouts dir)))

(defrecord ThrowingTrust []
  sp/IFolderTrust
  (trusted-roots [_] (throw (ex-info "unreadable" {})))
  (checkout-root [_ _] nil))

(def ^:private hive "/ws/hive")
(def ^:private vessel "/ws/hive/hive-vessel")
(def ^:private dirge "/ws/hive/hive-dirge")
(def ^:private dirge-worktree "/ws/hive/hive-dirge/.claude/worktrees/w")

(def ^:private stub
  (->StubTrust #{hive dirge}
               {vessel vessel
                dirge dirge
                dirge-worktree dirge}))

(deftest trust-candidates-stop-at-the-checkout-root
  (is (= [vessel] (sp/trust-candidates vessel vessel)))
  (is (= ["/a/b/c" "/a/b" "/a" "/"] (sp/trust-candidates "/a/b/c" nil)))
  (is (= ["/r/.claude/worktrees/w" "/r/.claude/worktrees" "/r/.claude" "/r"]
         (sp/trust-candidates "/r/.claude/worktrees/w/" "/r"))))

(deftest untrusted-refusal-follows-claude-code-trust
  (testing "a git repo does not inherit trust from above its checkout"
    (let [r (sp/untrusted-refusal stub :claude vessel)]
      (is (= vessel (:root r)))
      (is (re-find #"hasTrustDialogAccepted" (:message r)))))
  (testing "a non-git dir inherits trust from a trusted ancestor"
    (is (nil? (sp/untrusted-refusal stub :claude (str hive "/docs")))))
  (testing "a trusted repo and its worktrees start"
    (is (nil? (sp/untrusted-refusal stub :claude dirge)))
    (is (nil? (sp/untrusted-refusal stub :claude dirge-worktree))))
  (testing "modes that do not run the Claude Code TUI are never refused"
    (is (nil? (sp/untrusted-refusal stub :headless vessel)))
    (is (nil? (sp/untrusted-refusal stub :agent-sdk vessel))))
  (testing "vterm runs the TUI too"
    (is (some? (sp/untrusted-refusal stub :vterm vessel)))))

(deftest ensure-startable-throws-a-typed-refusal
  (binding [sp/*folder-trust* stub]
    (let [e (try (sp/ensure-startable! :claude vessel) nil
                 (catch clojure.lang.ExceptionInfo e e))]
      (is (= :spawn/untrusted-folder (:reason (ex-data e))))
      (is (= :claude (:spawn-mode (ex-data e)))))
    (is (nil? (sp/ensure-startable! :claude hive)))))

(deftest unreadable-trust-admits-the-spawn
  (binding [sp/*folder-trust* (->ThrowingTrust)]
    (is (nil? (sp/ensure-startable! :claude vessel)))))

(defn- recording-terminal
  "ITerminalAddon stub that records every spawn it is asked for."
  [calls]
  (reify term/ITerminalAddon
    (terminal-id [_] :claude)
    (terminal-spawn! [_ ctx _opts] (swap! calls conj (:id ctx)) (:id ctx))
    (terminal-dispatch! [_ _ _] true)
    (terminal-status [_ _ ds-status] ds-status)
    (terminal-kill! [_ ctx] {:killed? true :id (:id ctx)})
    (terminal-interrupt! [_ ctx] {:success? true :ling-id (:id ctx)})))

(defn- with-claude-terminal [addon f]
  (let [prior (terminal-registry/get-terminal-addon :claude)]
    (terminal-registry/register-terminal! :claude addon)
    (try (f)
         (finally
           (if prior
             (terminal-registry/register-terminal! :claude prior)
             (terminal-registry/deregister-terminal! :claude))))))

(deftest untrusted-spawn-is-refused-before-the-terminal-starts
  (let [calls (atom [])]
    (with-claude-terminal (recording-terminal calls)
      (fn []
        (binding [sp/*folder-trust* stub]
          (let [e (try (spawn/create-ling! "probe-untrusted" {:cwd vessel :spawn-mode :claude})
                       nil
                       (catch clojure.lang.ExceptionInfo e e))]
            (is (= :spawn/untrusted-folder (:reason (ex-data e))))
            (is (= [] @calls))))))))

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "start-preflight" (make-array java.nio.file.attribute.FileAttribute 0))))

(deftest claude-code-adapter-reads-config-and-git-layout
  (let [base     (.getCanonicalPath (temp-dir))
        repo     (str base "/repo")
        worktree (str repo "/.claude/worktrees/w")
        plain    (str base "/plain/sub")
        config   (str base "/claude.json")]
    (.mkdirs (io/file repo ".git" "worktrees" "w"))
    (.mkdirs (io/file worktree))
    (spit (io/file worktree ".git") (str "gitdir: " repo "/.git/worktrees/w\n"))
    (.mkdirs (io/file plain))
    (spit config (json/write-str {"projects" {repo {"hasTrustDialogAccepted" true}
                                              base {"hasTrustDialogAccepted" false}}}))
    (let [adapter (sp/->ClaudeCodeTrust config)]
      (is (= #{repo} (sp/trusted-roots adapter)))
      (is (= repo (sp/checkout-root adapter repo)))
      (is (= repo (sp/checkout-root adapter worktree)))
      (is (nil? (sp/untrusted-refusal adapter :claude worktree)))
      (is (some? (sp/untrusted-refusal adapter :claude plain))))
    (is (= #{} (sp/trusted-roots (sp/->ClaudeCodeTrust (str base "/absent.json")))))))
