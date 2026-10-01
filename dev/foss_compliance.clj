#!/usr/bin/env bb
(ns foss-compliance
  "Compliance sweep over the public hive-agi repositories.

   Enumerates the org from GitHub (never from a curated list), probes each
   local checkout plus its published coordinates, and runs an open registry
   of checks over the resulting facts.

   Facts are read from the RELEASE ref (origin/HEAD, else origin/main, else
   origin/master), after an explicit `git fetch`, via `git show <sha>:<path>`.
   The working tree and whatever branch is checked out are never consulted
   (except by the go check, which needs a buildable tree and says so). The
   ref read is printed per repo; a failed fetch is reported as STALE.

   Usage:
     bb dev/foss_compliance.clj                 # every public repo
     bb dev/foss_compliance.clj lsp-mcp scc-mcp # named repos only
     bb dev/foss_compliance.clj --offline       # no fetch, no Clojars/GitHub
     bb dev/foss_compliance.clj --ref=origin/x  # judge another ref
     bb dev/foss_compliance.clj --edn           # machine-readable output

   Exit code is 1 when any check fails, 0 otherwise."
  (:require [babashka.fs :as fs]
            [babashka.http-client :as http]
            [babashka.process :as process]
            [cheshire.core :as json]
            [clojure.edn :as edn]
            [clojure.string :as str]))

;; ---------------------------------------------------------------------------
;; boundary: shelling out, reading files, reaching the network
;; ---------------------------------------------------------------------------

(def ^:private manifest-dir "resources/META-INF/hive-addons")

(defn- sh
  "Run `args` in `dir`. Returns {:ok? :out :err}; never throws. A missing
   directory or absent executable is reported as a failed run, not an
   exception, so one unclonable repo cannot abort the sweep."
  [dir & args]
  (if-not (fs/directory? dir)
    {:ok? false :out "" :err (str "no such directory: " dir)}
    (try
      (let [{:keys [exit out err]} (apply process/sh {:dir (str dir) :continue true} args)]
        {:ok? (zero? exit) :out (str/trim (or out "")) :err (str/trim (or err ""))})
      (catch Exception e
        {:ok? false :out "" :err (ex-message e)}))))

(defn- org-repos
  "Public, non-archived repo names under `org`, from the GitHub API."
  [org]
  (let [{:keys [ok? out]} (sh "." "gh" "api" (str "orgs/" org "/repos") "--paginate"
                              "-q" ".[] | select(.archived|not) | [.name, .license.spdx_id // \"NONE\"] | @tsv")]
    (when ok?
      (into {} (for [line (str/split-lines out)
                     :when (seq line)
                     :let [[n spdx] (str/split line #"\t")]]
                 [n spdx])))))

(defn- clojars-latest
  "Latest released version of `lib` on Clojars, or nil."
  [lib]
  (try
    (let [url  (str "https://clojars.org/api/artifacts/" (namespace lib) "/" (name lib))
          resp (http/get url {:headers {"Accept" "application/json"} :throw false :timeout 15000})]
      (when (= 200 (:status resp))
        (get (json/parse-string (:body resp) true) :latest_release)))
    (catch Exception _ nil)))

(defn- jar-has-manifest?
  "True when the released jar for `lib`/`version` contains a hive-addons manifest.
   nil when the jar could not be fetched."
  [lib version cache-dir]
  (let [[grp art] [(namespace lib) (name lib)]
        path      (str (str/replace grp "." "/") "/" art "/" version "/" art "-" version ".jar")
        jar       (fs/path cache-dir (str art "-" version ".jar"))]
    (when-not (fs/exists? jar)
      (fs/create-dirs cache-dir)
      (sh "." "curl" "-fsSL" "-o" (str jar) (str "https://repo.clojars.org/" path)))
    (when (fs/exists? jar)
      (let [{:keys [ok? out]} (sh "." "unzip" "-l" (str jar))]
        (when ok? (str/includes? out "META-INF/hive-addons/"))))))

;; ---------------------------------------------------------------------------
;; release tree: what the checks read, taken from the RELEASE ref
;; ---------------------------------------------------------------------------
;;
;; The sweep used to slurp the working tree, so it answered a question about
;; whichever branch a developer happened to have checked out (often `staging`,
;; often unpulled) instead of about what the release pipeline ships. Every
;; fact below is now read from a git ref through `git`, a port of shape
;; (fn [& args] -> {:ok? :out :err}) bound to one repository. The real port
;; shells out; tests hand in a stub.

(def release-branches
  "Branch names tried, in order, when the remote names no HEAD."
  ["main" "master"])

(defn git-port
  "The real git port for the checkout at `dir`."
  [dir]
  (fn [& args] (apply sh dir "git" args)))

(defn- remote-name
  "First of origin/github the checkout knows, or nil."
  [git]
  (let [{:keys [ok? out]} (git "remote")
        known (when ok? (set (str/split-lines out)))]
    (some #(when (contains? known %) %) ["origin" "github"])))

(defn resolve-release-ref
  "The ref releases are cut from, after an explicit fetch unless `offline?`.

   Returns {:ref \"origin/main\" :sha .. :fetched? bool :fetch-error ..}, or
   {:error msg} when no release ref exists. A failed fetch is NOT fatal: the
   ref still resolves to the last fetched commit, and :fetched? false lets the
   report say so, so staleness surfaces as staleness, not as a violation."
  [git {:keys [offline? ref]}]
  (if-let [remote (remote-name git)]
    (let [fetch    (when-not offline? (git "fetch" "--quiet" "--tags" remote))
          head     (let [{:keys [ok? out]} (git "symbolic-ref" "--short"
                                                (str "refs/remotes/" remote "/HEAD"))]
                     (when (and ok? (seq out)) out))
          cands    (if ref
                     [ref]
                     (distinct (cons head (map #(str remote "/" %) release-branches))))
          resolved (some (fn [r]
                           (when r
                             (let [{:keys [ok? out]} (git "rev-parse" "--verify" "--quiet"
                                                          (str r "^{commit}"))]
                               (when (and ok? (seq out)) {:ref r :sha out}))))
                         cands)]
      (if resolved
        (assoc resolved
               :fetched? (boolean (and fetch (:ok? fetch)))
               :fetch-error (when (and fetch (not (:ok? fetch))) (:err fetch)))
        {:error (str "no release ref among " (str/join ", " (remove nil? cands)))}))
    {:error "no origin/github remote"}))

(defn ref-tree
  "A read-only view of the repository at commit `sha`:
   {:paths #{repo-relative file paths} :text (fn [path] -> string|nil)}."
  [git sha]
  (let [{:keys [ok? out]} (git "ls-tree" "-r" "-z" "--name-only" sha)
        paths (if ok? (into #{} (remove str/blank?) (str/split out #"\u0000")) #{})]
    {:paths paths
     :text  (fn [path]
              (when (contains? paths path)
                (let [{:keys [ok? out]} (git "show" (str sha ":" path))]
                  (when ok? out))))}))

(defn- tree-text [tree path] ((:text tree) path))

(defn- tree-edn
  "Parsed EDN at `path` in `tree`, or {::error msg} when it does not parse."
  [tree path]
  (when-let [s (tree-text tree path)]
    (try (edn/read-string s)
         (catch Exception e {::error (ex-message e)}))))

(defn- tree-file? [tree path] (contains? (:paths tree) path))

(defn tree-exists?
  "File or directory `path` is present in `tree` (git tracks no empty dirs)."
  [tree path]
  (let [p (str/replace (str path) #"/+$" "")]
    (or (tree-file? tree p)
        (boolean (some #(str/starts-with? % (str p "/")) (:paths tree))))))

(defn tree-files
  "Paths under directory `dir` whose name ends in one of `exts`. `deep?`
   descends into subdirectories."
  [tree dir exts deep?]
  (let [prefix (str (str/replace (str dir) #"/+$" "") "/")]
    (->> (:paths tree)
         (filter #(and (str/starts-with? % prefix)
                       (or deep? (not (str/includes? (subs % (count prefix)) "/")))
                       (some (fn [e] (str/ends-with? % e)) exts)))
         sort
         vec)))

;; ---------------------------------------------------------------------------
;; facts: one map per repo, everything a check may need
;; ---------------------------------------------------------------------------

(def ^:private clojure-exts [".clj" ".cljc" ".cljs" ".cljd"])

(defn- latest-tag
  [git sha]
  (let [{:keys [ok? out]} (git "describe" "--tags" "--abbrev=0" sha)]
    (when (and ok? (seq out)) out)))

(defn tree-facts
  "Everything the checks read about the release `tree` of `repo`. Pure apart
   from `clojars-fn` / `jar-fn`, the network ports."
  [{:keys [repo dir ref git tree spdx clojars-fn jar-fn]}]
  (let [vedn      (tree-edn tree "version.edn")
        lib       (:lib vedn)
        manifests (vec (for [p (tree-files tree manifest-dir [".edn"] false)]
                         {:path p :edn (tree-edn tree p)}))
        clojars   (when (and clojars-fn (qualified-symbol? lib) (= :clojars (:publish vedn)))
                    (clojars-fn lib))]
    {:repo          repo
     :dir           (str dir)
     :checkout?     true
     :ref           ref
     :paths         (:paths tree)
     :version.edn   vedn
     :lib           lib
     :version       (some-> (tree-text tree "VERSION") str/trim)
     :src-dirs      (:src-dirs vedn)
     :source-roots  (into #{} (filter #(seq (tree-files tree % clojure-exts true)))
                          (:src-dirs vedn))
     :manifests     manifests
     :host-sources  (when (seq manifests)
                      (into {} (for [p (tree-files tree "src" [".clj" ".cljc"] true)]
                                 [p (tree-text tree p)])))
     :deps.edn      (tree-edn tree "deps.edn")
     :workflows     (mapv #(last (str/split % #"/"))
                          (tree-files tree ".github/workflows" [".yml" ".yaml"] false))
     :release-yml   (tree-text tree ".github/workflows/release.yml")
     :license?      (some #(tree-file? tree %) ["LICENSE" "LICENSE.md" "LICENSE.txt"])
     :readme        (tree-text tree "README.md")
     :go?           (tree-file? tree "go.mod")
     :git-tag       (when git (latest-tag git (:sha ref)))
     :github-spdx   (get spdx repo)
     :clojars       clojars
     :jar-manifest? (when (and clojars jar-fn (seq manifests))
                      (jar-fn lib clojars))}))

(defn repo-facts
  "Everything the checks read about `repo`, probed once, from its release ref."
  [{:keys [root offline? spdx cache-dir ref git-fn]} repo]
  (let [dir (fs/path root repo)]
    (if-not (fs/directory? (fs/path dir ".git"))
      {:repo repo :dir (str dir) :checkout? false}
      (let [git      ((or git-fn git-port) dir)
            resolved (resolve-release-ref git {:offline? offline? :ref ref})]
        (if (:error resolved)
          {:repo repo :dir (str dir) :checkout? true :ref-error (:error resolved)}
          (tree-facts {:repo repo :dir dir :ref resolved :git git
                       :tree (ref-tree git (:sha resolved)) :spdx spdx
                       :clojars-fn (when-not offline? clojars-latest)
                       :jar-fn (when-not offline? #(jar-has-manifest? %1 %2 cache-dir))}))))))

;; ---------------------------------------------------------------------------
;; checks: pure, an open registry; each entry decides whether it applies
;; ---------------------------------------------------------------------------

(defn- verdict
  ([status evidence] {:status status :evidence evidence}))

(defn- packaging
  "An addon manifest only reaches consumers when its root is a RESOURCE root
   in :src-dirs. hive-build copies source roots by compiling them and
   resource roots verbatim, so a root holding only EDN must be declared."
  [{:keys [src-dirs source-roots jar-manifest?]}]
  (let [declared (set src-dirs)
        res-root (first (filter #(and (declared %) (not (source-roots %))
                                      (str/starts-with? manifest-dir %))
                                declared))]
    (cond
      (nil? res-root)
      (verdict :fail (str ":src-dirs " (pr-str (vec src-dirs))
                          " declares no resource root covering " manifest-dir))

      (false? jar-manifest?)
      (verdict :fail (str "released jar carries no META-INF/hive-addons "
                          "(root '" res-root "' is declared; release not cut yet?)"))

      (true? jar-manifest?)
      (verdict :pass (str "root '" res-root "' declared; released jar carries the manifest"))

      :else
      (verdict :warn (str "root '" res-root "' declared; jar not inspected")))))

(defn- mount-contract
  "The manifest's :addon/init-ns must exist as a source file under a declared
   source root. Whether its ctor returns an IAddon is a boot-time claim."
  [{:keys [paths manifests source-roots]}]
  (let [rows (for [{m :edn} manifests
                   :let [{:addon/keys [id init-ns init-fn]} m
                         rel (str (str/replace (str init-ns) #"[.-]"
                                               {"." "/" "-" "_"}) ".clj")
                         hit (first (filter #(contains? paths (str % "/" rel)) source-roots))]]
               {:id id :ns init-ns :fn init-fn :file hit})]
    (if-let [missing (seq (remove :file rows))]
      (verdict :fail (str "init-ns not found under a source root: "
                          (str/join ", " (map :ns missing))))
      (verdict :warn (str (count rows) " manifest(s) resolve; ctor return type is a boot claim: "
                          (str/join ", " (map :id rows)))))))

(defn- manifest-declarations
  "Every manifest must STATE :addon/maturity and :addon/trust-class, and the
   trust class must agree with where the artifact is published.

   Both fields are schema-optional with a permissive default, so an omission
   VALIDATES and then reads as the safe-looking answer. That is exactly how
   the licence gate came to be bypassed fleet-wide: an absent
   :addon/trust-class defaults to :foss, so hive-addon.mount.entitlement/gated?
   answered false for every addon and the closed gate was never consulted.
   Measuring the field's presence is the only thing that catches it."
  [facts]
  (let [vedn (:version.edn facts)
        rows (for [{manifest :edn} (:manifests facts)]
               {:id (:addon/id manifest)
                :status (:addon/maturity manifest)
                :trust (:addon/trust-class manifest)})
        undeclared (remove #(and (:status %) (:trust %)) rows)
        expected (case (:publish vedn)
                   :gitea :proprietary
                   :clojars :foss
                   nil)
        mismatched (when expected (remove #(= expected (:trust %)) rows))]
    (cond
      (seq undeclared)
      (verdict :fail (str "manifest omits :addon/maturity or :addon/trust-class: "
                          (str/join ", " (map :id undeclared))))

      (seq mismatched)
      (verdict :fail (str "publish " (:publish vedn) " implies :addon/trust-class "
                          expected ", manifest declares: "
                          (str/join ", " (map #(str (:id %) "=" (pr-str (:trust %)))
                                              mismatched))))

      (nil? expected)
      (verdict :warn (str "declared, but version.edn names no :publish target to "
                          "cross-check the trust class against: "
                          (str/join ", " (map #(str (:id %) " " (:status %)
                                                    "/" (:trust %))
                                              rows))))

      :else
      (verdict :pass (str (count rows) " manifest(s) declare maturity + trust-class: "
                          (str/join ", " (map #(str (:id %) " " (:status %)
                                                    "/" (:trust %))
                                              rows)))))))

(def ^:private hard-host-ref
  "A hive-mcp qualified symbol NOT preceded by a quote. A quoted symbol handed
   to requiring-resolve is the blessed soft-resolution seam; an unquoted one is
   resolved at compile time and makes the host a build dependency."
  #"(?<!['`])\bhive-mcp\.[a-z0-9.-]+/[A-Za-z!?*<>=+-][^\s()\[\]{},;]*")

(defn- strip-noise
  "Source with string literals and line comments blanked out. A namespace named
   in a docstring is prose, not a dependency, and counting it as one is how a
   checker starts crying wolf."
  [src]
  (-> src
      (str/replace #"(?s)\"(?:\\.|[^\"\\])*\"" "\"\"")
      (str/replace #";[^\n]*" "")))

(defn- host-coupling
  "An addon must compile against the contract libs, never against the host."
  [{:keys [host-sources]}]
  (let [hits  (for [[f src] (sort-by key host-sources)
                    :let [body (some-> src strip-noise)]
                    :when body
                    m (re-seq hard-host-ref body)]
                {:file f :sym m})]
    (if (seq hits)
      (verdict :fail (str (count hits) " compile-time host reference(s): "
                          (str/join ", " (distinct (map :sym (take 4 hits))))))
      (verdict :pass "no compile-time hive-mcp reference"))))

(defn- version-truth
  "VERSION, the newest git tag and the latest Clojars release must agree."
  [{:keys [version git-tag clojars]}]
  (let [tag (some-> git-tag (str/replace #"^v" ""))
        seen (remove nil? [version tag clojars])]
    (cond
      (nil? version)          (verdict :fail "no VERSION file")
      (apply = seen)          (verdict :pass (str "VERSION=" version " tag=" (or tag "-")
                                                  " clojars=" (or clojars "-")))
      :else                   (verdict :fail (str "VERSION=" version " tag=" (or tag "-")
                                                  " clojars=" (or clojars "-"))))))

(def ^:private test-invocation
  "A shell command that actually runs a suite. Matching the bare word 'test'
   is not enough: `cli: latest` contains it."
  #"(?m)-M:[\w:.-]*test|-X:[\w:.-]*test|-T:build\s+test|kaocha|bb\s+test|make\s+test|lein\s+test")

(defn- ci
  "A workflow must exist, and a release workflow must run the suite."
  [{:keys [workflows release-yml]}]
  (cond
    (empty? workflows) (verdict :fail "no .github/workflows")
    (and release-yml (not (re-find test-invocation release-yml)))
    (verdict :fail "release.yml deploys without running a suite")
    :else (verdict :pass (str/join ", " workflows))))

(defn- license
  "LICENSE on disk, version.edn :license and the GitHub SPDX must agree."
  [{:keys [license? version.edn github-spdx]}]
  (let [declared (get-in version.edn [:license :name])]
    (cond
      (not license?)                       (verdict :fail "no LICENSE file")
      (or (nil? declared) (= "UNDECLARED" declared))
      (verdict :fail (str "version.edn :license " (pr-str declared)))
      (and github-spdx (not (contains? #{"NONE" "NOASSERTION" nil} github-spdx))
           (not= github-spdx declared))
      (verdict :fail (str "version.edn says " declared ", GitHub detects " github-spdx))
      (contains? #{"NONE" "NOASSERTION"} github-spdx)
      (verdict :warn (str declared " on disk; GitHub detects " github-spdx))
      :else (verdict :pass declared))))

(defn- readme-commands
  "Every repo-relative path a README references in a command must exist."
  [{:keys [readme] :as facts}]
  (if-not readme
    (verdict :fail "no README.md")
    (let [refs (into #{} (map second)
                     (re-seq #"(?m)(?:^|[\s`(])((?:bin|scripts|dev)/[A-Za-z0-9._/-]+)" readme))
          gone (remove #(tree-exists? facts %) refs)]
      (if (seq gone)
        (verdict :fail (str "README names missing paths: " (str/join ", " gone)))
        (verdict :pass (str (count refs) " referenced path(s) exist"))))))

(defn- escaping-root?
  "True when a :local/root cannot be resolved from a fresh clone: it points
   outside the repo, or at a path the release tree does not contain. A
   vendored jar committed under the repo resolves everywhere and is not a
   violation."
  [facts root]
  (or (str/starts-with? root "/")
      (str/starts-with? root "..")
      (not (tree-exists? facts (str/replace root #"^\./" "")))))

(defn- deps-hygiene
  "A published deps.edn names coordinates a fresh clone can resolve."
  [{:keys [deps.edn] :as facts}]
  (let [locals (for [[lib coord] (:deps deps.edn)
                     :let [root (:local/root coord)]
                     :when (and root (escaping-root? facts root))]
                 (str lib))
        repos  (remove #{"central" "clojars"} (keys (:mvn/repos deps.edn)))]
    (cond
      (seq locals) (verdict :fail (str "unresolvable :local/root deps: " (str/join ", " locals)))
      (seq repos)  (verdict :warn (str "extra :mvn/repos: " (str/join ", " repos)))
      :else        (verdict :pass (str (count (:deps deps.edn)) " public coordinate(s)")))))

(defn- pom-coordinates
  "A :clojars release is described by a pom, and a pom can only name Maven
   coordinates. A :git/url dep therefore vanishes from the published artifact
   and every consumer fails to resolve it (how hive-overarch shipped broken)."
  [{:keys [deps.edn]}]
  (let [gits (sort (for [[lib coord] (:deps deps.edn)
                         :when (and (map? coord) (:git/url coord))]
                     (str lib)))]
    (if (seq gits)
      (verdict :fail (str ":publish :clojars but :deps carry :git/url (a pom cannot): "
                          (str/join ", " gits)))
      (verdict :pass (str (count (:deps deps.edn)) " dep(s), none by :git/url")))))

(defn- clojars-published?
  [facts]
  (and (:deps.edn facts)
       (contains? #{:clojars :clojars-aot} (get-in facts [:version.edn :publish]))))

(defn- go-build
  "gofmt, go build and go vet over the whole module. The one check that needs
   a buildable tree, so it runs in the WORKING TREE and says so."
  [{:keys [dir]}]
  (let [fmt   (sh dir "gofmt" "-l" ".")
        build (sh dir "go" "build" "./...")
        vet   (sh dir "go" "vet" "./...")]
    (cond
      (seq (:out fmt))   (verdict :fail (str "gofmt -l: " (str/replace (:out fmt) "\n" " ")))
      (not (:ok? build)) (verdict :fail (str "go build ./...: " (first (str/split-lines (:err build)))))
      (not (:ok? vet))   (verdict :fail (str "go vet ./...: " (first (str/split-lines (:err vet)))))
      :else              (verdict :pass "gofmt/build/vet clean (working tree)"))))

(def checks
  "Ordered registry. Each entry applies only where its subject exists, so a
   repo that ships no addon is not failed for shipping no manifest."
  [{:id :packaging      :applies? (comp seq :manifests)      :run packaging}
   {:id :mount-contract :applies? (comp seq :manifests)      :run mount-contract}
   {:id :declarations   :applies? (comp seq :manifests)      :run manifest-declarations}
   {:id :host-coupling  :applies? (comp seq :manifests)      :run host-coupling}
   {:id :version-truth  :applies? :version.edn               :run version-truth}
   {:id :ci             :applies? (constantly true)          :run ci}
   {:id :license        :applies? :version.edn               :run license}
   {:id :readme         :applies? (constantly true)          :run readme-commands}
   {:id :deps-hygiene   :applies? :deps.edn                  :run deps-hygiene}
   {:id :pom-coords     :applies? clojars-published?         :run pom-coordinates}
   {:id :go             :applies? :go?                       :run go-build}])

(defn run-checks
  "Every applicable check over `facts`, as [{:check :status :evidence}]."
  [facts]
  (into []
        (keep (fn [{:keys [id applies? run]}]
                (when (applies? facts)
                  (try (assoc (run facts) :check id)
                       (catch Exception e
                         {:check id :status :fail :evidence (str "check threw: " (ex-message e))})))))
        checks))

;; ---------------------------------------------------------------------------
;; report
;; ---------------------------------------------------------------------------

(def ^:private marks {:pass "PASS" :fail "FAIL" :warn "WARN"})

(defn ref-label
  "How the ref a repo was judged at is named in the report. A failed fetch is
   called out, so a stale ref reads as staleness rather than as a violation."
  [{:keys [ref sha fetched? fetch-error]} offline?]
  (str ref "@" (subs (str sha "0000000") 0 7)
       (cond offline?    " (offline: last fetched)"
             fetched?    ""
             fetch-error (str " (STALE: fetch failed: " (first (str/split-lines fetch-error)) ")")
             :else       " (not fetched)")))

(defn ref-results
  "The checks for `facts`, or the one verdict that says the release ref could
   not be read. A repo with no release ref is not judged on a desk checkout."
  [facts]
  (cond
    (not (:checkout? facts)) nil
    (:ref-error facts)       [{:check :release-ref :status :fail :evidence (:ref-error facts)}]
    :else                    (run-checks facts)))

(defn- print-repo!
  [{:keys [repo checkout? ref]} results offline?]
  (if-not checkout?
    (println (format "%-22s  %-4s  %-15s %s" repo "SKIP" "-" "no local checkout"))
    (do (when ref
          (println (format "%-22s  %-4s  %-15s %s" repo "REF" "release-ref" (ref-label ref offline?))))
        (doseq [{:keys [check status evidence]} results]
          (println (format "%-22s  %-4s  %-15s %s" repo (marks status status) (name check) evidence))))))

(defn- summarize
  [rows]
  (frequencies (map :status rows)))

(defn -main
  [& args]
  (let [flags    (set (filter #(str/starts-with? % "--") args))
        ref      (some #(second (re-matches #"--ref=(.+)" %)) flags)
        named    (remove #(str/starts-with? % "--") args)
        offline? (contains? flags "--offline")
        root     (str (fs/parent (fs/cwd)))
        spdx     (if offline? {} (or (org-repos "hive-agi") {}))
        repos    (if (seq named) (vec named) (vec (sort (keys spdx))))
        ctx      {:root root :offline? offline? :spdx spdx :ref ref
                  :cache-dir (fs/path (fs/temp-dir) "hive-foss-jars")}]
    (when (empty? repos)
      (println "No repos to sweep (gh unavailable? pass repo names explicitly).")
      (System/exit 2))
    (let [report (vec (for [r repos
                            :let [facts (repo-facts ctx r)]]
                        {:repo r :facts facts :results (ref-results facts)}))
          rows   (mapcat :results report)]
      (if (contains? flags "--edn")
        (prn (mapv (fn [{:keys [repo facts results]}]
                     {:repo repo :ref (select-keys (:ref facts) [:ref :sha :fetched? :fetch-error])
                      :results results})
                   report))
        (do (println (format "%-22s  %-4s  %-15s %s" "REPO" "" "CHECK" "EVIDENCE"))
            (doseq [{:keys [facts results]} report] (print-repo! facts results offline?))
            (println)
            (println "summary:" (pr-str (summarize rows)))))
      (System/exit (if (some #(= :fail (:status %)) rows) 1 0)))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))
