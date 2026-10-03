#!/usr/bin/env bb
(ns foss-compliance-test
  "foss_compliance reads the RELEASE ref, never the working tree.

   Run: bb dev/foss_compliance_test.clj

   Every test hands the sweep a stub git port, a plain function over an
   in-memory map of refs to trees. Nothing is redefined; the port is the seam."
  (:require [babashka.fs :as fs]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing run-tests]]))

(load-file (str (fs/path (fs/parent *file*) "foss_compliance.clj")))
(alias 'fc 'foss-compliance)

;; ---------------------------------------------------------------------------
;; stub git port
;; ---------------------------------------------------------------------------

(defn stub-git
  "A git port over `repo`:
     {:remotes [\"origin\"] :head \"origin/main\" (optional)
      :refs {\"origin/main\" {:sha \"aaa…\" :files {path text}}}
      :tags {sha \"v1.2.3\"} :fetch-fails? bool}
   Records every call in `calls` (an atom)."
  [repo calls]
  (let [by-sha (into {} (for [[_ {:keys [sha files]}] (:refs repo)] [sha files]))
        ok     (fn [out] {:ok? true :out out :err ""})
        no     (fn [err] {:ok? false :out "" :err err})]
    (fn [& args]
      (swap! calls conj (vec args))
      (let [[cmd & more] args]
        (case cmd
          "remote"      (ok (str/join "\n" (:remotes repo)))
          "fetch"       (if (:fetch-fails? repo) (no "fatal: unable to access remote") (ok ""))
          "symbolic-ref" (if-let [h (:head repo)] (ok h) (no "not a symbolic ref"))
          "rev-parse"   (let [r (str/replace (last more) "^{commit}" "")]
                          (if-let [sha (get-in repo [:refs r :sha])] (ok sha) (no "")))
          "ls-tree"     (if-let [files (by-sha (last more))]
                          (ok (str/join "\u0000" (keys files)))
                          (no "bad object"))
          "show"        (let [[sha path] (str/split (first more) #":" 2)]
                          (if-let [t (get-in by-sha [sha path])] (ok t) (no "no such path")))
          "describe"    (if-let [t (get (:tags repo) (last more))] (ok t) (no "no tags"))
          (no (str "unexpected git call " (pr-str args))))))))

(def release-files
  {"VERSION"                       "0.6.5\n"
   "version.edn"                   (pr-str {:lib 'io.github.hive-agi/thing
                                            :publish :clojars
                                            :license {:name "MIT"}
                                            :src-dirs ["src"]})
   "deps.edn"                      (pr-str {:deps {'org.clojure/clojure {:mvn/version "1.12.0"}}})
   "LICENSE"                       "MIT License"
   "README.md"                     "Run `bin/go` to start."
   "bin/go"                        "#!/bin/sh"
   "src/thing/core.clj"            "(ns thing.core)"
   ".github/workflows/release.yml" "run: clojure -M:test"})

(def staging-files
  "What a developer's `staging` checkout might hold: no workflow, old VERSION."
  (-> release-files
      (assoc "VERSION" "0.6.2\n")
      (dissoc ".github/workflows/release.yml")))

(def repo
  {:remotes ["origin"]
   :head    "origin/main"
   :refs    {"origin/main"    {:sha "1111111aaaa" :files release-files}
             "origin/staging" {:sha "2222222bbbb" :files staging-files}}
   :tags    {"1111111aaaa" "v0.6.5"}})

(defn facts-for
  ([r] (facts-for r {}))
  ([r opts]
   (let [calls (atom [])
         git   (stub-git r calls)
         ref   (fc/resolve-release-ref git opts)]
     {:calls calls
      :ref   ref
      :facts (when-not (:error ref)
               (fc/tree-facts {:repo "thing" :dir "/nowhere" :ref ref :git git
                               :tree (fc/ref-tree git (:sha ref))
                               :spdx {"thing" "MIT"}
                               :clojars-fn (constantly "0.6.5")}))})))

(defn status-of [facts check]
  (:status (first (filter #(= check (:check %)) (fc/run-checks facts)))))

;; ---------------------------------------------------------------------------
;; tests
;; ---------------------------------------------------------------------------

(deftest resolves-the-release-ref-after-a-fetch
  (let [{:keys [ref calls]} (facts-for repo)]
    (is (= "origin/main" (:ref ref)))
    (is (= "1111111aaaa" (:sha ref)))
    (is (true? (:fetched? ref)))
    (is (= ["fetch" "--quiet" "--tags" "origin"]
           (first (filter #(= "fetch" (first %)) @calls)))
        "the fetch is explicit and happens before the ref is read")))

(deftest falls-back-to-main-then-master-without-a-remote-head
  (is (= "origin/main" (:ref (:ref (facts-for (dissoc repo :head))))))
  (let [master (-> repo (dissoc :head)
                   (update :refs #(-> % (assoc "origin/master" (get % "origin/main"))
                                      (dissoc "origin/main"))))]
    (is (= "origin/master" (:ref (:ref (facts-for master))))))
  (testing "a github-only remote is used when there is no origin"
    (let [gh (-> repo (assoc :remotes ["github"]) (dissoc :head)
                 (assoc :refs {"github/main" (get-in repo [:refs "origin/main"])}))]
      (is (= "github/main" (:ref (:ref (facts-for gh))))))))

(deftest the-checked-out-branch-never-decides-the-verdict
  (testing "facts come from the release tree, not from staging"
    (let [{:keys [facts]} (facts-for repo)]
      (is (= "0.6.5" (:version facts)))
      (is (= ["release.yml"] (:workflows facts)))
      (is (= :pass (status-of facts :ci)))
      (is (= :pass (status-of facts :version-truth)))
      (is (= :pass (status-of facts :readme)))
      (is (= :pass (status-of facts :license)))))
  (testing "only an explicit --ref judges another branch"
    (let [{:keys [facts]} (facts-for repo {:ref "origin/staging"})]
      (is (= "0.6.2" (:version facts)))
      (is (= :fail (status-of facts :ci)))
      (is (= :fail (status-of facts :version-truth))))))

(deftest every-read-goes-through-git-show-at-the-resolved-sha
  (let [{:keys [calls]} (facts-for repo)
        shows (filter #(= "show" (first %)) @calls)]
    (is (seq shows))
    (is (every? #(str/starts-with? (second %) "1111111aaaa:") shows))))

(deftest a-failed-fetch-is-reported-as-staleness-not-a-violation
  (let [{:keys [ref facts]} (facts-for (assoc repo :fetch-fails? true))]
    (is (= "origin/main" (:ref ref)) "the last fetched ref still resolves")
    (is (false? (:fetched? ref)))
    (is (str/includes? (fc/ref-label ref false) "STALE"))
    (is (= :pass (status-of facts :ci)))))

(deftest offline-never-fetches
  (let [{:keys [ref calls]} (facts-for repo {:offline? true})]
    (is (not-any? #(= "fetch" (first %)) @calls))
    (is (str/includes? (fc/ref-label ref true) "offline"))))

(deftest no-release-ref-is-one-loud-failure
  (let [r {:remotes ["origin"] :refs {"origin/wip" {:sha "333" :files release-files}}}
        {:keys [ref]} (facts-for r)]
    (is (:error ref))
    (is (= [{:check :release-ref :status :fail :evidence (:error ref)}]
           (fc/ref-results {:checkout? true :ref-error (:error ref)})))))

(deftest clojars-release-with-a-git-url-dep-fails
  (let [bad (assoc-in repo [:refs "origin/main" :files "deps.edn"]
                      (pr-str {:deps {'io.github.x/overarch {:git/url "https://github.com/x/o"
                                                             :git/sha "abc"}}}))
        {:keys [facts]} (facts-for bad)]
    (is (= :fail (status-of facts :pom-coords))))
  (is (= :pass (status-of (:facts (facts-for repo)) :pom-coords)))
  (testing "a non-clojars repo is not judged by the pom rule"
    (let [gitea (assoc-in repo [:refs "origin/main" :files "version.edn"]
                          (pr-str {:lib 'x/y :publish :gitea :license {:name "MIT"}}))]
      (is (nil? (status-of (:facts (facts-for gitea)) :pom-coords))))))

(deftest manifests-and-sources-are-read-from-the-tree
  (let [addon (update-in repo [:refs "origin/main" :files] assoc
                         "resources/META-INF/hive-addons/thing.edn"
                         (pr-str {:addon/id "thing" :addon/init-ns 'thing.core
                                  :addon/maturity :beta :addon/trust-class :foss})
                         "version.edn"
                         (pr-str {:lib 'io.github.hive-agi/thing :publish :clojars
                                  :license {:name "MIT"} :src-dirs ["src" "resources"]})
                         "src/thing/bad.clj" "(ns thing.bad) (hive-mcp.core/boot!)")
        {:keys [facts]} (facts-for addon)]
    (is (= #{"src"} (:source-roots facts)))
    (is (= :warn (status-of facts :mount-contract)) "init-ns resolves in the tree")
    (is (= :pass (status-of facts :declarations)))
    (is (= :fail (status-of facts :host-coupling)))))

(defn- addon-repo
  "`repo` shipping one addon manifest, with version.edn naming `publish` and
   `licence` (nil leaves :license out) and the manifest naming `manifest`."
  [{:keys [publish licence manifest]}]
  (update-in repo [:refs "origin/main" :files] assoc
             "resources/META-INF/hive-addons/thing.edn"
             (pr-str (merge {:addon/id "thing" :addon/init-ns 'thing.core} manifest))
             "version.edn"
             (pr-str (cond-> {:lib 'io.github.hive-agi/thing :publish publish
                              :src-dirs ["src" "resources"]}
                       licence (assoc :license {:name licence})))))

(defn- declarations-of [spec]
  (status-of (:facts (facts-for (addon-repo spec))) :declarations))

(deftest the-licence-not-the-registry-decides-the-trust-class
  (testing "a FOSS licence on a private Gitea registry is a :warn, not a violation"
    (is (= :warn (declarations-of {:publish :gitea :licence "AGPL-3.0-or-later"
                                   :manifest {:addon/maturity :beta
                                              :addon/trust-class :foss}}))))
  (testing "a proprietary licence on Clojars is the same convention mismatch: :warn"
    (is (= :warn (declarations-of {:publish :clojars :licence "LicenseRef-Proprietary"
                                   :manifest {:addon/maturity :beta
                                              :addon/trust-class :proprietary}}))))
  (testing "registry and licence agreeing with the trust class passes"
    (is (= :pass (declarations-of {:publish :gitea :licence "LicenseRef-Proprietary"
                                   :manifest {:addon/maturity :beta
                                              :addon/trust-class :proprietary}})))
    (is (= :pass (declarations-of {:publish :clojars :licence "EPL-2.0"
                                   :manifest {:addon/maturity :beta
                                              :addon/trust-class :foss}})))))

(deftest a-licence-that-contradicts-the-trust-class-fails
  (is (= :fail (declarations-of {:publish :gitea :licence "LicenseRef-Proprietary"
                                 :manifest {:addon/maturity :beta
                                            :addon/trust-class :foss}}))
      "a proprietary licence cannot ship under the :foss trust class")
  (is (= :fail (declarations-of {:publish :clojars :licence "MIT"
                                 :manifest {:addon/maturity :beta
                                            :addon/trust-class :proprietary}}))
      "and an OSI licence cannot ship as :proprietary"))

(deftest an-undeclared-trust-class-still-fails
  (is (= :fail (declarations-of {:publish :clojars :licence "MIT"
                                 :manifest {:addon/maturity :beta}})))
  (is (= :fail (declarations-of {:publish :clojars :licence "MIT"
                                 :manifest {:addon/trust-class :foss}}))))

(deftest no-known-licence-falls-back-to-the-warn-path
  (is (= :warn (declarations-of {:publish :gitea :licence nil
                                 :manifest {:addon/maturity :beta
                                            :addon/trust-class :foss}})))
  (is (= :warn (declarations-of {:publish :clojars :licence "UNDECLARED"
                                 :manifest {:addon/maturity :beta
                                            :addon/trust-class :proprietary}}))))

(when (= *file* (System/getProperty "babashka.file"))
  (let [{:keys [fail error]} (run-tests 'foss-compliance-test)]
    (System/exit (if (zero? (+ fail error)) 0 1))))
