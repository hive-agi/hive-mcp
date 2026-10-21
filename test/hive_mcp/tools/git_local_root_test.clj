(ns hive-mcp.tools.git-local-root-test
  "The commit choke point for an escaping :local/root, driven through stub
   checker and repo ports (no hive-git on the unit classpath, no with-redefs)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.git-local-root :as lr]
            [hive-mcp.tools.magit :as magit]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private escaping
  {:file "deps.edn" :reason :local-root :coord 'io.github.hive-agi/hive-spi
   :path "../hive-spi-wt" :line 3})

(defn- stub-violations
  "A stand-in for hive-git.local-root/violations: content containing
   `../` escapes, anything else is clean."
  [file content]
  (if (and content (str/includes? content "../"))
    [(assoc escaping :file file)]
    []))

(defn- stub-checker
  "A checker whose refuse-commit reads the stub index through the repo
   port it closes over, and records the ctx and paths it was handed."
  [index calls]
  {:git :stub-git
   :violations stub-violations
   :refuse-commit (fn [ctx paths]
                    (swap! calls conj [ctx paths])
                    (let [vs (into [] (mapcat #(stub-violations % (get index %)))
                                   (distinct (concat ["deps.edn" "bb.edn"] paths)))]
                      (if (seq vs)
                        {:error :git/local-root :violations vs :message "refused"}
                        {:ok {:checked paths}})))})

(defn- stub-repo [{:keys [staged modified tree]}]
  {:staged-paths   (fn [_] staged)
   :modified-paths (fn [_] modified)
   :read-file      (fn [_ p] (get tree p))})

(deftest no-hive-git-checks-nothing
  (is (nil? (lr/refusal nil (stub-repo {}) "/r" false)))
  (is (nil? (lr/refusal nil (stub-repo {}) "/r" true))))

(deftest clean-index-commits
  (let [calls (atom [])
        checker (stub-checker {"deps.edn" "{:deps {a/b {:mvn/version \"1\"}}}"} calls)]
    (is (nil? (lr/refusal checker (stub-repo {:staged ["src/a.clj"]}) "/r" false)))
    (is (= [[{:git :stub-git :repo "/r"} ["src/a.clj"]]] @calls)
        "refuse-commit gets the git port, the repo and the staged paths")))

(deftest staged-escaping-local-root-is-refused
  (let [checker (stub-checker {"sub/deps.edn" "{:deps {x/y {:local/root \"../../x\"}}}"} (atom []))
        out (lr/refusal checker (stub-repo {:staged ["sub/deps.edn"]}) "/r" false)]
    (is (true? (:isError out)))
    (is (str/includes? (:text out) "sub/deps.edn:3"))
    (is (str/includes? (:text out) "io.github.hive-agi/hive-spi"))
    (is (str/includes? (:text out) "../hive-spi-wt"))))

(deftest commit-all-checks-the-working-tree
  (let [checker (stub-checker {} (atom []))
        repo (stub-repo {:staged [] :modified ["deps.edn" "README.md"]
                         :tree {"deps.edn" "{:deps {x/y {:local/root \"../x\"}}}"}})]
    (testing "a plain commit records the index, which is clean"
      (is (nil? (lr/refusal checker repo "/r" false))))
    (testing "commit --all would record the modified deps.edn"
      (is (true? (:isError (lr/refusal checker repo "/r" true)))))))

(deftest an-unexpected-checker-error-refuses
  (let [checker (assoc (stub-checker {} (atom []))
                       :refuse-commit (fn [_ _] {:error :git/show-failed :message "boom"}))
        out (lr/refusal checker (stub-repo {:staged []}) "/r" false)]
    (is (true? (:isError out)))
    (is (str/includes? (:text out) "boom"))))

(deftest the-commit-handler-routes-through-the-check
  (testing "the magit commit handler names the check in its contract"
    (is (str/includes? (:doc (meta #'magit/handle-magit-commit)) "local/root"))))
