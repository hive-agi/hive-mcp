(ns hive-mcp.tools.magit-path-safety-test
  "stage and commit read the caller's paths literally and touch nothing else
   (review 20261111112014-721856fa of lane git-bridge, findings 1-9).

   Default suite: every test runs at the tool-handler seam (the consolidated
   `git` tool) over hive-mcp.test.stub.git-world, an in-memory repository
   bound as `magit/*repo-port*`, with the editor vessel stubbed. No git
   binary, no disk, no Emacs. The same chain against a real temp repo is
   hive-mcp.tools.magit-path-safety-e2e-test, skipped by default."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.consolidated.git :as git-tool]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.test.stub.git-world :as gw]
            [hive-mcp.test.stub.swarm-host :as swarm]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private entrypoints-failed
  {:success false :timed-out false :error "Emacs bridge entrypoints failed to load."})

(def ^:private next-app
  {"pages/[id].tsx" "id" "pages/i.tsx" "i" "pages/d.tsx" "d" "a.txt" "a" "b.txt" "b"})

(defmacro ^:private in-world
  "Run BODY with W as the repository and the editor answering through RESPOND
   (:editor = the Magit-like fake, :down = bridge entrypoints failing)."
  [[w respond] & body]
  `(binding [magit/*repo-port* (gw/port ~w)
             magit/*bridge-down-ms* 0
             magit/*handshake-retry-delay-ms* 0]
     (swarm/with-swarm-host [h# (case ~respond
                                  :editor (gw/editor ~w)
                                  :down (constantly entrypoints-failed))]
       ~@body)))

(defn- git [cmd params]
  (git-tool/handle-git (merge {:command cmd :directory gw/root} params)))

(defn- error? [r] (true? (:isError r)))

;;; ---------------------------------------------------------------------------
;;; Finding 1a: a bracket or glob path is a literal path, not a pattern
;;; ---------------------------------------------------------------------------

(deftest a-bracket-path-commits-only-itself
  (doseq [backend [:editor :down]
          include? [false true]]
    (let [w (gw/world {:files next-app})]
      (gw/rm! w "pages/[id].tsx")
      (in-world [w backend]
        (let [r (git "commit" {:message "drop id" :files ["pages/[id].tsx"] :include_staged include?})]
          (is (not (error? r)) (pr-str backend include? (:text r)))))
      (is (= [#{"pages/[id].tsx"}] (map :changes (gw/commits w))) (pr-str backend include?))
      (is (= "i" (get (gw/head w) "pages/i.tsx")))
      (is (= "d" (get (gw/head w) "pages/d.tsx"))))))

(deftest a-glob-is-not-expanded
  (doseq [backend [:editor :down]]
    (let [w (gw/world {:files next-app})
          before (gw/index w)]
      (in-world [w backend]
        (let [r (git "commit" {:message "x" :files ["*.txt"] :include_staged true})]
          (is (error? r))
          (is (str/includes? (:text r) "path-not-found") (:text r))))
      (is (= before (gw/index w)) "the index is untouched")
      (is (empty? (gw/commits w))))))

(deftest stage-of-a-bracket-path-stages-only-itself
  (doseq [backend [:editor :down]]
    (let [w (gw/world {:files next-app})]
      (gw/write! w "pages/[id].tsx" "id2")
      (gw/write! w "pages/i.tsx" "i2")
      (in-world [w backend]
        (let [r (git "stage" {:files ["pages/[id].tsx"]})]
          (is (not (error? r)) (:text r))
          (is (str/includes? (:text r) "Staged 1 path(s): pages/[id].tsx") (:text r))))
      (is (= "id2" (get (gw/index w) "pages/[id].tsx")))
      (is (= "i" (get (gw/index w) "pages/i.tsx")) "i.tsx stays unstaged"))))

;;; ---------------------------------------------------------------------------
;;; Finding 1b: absolute paths resolve against the root; present means present
;;; ---------------------------------------------------------------------------

(deftest an-absolute-path-to-an-edited-file-commits-the-edit
  (doseq [backend [:editor :down]]
    (let [w (gw/world {:files next-app})]
      (gw/write! w "a.txt" "edited")
      (in-world [w backend]
        (let [r (git "commit" {:message "edit a" :files [(str gw/root "/a.txt")] :include_staged true})]
          (is (not (error? r)) (pr-str backend (:text r)))))
      (is (= "edited" (get (gw/head w) "a.txt")) "an edit, never a deletion"))))

(deftest a-path-outside-the-repo-is-refused-and-touches-nothing
  (let [w (gw/world {:files next-app})
        before (gw/index w)]
    (in-world [w :down]
      (let [r (git "commit" {:message "x" :files ["/etc/passwd"]})]
        (is (error? r))
        (is (str/includes? (:text r) "path-outside-repo") (:text r))
        (is (str/includes? (:text r) "Next:"))))
    (is (= before (gw/index w)))))

(deftest a-subdirectory-and-dot-slash-path-resolves-from-the-directory
  (let [w (gw/world {:files {"src/a.txt" "a" "src/b.txt" "b"}})]
    (gw/rm! w "src/a.txt")
    (in-world [w :down]
      (let [r (git-tool/handle-git {:command "commit" :directory (str gw/root "/src")
                                    :message "drop a" :files ["./a.txt"]})]
        (is (not (error? r)) (:text r))))
    (is (= [#{"src/a.txt"}] (map :changes (gw/commits w))))))

;;; ---------------------------------------------------------------------------
;;; Finding 2: a dangling symlink is present, not deleted
;;; ---------------------------------------------------------------------------

(deftest a-retargeted-dangling-symlink-commits-the-retarget
  (doseq [backend [:editor :down]]
    (let [w (gw/world {:files {"t1" "x" "link" {:link "t1"}}})]
      (gw/write! w "link" {:link "missing"})
      (in-world [w backend]
        (let [r (git "commit" {:message "retarget" :files ["link"]})]
          (is (not (error? r)) (pr-str backend (:text r)))))
      (is (= {:link "missing"} (get (gw/head w) "link")) (pr-str backend)))))

;;; ---------------------------------------------------------------------------
;;; Finding 3: stage on the editor path never claims what it did not do
;;; ---------------------------------------------------------------------------

(deftest editor-stage-of-a-path-that-names-nothing-is-an-error
  (let [w (gw/world {:files next-app})]
    (in-world [w :editor]
      (let [r (git "stage" {:files ["nope.txt"]})]
        (is (error? r))
        (is (str/includes? (:text r) "path-not-found") (:text r))))))

(deftest editor-stage-of-an-ignored-path-is-an-error
  (let [w (gw/world {:files next-app :ignore ["*.log"]})]
    (gw/write! w "ign.log" "x")
    (in-world [w :editor]
      (let [r (git "stage" {:files ["ign.log"]})]
        (is (error? r))
        (is (str/includes? (:text r) "ignored") (:text r))))
    (is (not (contains? (gw/index w) "ign.log")))))

(deftest editor-stage-of-an-unchanged-path-says-nothing-to-stage
  (let [w (gw/world {:files next-app})]
    (in-world [w :editor]
      (let [r (git "stage" {:files ["a.txt"]})]
        (is (not (str/includes? (:text r) "Staged")) (:text r))
        (is (str/includes? (:text r) "Nothing to stage for a.txt") (:text r))))))

(deftest stage-all-never-answers-blank
  (doseq [backend [:editor :down]]
    (let [w (gw/world {:files next-app})]
      (gw/write! w "a.txt" "a2")
      (in-world [w backend]
        (is (not (str/blank? (:text (git "stage" {:files "all"})))) (pr-str backend))))))

;;; ---------------------------------------------------------------------------
;;; Findings 6, 7, 9: deleted directory; refusals leave the index as found
;;; ---------------------------------------------------------------------------

(deftest a-deleted-directory-commits-as-a-deletion
  (let [w (gw/world {:files {"lib/x.clj" "x" "lib/y.clj" "y" "a.txt" "a"}})]
    (gw/rm! w "lib/x.clj")
    (gw/rm! w "lib/y.clj")
    (in-world [w :down]
      (let [r (git "commit" {:message "drop lib" :files ["lib"]})]
        (is (not (error? r)) (:text r))))
    (is (= [#{"lib/x.clj" "lib/y.clj"}] (map :changes (gw/commits w))))))

(deftest a-refused-commit-puts-the-index-back
  (testing "a deletion beside an unchanged path (finding 7)"
    (let [w (gw/world {:files next-app})
          before (gw/index w)]
      (gw/rm! w "a.txt")
      (in-world [w :down]
        (let [r (git "commit" {:message "x" :files ["a.txt" "b.txt"]})]
          (is (not (error? r)) "a.txt is a real change; b.txt just has none")))
      (is (= [#{"a.txt"}] (map :changes (gw/commits w))))
      (is (not= before (gw/index w)))))
  (testing "a foreign staged path refuses, and the deletion staged for the call is undone"
    (let [w (gw/world {:files next-app})]
      (gw/write! w "b.txt" "b2")
      (in-world [w :down] (git "stage" {:files ["b.txt"]}))
      (gw/rm! w "a.txt")
      (let [before (gw/index w)]
        (in-world [w :down]
          (let [r (git "commit" {:message "x" :files ["a.txt"]})]
            (is (error? r))
            (is (str/includes? (:text r) "unrelated-staged") (:text r))))
        (is (= before (gw/index w)) "only b.txt is staged, as before the call")
        (is (empty? (gw/commits w)))))))

;;; ---------------------------------------------------------------------------
;;; Finding 4: the error text says what happened
;;; ---------------------------------------------------------------------------

(deftest the-error-text-claims-a-retry-only-when-one-ran
  (let [w (gw/world {:files next-app})]
    (testing "fast failure, retried, failed again: down, and says so"
      (in-world [w :down]
        (let [r (git "commit" {:message "nothing"})]
          (is (error? r))
          (is (str/includes? (:text r) "bridge down") (:text r))
          (is (str/includes? (:text r) "again on one retry") (:text r))
          (is (not (str/includes? (:text r) "retryable")) (:text r)))))
    (testing "slow failure, not retried: never claims a retry"
      (binding [magit/*handshake-race-ms* 0]
        (in-world [w :down]
          (let [r (git "commit" {:message "nothing"})]
            (is (str/includes? (:text r) "was not retried") (:text r))
            (is (not (str/includes? (:text r) "retry already ran")) (:text r))))))))

;;; ---------------------------------------------------------------------------
;;; Finding 5: a bridge seen down is remembered, not paid for on every op
;;; ---------------------------------------------------------------------------

(deftest a-bridge-seen-down-is-skipped-for-the-next-ops
  (let [w (gw/world {:files next-app})]
    (gw/write! w "a.txt" "a2")
    (gw/write! w "b.txt" "b2")
    (binding [magit/*repo-port* (gw/port w)
              magit/*handshake-race-ms* 0
              magit/*bridge-down-ms* 60000]
      (swarm/with-swarm-host [h (constantly entrypoints-failed)]
        (git-tool/handle-git {:command "batch-commit" :directory gw/root
                              :operations [{:message "a" :files ["a.txt"]}
                                           {:message "b" :files ["b.txt"]}]})
        (is (= 1 (count (swarm/calls h))) "only the first op asked the editor")))
    (is (= ["a" "b"] (map :message (gw/commits w))))))

;;; ---------------------------------------------------------------------------
;;; CONTRACT: stage and commit never touch a path the caller did not name
;;; ---------------------------------------------------------------------------

(def ^:private shapes
  "Path shapes that each broke a stage once: patterns, absolute and ./
   spellings, a directory, a link, deletions and edits beside intact files."
  [{:name "bracket, deleted" :prep #(gw/rm! % "pages/[id].tsx") :files ["pages/[id].tsx"]}
   {:name "bracket, edited" :prep #(gw/write! % "pages/[id].tsx" "e") :files ["pages/[id].tsx"]}
   {:name "star" :prep #(gw/write! % "a.txt" "e") :files ["*.txt"]}
   {:name "question mark" :prep #(gw/write! % "a.txt" "e") :files ["?.txt"]}
   {:name "absolute, edited" :prep #(gw/write! % "a.txt" "e") :files [(str gw/root "/a.txt")]}
   {:name "absolute, deleted" :prep #(gw/rm! % "a.txt") :files [(str gw/root "/a.txt")]}
   {:name "dot slash" :prep #(gw/write! % "a.txt" "e") :files ["./a.txt"]}
   {:name "directory" :prep #(gw/write! % "pages/i.tsx" "e") :files ["pages"]}
   {:name "dangling link" :prep #(gw/write! % "link" {:link "gone"}) :files ["link"]}
   {:name "deleted beside edited" :prep #(do (gw/rm! % "a.txt") (gw/write! % "b.txt" "e"))
    :files ["a.txt"]}
   {:name "unknown" :prep identity :files ["nope.txt"]}
   {:name "magic prefix" :prep #(gw/write! % "a.txt" "e") :files [":(glob)*.txt"]}])

(defn- named? [files path]
  (let [rel (fn [f] (-> f
                        (str/replace (re-pattern (str "^" (java.util.regex.Pattern/quote gw/root) "/?")) "")
                        (str/replace #"^\./" "")
                        (str/replace #"/$" "")))]
    (some (fn [f] (let [r (rel f)] (or (= r path) (str/starts-with? path (str r "/"))))) files)))

(defn- check-contract [label w files before-index]
  (let [after (gw/index w)
        touched (filter #(not= (get before-index %) (get after %))
                        (distinct (concat (keys before-index) (keys after))))
        committed (mapcat :changes (gw/commits w))]
    (is (every? #(named? files %) touched)
        (str label ": the index changed for paths not named: "
             (pr-str (remove #(named? files %) touched))))
    (is (every? #(named? files %) committed)
        (str label ": a commit recorded paths not named: "
             (pr-str (remove #(named? files %) committed))))
    (is (every? (fn [p] (or (contains? after p) (not (contains? (:disk @w) p)) (not (contains? before-index p))))
                (keys before-index))
        (str label ": a path present on disk was dropped from the index"))))

(deftest contract-stage-and-commit-touch-only-named-paths
  (doseq [{:keys [name prep files]} shapes
          backend [:editor :down]
          [verb params] [["stage" {}]
                         ["commit" {:message "c"}]
                         ["commit" {:message "c" :include_staged true}]]]
    (let [label (str name " / " (clojure.core/name backend) " / " verb
                     (when (:include_staged params) " include_staged"))
          w (gw/world {:files (assoc next-app "link" {:link "a.txt"})})
          _ (prep w)
          before (gw/index w)]
      (in-world [w backend]
        (let [r (git verb (assoc params :files files))]
          (is (string? (:text r)) label)
          (when (error? r)
            (is (= before (gw/index w)) (str label ": a refusal left the index changed: " (:text r))))))
      (check-contract label w files before))))
