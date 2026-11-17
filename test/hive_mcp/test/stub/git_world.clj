(ns hive-mcp.test.stub.git-world
  "An in-memory git repository: index, HEAD, working tree and .gitignore as
   plain data, answering the git argv the hive git verbs issue. Bound as
   `hive-mcp.tools.magit/*repo-port*`, so the default suite stages and commits
   with no repository, no git binary and no disk.

   Pathspecs behave as in git: a path is a GLOB (`*`, `?`, `[...]`) unless the
   run carries --literal-pathspecs, and an absolute path inside the root
   counts as that path. That is the semantics the wave-1111 data loss came
   from, so a test against this world catches it.

   API:
     (world {:files {path content} :head {path content} ...})  a world atom
       content is a string, or {:link target} for a symlink
     (port w)            the *repo-port* map for W
     (editor w)          a vessel RESPOND that behaves like hive-emacs Magit:
                         no --literal-pathspecs, stage answers \"nil\"
     (index w) (head w) (commits w) (runs w)
     (rm! w path) (write! w path content)"
  (:require [clojure.data.json :as json]
            [clojure.string :as str]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def root "/fake/repo")

(defn world
  "A world whose HEAD and index hold HEAD (default: FILES) and whose disk holds FILES."
  [{:keys [files head index ignore] :or {ignore []}}]
  (let [head (or head files)]
    (atom {:disk files :head head :index (or index head) :ignore ignore
           :commits [] :runs []})))

(defn index [w] (:index @w))
(defn head [w] (:head @w))
(defn commits [w] (:commits @w))
(defn runs [w] (:runs @w))
(defn rm! [w path] (swap! w update :disk dissoc path))
(defn write! [w path content] (swap! w assoc-in [:disk path] content))

(defn- glob->re [spec]
  (re-pattern
   (apply str (map (fn [c]
                     (case c
                       \* ".*" \? "." \[ "[" \] "]"
                       (java.util.regex.Pattern/quote (str c))))
                   spec))))

(defn- rel-spec
  "SPEC from directory DIR as a root-relative spec."
  [dir spec]
  (let [prefix (if (= dir root) "" (subs dir (inc (count root))))
        s (cond
            (str/starts-with? spec (str root "/")) (subs spec (inc (count root)))
            (= spec root) "."
            (str/starts-with? spec "/") (str "\u0000outside" spec)
            :else (str (when (seq prefix) (str prefix "/")) spec))
        s (str/replace s #"(^|/)\./" "$1")
        s (str/replace s #"/\.$" "")
        s (str/replace s #"/+$" "")]
    (if (or (= s "") (= s ".")) "." s)))

(defn- matches?
  [literal? spec path]
  (or (= spec ".")
      (= path spec)
      (str/starts-with? path (str spec "/"))
      (and (not literal?) (boolean (re-matches (glob->re spec) path)))))

(defn- ignored? [w path]
  (some #(re-matches (glob->re %) path) (:ignore @w)))

(defn- disk-dirs [disk]
  (set (mapcat (fn [p] (let [parts (str/split p #"/")]
                         (map #(str/join "/" (take % parts)) (range 1 (count parts)))))
               (keys disk))))

(defn kind
  "The :kind port: what sits at ABS, never following a link."
  [w abs]
  (let [disk (:disk @w)
        rel (rel-spec root abs)]
    (cond
      (contains? disk rel)
      (let [c (get disk rel)]
        (if (map? c)
          {:type :symlink :target-exists? (contains? disk (:link c))}
          {:type :file}))
      (or (= rel ".") (contains? (disk-dirs disk) rel)) {:type :dir}
      :else {:type :absent})))

(defn- lines [xs] (str (str/join "\n" xs) (when (seq xs) "\n")))
(defn- ok [out] {:exit 0 :out out})
(defn- fail [code out] {:exit code :out out})

(defn- split-args
  "[flags paths] around \"--\"."
  [args]
  (let [[a b] (split-with #(not= "--" %) args)]
    [(vec a) (vec (rest b))]))

(defn- sel [literal? specs paths]
  (if (empty? specs)
    (sort paths)
    (sort (filter (fn [p] (some #(matches? literal? % p) specs)) paths))))

(defn- changed [a b]
  (filter #(not= (get a %) (get b %)) (distinct (concat (keys a) (keys b)))))

(defn- stage-specs
  "git add -A -- SPECS: index takes the disk for every path SPECS name."
  [w literal? specs]
  (let [{:keys [disk index]} @w
        known (distinct (concat (keys disk) (keys index)))
        hit (sel literal? specs (remove #(and (not (contains? index %)) (ignored? w %)) known))
        unmatched (remove (fn [s] (some #(matches? literal? s %) known)) specs)
        ign (filter (fn [s] (and (some #(matches? literal? s %) (keys disk))
                                 (not-any? #(matches? literal? s %) hit)))
                    specs)]
    (cond
      (seq unmatched) (fail 128 (str "fatal: pathspec '" (first unmatched) "' did not match any files\n"))
      (seq ign) (fail 1 (str "The following paths are ignored by one of your .gitignore files:\n"
                             (str/join "\n" ign) "\n"))
      :else (do (swap! w update :index
                       (fn [ix] (reduce (fn [ix p] (if (contains? disk p)
                                                     (assoc ix p (get disk p))
                                                     (dissoc ix p)))
                                        ix hit)))
                (ok "")))))

(defn- commit! [w all? msg]
  (when all?
    (swap! w (fn [{:keys [disk] :as s}]
               (update s :index
                       (fn [ix] (reduce (fn [ix p] (if (contains? disk p)
                                                     (assoc ix p (get disk p))
                                                     (dissoc ix p)))
                                        ix (keys ix)))))))
  (let [{:keys [index head]} @w]
    (if (= index head)
      (fail 1 "nothing to commit, working tree clean\n")
      (do (swap! w (fn [s] (-> s
                               (update :commits conj {:message (str/trim (str msg))
                                                      :changes (set (changed head index))})
                               (assoc :head index))))
          (ok (str "[main] " (str/trim (str msg)) "\n"))))))

(defn run
  "The :run port: one git ARGS in DIR."
  [w dir args in _timeout-ms]
  (swap! w update :runs conj (vec args))
  (let [literal? (some #{"--literal-pathspecs"} args)
        args (vec (remove #{"--literal-pathspecs"} args))
        [flags raw] (split-args args)
        specs (mapv #(rel-spec dir %) raw)
        {:keys [disk index head]} @w
        inside? (or (= dir root) (str/starts-with? dir (str root "/")))]
    (cond
      (not inside?) (fail 128 "fatal: not a git repository\n")
      (some #(str/starts-with? % "\u0000outside") specs)
      (fail 128 (str "fatal: " (first raw) ": is outside repository\n"))

      (= flags ["rev-parse" "--show-toplevel"]) (ok (str root "\n"))
      (= flags ["rev-parse" "--show-prefix"])
      (ok (str (if (= dir root) "" (str (subs dir (inc (count root))) "/")) "\n"))
      (= flags ["rev-parse" "--git-path" "index"]) (ok ".git/index\n")

      (= flags ["ls-files" "--cached"]) (ok (lines (sel literal? specs (keys index))))
      (= flags ["ls-files" "-s" "-z"])
      (ok (apply str (map (fn [p] (str "100644 " (Integer/toHexString (hash (get index p))) " 0\t" p "\u0000"))
                          (sort (keys index)))))
      (= flags ["ls-files" "--others" "--exclude-standard"])
      (ok (lines (sel literal? specs (remove #(or (contains? index %) (ignored? w %)) (keys disk)))))
      (= flags ["ls-files" "--others" "--ignored" "--exclude-standard"])
      (ok (lines (sel literal? specs (filter #(and (not (contains? index %)) (ignored? w %)) (keys disk)))))
      (= flags ["ls-tree" "-r" "--name-only" "HEAD"]) (ok (lines (sel literal? specs (keys head))))

      (= flags ["diff" "--cached" "--name-only"]) (ok (lines (sel literal? specs (changed head index))))
      (= flags ["diff" "--cached" "--name-only" "--no-renames" "-z"])
      (ok (apply str (map #(str % "\u0000") (sort (changed head index)))))
      (= flags ["diff" "--name-only"])
      (ok (lines (sel literal? specs (filter #(not= (get index %) (get disk %)) (keys index)))))

      (= (take 2 flags) ["rm" "--cached"])
      (let [hit (sel literal? specs (keys index))
            r? (some #{"-r"} flags)
            dir-hit (some (fn [s] (some #(str/starts-with? % (str s "/")) hit)) specs)]
        (if (and dir-hit (not r?))
          (fail 128 (str "fatal: not removing '" dir-hit "' recursively without -r\n"))
          (do (swap! w update :index #(apply dissoc % hit)) (ok ""))))

      (and (= "add" (first flags)) (seq specs)) (stage-specs w literal? specs)
      (= flags ["add" "-A"]) (stage-specs w literal? ["."])

      (= "commit" (first flags)) (commit! w (some #{"-a"} flags) in)

      :else (fail 129 (str "git-world: unsupported argv " (pr-str args) "\n")))))

(defn port
  "The `hive-mcp.tools.magit/*repo-port*` map over W."
  [w]
  {:run (fn [dir args in timeout-ms] (run w dir args in timeout-ms))
   :kind (fn [abs] (kind w abs))
   :snapshot-index (fn [_root] (:index @w))
   :restore-index! (fn [_root snap] (swap! w assoc :index snap))})

(defn editor
  "A vessel RESPOND behaving like the hive-emacs Magit translators over W:
   paths reach git WITHOUT --literal-pathspecs, `file-exists-p` follows links,
   and api-stage answers \"nil\" whatever git said."
  [w]
  (fn [op _timeout-ms]
    (let [dir (:directory op)
          g (fn [& args] (run w dir (vec args) nil nil))]
      (case (:op op)
        :magit/stage
        (do (if (= :all (:files op))
              (g "add" "-A")
              (doseq [f (:files op)] (g "add" "--" f)))
            {:success true :timed-out false :result "nil"})

        :magit/stage-verify
        (let [ps (:paths op)
              exists? (fn [p] (let [{:keys [type target-exists?]} (kind w (str dir "/" p))]
                                (or (#{:file :dir} type) (and (= :symlink type) target-exists?))))
              missing (first (remove exists? ps))]
          {:success true :timed-out false
           :result (json/write-str
                    (if missing
                      {:status "missing" :path missing}
                      (do (doseq [p ps] (g "add" "--" p))
                          {:status (if (str/blank? (:out (apply g "diff" "--cached" "--name-only" "--" ps)))
                                     "empty" "ok")})))})

        :magit/commit
        (let [r (commit! w (:all op) (:message op))]
          (if (zero? (:exit r))
            {:success true :timed-out false :result (:out r)}
            {:success false :timed-out false :error (:out r)}))

        {:success false :timed-out false :error (str "git-world editor: unscripted op " (:op op))}))))
