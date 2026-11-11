(ns hive-mcp.test.stub.git-world
  "An in-memory git repository and worktree for the git verbs' `*repo-port*`
   (hive-mcp.tools.magit), so the default suite reaches no real repository
   (convention 20261111105238-0ceaa524).

   The world is an atom of
     {:root \"/w\" :head {path content} :index {path content}
      :wt {path content} :ignored #{path} :commits [message ...]}
   where a content is a string, or [:link target] for a symlink.

   Pathspecs follow git: WITH --literal-pathspecs a spec names that path or
   the directory of that name; WITHOUT it `*`, `?` and `[...]` are globs, so
   a verb that forgets the flag stages other people's files here exactly as
   it would in a real repo.

   API:
     (world files)            a world whose HEAD, index and worktree hold FILES
     (port w)                 the `*repo-port*` map over W
     (git w dir args)         run one fake git command {:exit :out}
     (vessel w)               a swarm-host RESPOND emulating hive-emacs's elisp
                              (no literal pathspecs, file-exists-p follows links)
     with-world               macro: bind *repo-port* to (port w) for a body"
  (:require [clojure.string :as str]
            [hive-mcp.tools.magit :as magit]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def root "/w")

(defn world
  "A world rooted at `root` with FILES ({path content}) committed and clean."
  ([files] (world files {}))
  ([files {:keys [ignored]}]
   (atom {:root root :head files :index files :wt files
          :ignored (set ignored) :commits ["base"]})))

;;; ---------------------------------------------------------------------------
;;; Paths and pathspecs
;;; ---------------------------------------------------------------------------

(defn- normalize [p]
  (let [parts (reduce (fn [acc s]
                        (case s
                          ("" ".") acc
                          ".." (if (seq acc) (pop acc) acc)
                          (conj acc s)))
                      [] (str/split p #"/"))]
    (if (empty? parts) "." (str/join "/" parts))))

(defn- under [base p]
  (when (or (= base p) (str/starts-with? p (str base "/")))
    (if (= base p) "." (subs p (inc (count base))))))

(defn- prefix-of [w dir]
  (let [r (under (:root @w) (normalize dir))]
    (if (or (nil? r) (= "." r)) "" (str r "/"))))

(defn- spec->rel
  "Root-relative form of pathspec S given in DIR, as git reads it."
  [w dir s]
  (if (str/starts-with? s "/")
    (or (under (subs (:root @w) 1) (subs (normalize s) 0)) (str "<outside>" s))
    (normalize (str (prefix-of w dir) s))))

(defn- glob->re [s]
  (re-pattern
   (str/join (map (fn [c] (case c \* ".*" \? "." \[ "[" \] "]" (java.util.regex.Pattern/quote (str c)))) s))))

(defn- matcher
  "A predicate on root-relative paths for spec REL. Literal: the path or a
   directory of that name. Otherwise git's glob magic applies as well."
  [rel literal?]
  (let [lit (fn [f] (or (= "." rel) (= f rel) (str/starts-with? f (str rel "/"))))]
    (if literal?
      lit
      (let [re (glob->re rel)] (fn [f] (or (lit f) (boolean (re-matches re f))))))))

(defn- matching [paths rel literal?]
  (filter (matcher rel literal?) paths))

;;; ---------------------------------------------------------------------------
;;; The fake git
;;; ---------------------------------------------------------------------------

(defn- ok [lines z?] {:exit 0 :out (str/join (if z? "\u0000" "\n") (sort lines))})

(defn- split-specs [args]
  (let [[opts [_ & specs]] (split-with #(not= "--" %) args)] [(vec opts) (vec specs)]))
