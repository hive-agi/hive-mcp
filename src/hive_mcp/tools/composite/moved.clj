(ns hive-mcp.tools.composite.moved
  "Where a tool's deprecated addon commands went, as one short description line.

   A deprecated contribution routes but is not advertised
   (`hive-mcp.tools.composite/deprecated-contribution?`). Its summary usually
   names the root that replaced it (\"Deprecated: use `cloak status`.\"), and
   that summary was the only text an agent read that tied the old topic words
   to the new tool. This namespace keeps that link for a few dozen bytes.

   Measured 2026-10-23 (hive-assay advert lab r3, deepseek-v4-1-flash-1017):
   with the `cloak` root listed, a question about the \"default enclave\" of
   memory \"sealing\" still took 10-22 turns, through memory search and source
   greps, before the agent reached `cloak status`."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private replacement-marker
  "What precedes the replacement root in a deprecation text. Compared
   case-insensitively."
  "use `")

(defn- root-char? [c]
  (or (Character/isLetterOrDigit (char c)) (= \- c) (= \_ c)))

(defn replacement-root
  "The root tool deprecation TEXT points to: the first word inside the
   backticks after `use`, lower-cased. nil when it names none."
  [text]
  (let [lower (some-> text str/lower-case)
        start (some-> lower (str/index-of replacement-marker))]
    (when start
      (let [word (apply str (take-while root-char? (subs lower (+ start (count replacement-marker)))))]
        (when-not (str/blank? word) word)))))

(defn- topic-word
  "The first hyphen-separated word of command name CMD: `work-handoff-push` -> `work`."
  [cmd]
  (if-let [i (str/index-of cmd "-")] (subs cmd 0 i) cmd))

(defn moved-commands-text
  "DEPRECATED ([command-name deprecation-text] ...) as one line per replacement
   root: the root plus the topic words (first word of each command name) that
   now live there. Commands whose text names no root are skipped. nil when
   nothing is left."
  [deprecated]
  (let [by-root (reduce (fn [acc [cmd text]]
                          (if-let [root (replacement-root text)]
                            (update acc root (fnil conj (sorted-set)) (topic-word cmd))
                            acc))
                        (sorted-map)
                        deprecated)]
    (when (seq by-root)
      (str/join " " (for [[root topics] by-root]
                      (str "Moved to the `" root "` tool (call it with command=\"help\"): "
                           (str/join ", " topics) "."))))))

(m/=> replacement-root [:=> [:cat [:maybe :string]] [:maybe :string]])

(m/=> moved-commands-text
      [:=> [:cat [:maybe [:sequential [:tuple :string [:maybe :string]]]]] [:maybe :string]])
