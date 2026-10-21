(ns hive-mcp.agent.agent-definition.frontmatter-test
  "Trifectas for the YAML frontmatter split and its coercion into an agent
   definition, driven by the contracts on
   `hive-mcp.agent.agent-definition.frontmatter`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.agent.agent-definition.frontmatter :as fm]
            [hive-mcp.agent.agent-definition.spec :as spec]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private FmText
  "Markdown sources: half carry a `---` frontmatter block (with or without a
   description), half are plain text, so both branches of the split are hit."
  [:string {:gen/schema [:tuple [:enum :fm :raw] [:enum "scout" "a1" "x-y"]
                         [:enum "finds" "d1" ""] [:enum "body" " pad " "" "line1\nline2"]]
            :gen/fmap   (fn [[k n d b]]
                          (if (= k :fm)
                            (str "---\nname: " n "\n"
                                 (when (seq d) (str "description: " d "\n"))
                                 "---\n" b)
                            (str n " " b)))}])

(deftrifecta-from-schema parse-frontmatter-trifecta
  hive-mcp.agent.agent-definition.frontmatter/parse-frontmatter
  {:in          FmText
   :out         spec/FrontmatterResult
   :rel         (fn [raw out]
                  (if (str/starts-with? raw "---\n")
                    (and (string? (get (:frontmatter out) :name))
                         (= (:content out) (str/trim (last (str/split raw #"---\n" 3)))))
                    (and (= {} (:frontmatter out)) (= raw (:content out)))))
   :num-tests   200
   :n-cases     12
   :golden-path "test/golden/agent/frontmatter-parse.edn"})

(def ^:private Frontmatter
  [:map
   [:name {:optional true} [:enum "scout" "a1"]]
   [:description {:optional true} [:enum "finds" "d1\\nmore"]]
   [:tools {:optional true} [:or [:enum "*" "read"] [:vector {:max 3} [:enum "x" "y" "*"]]]]
   [:model {:optional true} [:enum "inherit" "INHERIT" " opus " "" nil]]
   [:max-turns {:optional true} [:enum 7 0 -1 "12" "x"]]
   [:skills {:optional true} [:enum "a, b" "" nil]]
   [:hivemind-role {:optional true} [:enum "role/worker" :role/hivemind "nope"]]])

(defn- expected-tools [t]
  (cond (nil? t) nil
        (string? t) (when-not (= "*" t) [t])
        :else (when-not (some #{"*"} t) (mapv str t))))

(deftrifecta-from-schema frontmatter->agent-def-trifecta
  hive-mcp.agent.agent-definition.frontmatter/frontmatter->agent-def
  {:in          [:cat Frontmatter [:enum "" "  " "You scout."] spec/Source]
   :out         spec/CoerceResult
   :rel         (fn [[f content source] out]
                  (cond
                    (nil? (:name f)) (nil? out)
                    (nil? (:description f)) (str/includes? (:error out) (:name f))
                    :else
                    (and (= (:name f) (:agent-type out))
                         (= source (:source out))
                         (not (str/includes? (:description out) "\\n"))
                         (= (if (str/blank? content) (str "You are " (:name f) ".") content)
                            (:system-prompt out))
                         (= (expected-tools (:tools f)) (:tools out))
                         (= (when (#{7 "12"} (:max-turns f)) (if (= "12" (:max-turns f)) 12 7))
                            (:max-turns out))
                         (= (case (:model f) ("inherit" "INHERIT") :inherit " opus " "opus" nil)
                            (:model out))
                         (= (case (:hivemind-role f) "role/worker" :role/worker
                                  :role/hivemind :role/hivemind nil)
                            (:hivemind-role out)))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/agent/frontmatter-agent-def.edn"})

(deftest file-path-and-base-dir-test
  (is (= {:filename "scout" :base-dir "/x"}
         (select-keys (fm/frontmatter->agent-def {"name" "s" "description" "d"} "" :user
                                                 {:file-path "/x/scout.md" :base-dir "/x"})
                      [:filename :base-dir]))))
