(ns hive-mcp.presets.core-test
  "Name-matched facets for the preset core. extract-preset-core gets a
   schema-driven trifecta over generated markdown; the file fallback
   (scan-presets-dir, get-preset-from-file) gets a golden over a temp
   presets directory. The vector-store fns are exercised against the
   in-memory stub elsewhere (presets m16 facets and tools tests)."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.presets.core :as presets]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Markdown
  ;; A title, an optional paragraph, then 0..7 bullets: reaches the
  ;; no-summary, no-bullets and capped-at-five arms.
  [:string {:gen/schema [:tuple [:enum "# Testing" "# Refactor" ""]
                         [:enum "" "Write the test first." "Keep layers pure."]
                         [:vector {:max 7} [:enum "a" "b" "c"]]]
            :gen/fmap (fn [[title para bullets]]
                        (str/join "\n" (concat [title "" para ""] (map #(str "- " %) bullets))))}])

(def ^:private PresetIn
  [:map [:name [:enum "tdd" "clarity"]]
   [:category {:optional true} [:enum "testing" "coding"]]
   [:content Markdown]])

(deftrifecta-from-schema extract-preset-core-summarises
  #'presets/extract-preset-core
  {:in [:cat PresetIn]
   :out [:map [:title :any] [:category :string] [:summary :string] [:key-points [:vector :string]]]
   :rel (fn [[{:keys [name category content]}] out]
          (let [bullets (keep #(second (re-matches #"- (.+)" %)) (str/split-lines content))
                para    (some #(when (#{"Write the test first." "Keep layers pure."} %) %)
                              (str/split-lines content))]
            (and (= name (:name out) (:title out))
                 (if category (= category (:category out)) (string? (:category out)))
                 (= (vec (take 5 bullets)) (:key-points out))
                 (= (or para "No summary available") (:summary out)))))
   :classify (fn [_ out] (cond (= 5 (count (:key-points out))) :capped
                               (seq (:key-points out)) :some-bullets
                               :else :no-bullets))
   :classify-domain #{:capped :some-bullets :no-bullets}
   :golden-path "test/golden/presets/core-extract-preset-core.edn"
   :num-tests 300 :classify-floor 13})

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "presets-core" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- scenario []
  (let [dir (temp-dir)]
    (try
      (spit (io/file dir "tdd.md") "# TDD Discipline\n\n**red-green** cycles.\n\n## Small Steps\n- write a test\n")
      (spit (io/file dir "ddd.md") "Bounded context and aggregate design.\n")
      (spit (io/file dir "README.md") "# not a preset")
      (spit (io/file dir "notes.txt") "ignored")
      (let [strip #(dissoc % :file-path)]
        {:scanned     (->> (presets/scan-presets-dir (.getPath dir)) (map strip) (sort-by :id) vec)
         :from-file   (strip (presets/get-preset-from-file (.getPath dir) "tdd"))
         :missing     (presets/get-preset-from-file (.getPath dir) "nope")
         :not-a-dir   (presets/scan-presets-dir (str (.getPath dir) "/tdd.md"))})
      (finally (rm-rf dir)))))

(deftest-golden-fn file-fallback-golden
  "test/golden/presets/core-file-fallback.edn"
  scenario)

(deftest reset-collection-cache-answers-nil
  (is (nil? (presets/reset-collection-cache!))))
