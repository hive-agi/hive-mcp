(ns hive-mcp.plan.parser.markdown-test
  "Facets for the markdown plan parser. Plans are generated as data (titles
   with optional [id:] [priority:] [estimate:] [depends:] annotations) and
   rendered to markdown, so the relation can check every annotation was read
   back. parse-markdown-plan mints a timestamp plan :id, so its trifecta has
   no seeded golden; the golden below projects the id away. The plan field
   registry is emptied around each test."
  (:require [clojure.string :as str]
            [clojure.test :refer [use-fixtures]]
            [hive-mcp.plan.field-registry :as fr]
            [hive-mcp.plan.parser.markdown :as md]
            [hive-schemas.test :as hst]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- registry-fixture [f]
  (let [saved @@#'fr/registry]
    (fr/reset-registry!)
    (try (f) (finally (reset! @#'fr/registry saved)))))

(use-fixtures :each registry-fixture)

(defn- out-schema [fsym]
  (-> (m/function-schemas) (get-in ['hive-mcp.plan.parser.markdown fsym :schema]) m/children second))

(def ^:private StepData
  [:map
   [:title [:enum "Setup" "Build thing" "Write tests"]]
   [:id {:optional true} [:enum "t1" "t2"]]
   [:priority {:optional true} [:enum :high :medium :low]]
   [:estimate {:optional true} [:enum :small :large]]
   [:depends-on {:optional true} [:vector {:min 1 :max 2} [:enum "t1" "t2"]]]
   [:body {:optional true} [:enum "Some details." "More notes."]]])

(def ^:private DocData
  [:map
   [:title {:optional true} [:enum "My Plan" "Other Plan"]]
   [:steps [:vector {:max 3} StepData]]])

(defn- render-step [{:keys [title id priority estimate depends-on body]}]
  (str "## " title
       (when id (str " [id: " id "]"))
       (when priority (str " [priority: " (name priority) "]"))
       (when estimate (str " [estimate: " (name estimate) "]"))
       (when depends-on (str " [depends: " (str/join ", " depends-on) "]"))
       "\n" (when body (str body "\n"))))

(defn- render [{:keys [title steps]}]
  (str (when title (str "# " title "\n\n"))
       "Preamble text.\n\n"
       (str/join "\n" (map render-step steps))))

(def ^:private Markdown
  [:string {:gen/schema DocData :gen/fmap render}])

(defn- read-back
  "Recover the generated data from its rendering, independently of the
   parser: header lines by prefix, annotations as `[k: v]` substrings."
  [content]
  (let [lines (str/split-lines content)
        title (some #(when (str/starts-with? % "# ") (subs % 2)) lines)
        ann (fn [text k]
              (when-let [i (str/index-of text (str "[" k ": "))]
                (let [start (+ i (count k) 3)]
                  (subs text start (str/index-of text "]" start)))))
        strip (fn [text]
                (loop [t text]
                  (if-let [i (str/index-of t " [")]
                    (recur (str (subs t 0 i) (subs t (inc (str/index-of t "]" i)))))
                    (str/trim t))))
        steps (for [line lines :when (str/starts-with? line "## ")
                    :let [text (subs line 3)]]
                {:title (strip text)
                 :id (ann text "id")
                 :priority (some-> (ann text "priority") keyword)
                 :estimate (some-> (ann text "estimate") keyword)
                 :depends-on (some-> (ann text "depends") (str/split #", ") vec)})]
    {:title title :steps (vec steps)}))

(hst/deftrifecta-from-schema parse-markdown-plan-contract
  hive-mcp.plan.parser.markdown/parse-markdown-plan
  {:in [:cat Markdown]
   :out (out-schema 'parse-markdown-plan)
   :rel (fn [[content] out]
          (let [{:keys [title steps]} (read-back content)]
            (if (empty? steps)
              (and (false? (:success out)) (= "No ## headers found in content" (:error out)))
              (let [plan (:plan out)]
                (and (:success out)
                     (= (or title "Untitled Plan") (:title plan))
                     (= :markdown (:source-format plan))
                     (= (count steps) (count (:steps plan)))
                     (every? true?
                             (map (fn [s p]
                                    (and (= (:title s) (:title p))
                                         (or (nil? (:id s)) (= (:id s) (:id p)))
                                         (= (or (:priority s) :medium) (:priority p))
                                         (= (or (:estimate s) :medium) (:estimate p))
                                         (= (or (:depends-on s) []) (:depends-on p))))
                                  steps (:steps plan))))))))
   :classify (fn [_ out] (if (:success out) :parsed :no-steps))
   :classify-domain #{:parsed :no-steps} :classify-floor 13 :num-tests 150})

(hst/deftrifecta-from-schema extract-title-contract
  hive-mcp.plan.parser.markdown/extract-title
  {:in [:cat Markdown]
   :out (out-schema 'extract-title)
   :rel (fn [[content] out] (= out (:title (read-back content))))
   :classify (fn [_ out] (if out :titled :untitled))
   :classify-domain #{:titled :untitled}
   :golden-path "test/golden/hive-mcp/plan/parser/markdown-extract-title.edn"})

(golden/deftest-golden parse-markdown-plan-golden
  "test/golden/hive-mcp/plan/parser/markdown-parse.edn"
  (let [view (fn [content]
               (let [r (md/parse-markdown-plan content)]
                 (cond-> r (:success r) (update :plan dissoc :id))))]
    {:annotated (view "# Plan\n## Add validation [id: t1] [priority: high] [files: src/a.clj, src/b.clj]\nBody\n### Sub detail\n## Tests [depends: t1] [estimate: small]")
     :edn-overlay (view "# P\n## Step one\n{:id \"x1\" :priority :low :files [\"f.clj\"] :ignored 1}\nRest of body")
     :blank-slug (view "## !!!\n")
     :no-headers (view "# Only a title\nprose")}))
