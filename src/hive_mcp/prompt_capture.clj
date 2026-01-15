(ns hive-mcp.prompt-capture
  "Prompt Engineering Knowledge Base - Clojure-native prompt capture system.

   Captures well-structured LLM prompts with analysis for RAG-based prompt improvement.

   Features:
   - Schema validation with Malli (delegated to hive-mcp.prompt.validation)
   - Taxonomy categories (coding, debug, planning, meta, research, config)
   - Quality filtering (success/failure/partial)
   - Org-mode storage via org-clj
   - Confirmation display after save

   Usage:
     (capture-prompt {:prompt \"...\", :accomplishes \"...\", ...})
     (list-prompts)
     (list-prompts {:category :coding})
     (search-prompts \"keyword\")"
  (:require [clojure.string :as str]
            [hive-mcp.org-clj.parser :as parser]
            [hive-mcp.org-clj.writer :as writer]
            [hive-mcp.prompt.validation :as validation]
            [clojure.java.io :as io])
  (:import [java.time LocalDateTime]
           [java.time.format DateTimeFormatter]
           [java.util UUID]))

;;; ============================================================
;;; Storage - Org-Mode Integration
;;; ============================================================

(def default-prompts-file
  "Default location for prompts org file."
  (str (System/getProperty "user.home") "/.emacs.d/hive-mcp/prompts.org"))

(defn generate-id
  "Generate a unique ID for a prompt entry."
  []
  (let [now (LocalDateTime/now)
        fmt (DateTimeFormatter/ofPattern "yyyyMMddHHmmss")]
    (str (.format now fmt) "-" (subs (str (UUID/randomUUID)) 0 8))))

(defn now-timestamp
  "Get current timestamp in ISO format."
  []
  (let [now (LocalDateTime/now)
        fmt (DateTimeFormatter/ofPattern "yyyy-MM-dd'T'HH:mm:ss")]
    (.format now fmt)))

(defn entry->headline
  "Convert a prompt entry to an org headline structure."
  [{:keys [id prompt accomplishes well-structured improvements
           category tags quality created updated source model context]}]
  {:level 2
   :keyword "NOTE"
   :title (str "[" (name category) "] "
               (if (> (count prompt) 50)
                 (str (subs prompt 0 47) "...")
                 prompt))
   :tags (mapv name tags)
   :properties (cond-> {"ID" id
                        "CATEGORY" (name category)
                        "QUALITY" (name quality)
                        "CREATED" created}
                 updated (assoc "UPDATED" updated)
                 source (assoc "SOURCE" source)
                 model (assoc "MODEL" model))
   ;; Content must be a vector of lines for the org writer
   :content (vec (concat
                  ["*** Prompt"
                   "#+begin_src text"]
                  (str/split-lines prompt)
                  ["#+end_src"
                   ""
                   "*** What It Accomplishes"]
                  (str/split-lines accomplishes)
                  [""
                   "*** Why Well-Structured"]
                  (str/split-lines well-structured)
                  (when improvements
                    (concat [""
                             "*** Improvements"]
                            (str/split-lines improvements)))
                  (when context
                    (concat [""
                             "*** Context"]
                            (str/split-lines context)))))})

(defn headline->entry
  "Convert an org headline back to a prompt entry."
  [{:keys [properties children title tags] :as _headline}]
  ;; Children are sub-headlines like *** Prompt, *** What It Accomplishes, etc.
  (let [get-child-content (fn [child-title]
                            (some->> children
                                     (filter #(= (:title %) child-title))
                                     first
                                     :content
                                     (remove #(or (str/starts-with? % "#+begin_src")
                                                  (str/starts-with? % "#+end_src")))
                                     (str/join "\n")
                                     str/trim))
        ;; Extract tags from title if parser put them there
        title-tags (when (str/includes? (or title "") ":")
                     (let [tag-match (re-find #":([^:]+(?::[^:]+)*):$" title)]
                       (when tag-match
                         (str/split (second tag-match) #":"))))]
    {:id (get properties :ID)
     :prompt (get-child-content "Prompt")
     :accomplishes (get-child-content "What It Accomplishes")
     :well-structured (get-child-content "Why Well-Structured")
     :improvements (get-child-content "Improvements")
     :category (keyword (get properties :CATEGORY "meta"))
     :tags (vec (or title-tags tags []))
     :quality (keyword (get properties :QUALITY "untested"))
     :created (get properties :CREATED)
     :updated (get properties :UPDATED)
     :source (get properties :SOURCE)
     :model (get properties :MODEL)
     :context (get-child-content "Context")}))

(defn load-prompts-file
  "Load prompts from org file. Returns vector of entries."
  [file-path]
  (if (.exists (io/file file-path))
    (let [doc (parser/parse-document (slurp file-path))
          headlines (:headlines doc)]
      (mapv headline->entry headlines))
    []))

(defn save-prompts-file
  "Save prompts to org file."
  [file-path entries]
  (let [dir (io/file (.getParent (io/file file-path)))]
    (when-not (.exists dir)
      (.mkdirs dir)))
  (let [doc {:properties {"TITLE" "Prompt Engineering Knowledge Base"
                          "STARTUP" "overview"
                          "TODO" "NOTE | ARCHIVED"}
             :headlines (mapv entry->headline entries)}
        org-text (writer/write-document doc)]
    (spit file-path org-text)
    {:saved true :count (count entries) :file file-path}))

;;; ============================================================
;;; Core API
;;; ============================================================

(defn format-confirmation
  "Format a confirmation message for display."
  [{:keys [id category quality tags prompt accomplishes]}]
  (str "╔══════════════════════════════════════════════════════════════╗\n"
       "║           📝 PROMPT CAPTURED SUCCESSFULLY                    ║\n"
       "╠══════════════════════════════════════════════════════════════╣\n"
       "║ ID: " id (str/join "" (repeat (- 55 (count id)) " ")) "║\n"
       "║ Category: " (name category) (str/join "" (repeat (- 50 (count (name category))) " ")) "║\n"
       "║ Quality: " (name quality) (str/join "" (repeat (- 51 (count (name quality))) " ")) "║\n"
       "║ Tags: " (str/join ", " (take 5 tags))
       (str/join "" (repeat (max 0 (- 54 (count (str/join ", " (take 5 tags))))) " ")) "║\n"
       "╠══════════════════════════════════════════════════════════════╣\n"
       "║ Preview:                                                     ║\n"
       "║ " (if (> (count prompt) 58)
              (str (subs prompt 0 55) "...")
              (str prompt (str/join "" (repeat (- 58 (count prompt)) " ")))) " ║\n"
       "╠══════════════════════════════════════════════════════════════╣\n"
       "║ Accomplishes:                                                ║\n"
       "║ " (if (> (count accomplishes) 58)
              (str (subs accomplishes 0 55) "...")
              (str accomplishes (str/join "" (repeat (max 0 (- 58 (count accomplishes))) " ")))) " ║\n"
       "╚══════════════════════════════════════════════════════════════╝"))

(defn capture-prompt
  "Capture a prompt with analysis. Returns the saved entry with confirmation.
   
   Required keys:
   - :prompt - The prompt text
   - :accomplishes - What the prompt accomplishes
   - :well-structured - Why it's well-structured
   
   Optional keys:
   - :improvements - Suggested improvements
   - :category - Category keyword (auto-inferred if not provided)
   - :tags - Vector of tag strings
   - :quality - :success, :partial, :failure, or :untested
   - :source - \"user\", \"observed\", \"generated\"
   - :model - Model used (e.g., \"claude-opus-4-5\")
   - :context - Additional context"
  [{:keys [prompt accomplishes well-structured improvements
           category tags quality source model context file-path]
    :or {quality :untested
         source "user"
         tags []}}]
  (let [file (or file-path default-prompts-file)
        inferred-category (or category (validation/infer-category prompt))
        auto-tags (into tags (get validation/category-tags inferred-category []))
        id (generate-id)
        now (now-timestamp)
        entry {:id id
               :prompt prompt
               :accomplishes accomplishes
               :well-structured well-structured
               :improvements (or improvements
                                 (str/join "\n" (validation/suggest-improvements prompt)))
               :category inferred-category
               :tags (vec (distinct auto-tags))
               :quality quality
               :created now
               :source source
               :model model
               :context context}
        validation-result (validation/validate-entry entry)]
    (if (:valid? validation-result)
      (let [existing (load-prompts-file file)
            updated (conj existing entry)]
        (save-prompts-file file updated)
        {:success true
         :entry entry
         :quality-assessment (validation/assess-prompt-quality prompt)
         :confirmation (format-confirmation entry)})
      {:success false
       :errors (:errors validation-result)})))

(defn list-prompts
  "List captured prompts with optional filtering.
   
   Options:
   - :category - Filter by category
   - :quality - Filter by quality rating
   - :tags - Filter by tags (any match)
   - :limit - Max results"
  [& [{:keys [category quality tags limit file-path]
       :or {limit 50}}]]
  (let [file (or file-path default-prompts-file)
        entries (load-prompts-file file)
        filtered (cond->> entries
                   category (filter #(= (:category %) category))
                   quality (filter #(= (:quality %) quality))
                   (seq tags) (filter #(some (set (:tags %)) tags))
                   true (take limit))]
    {:count (count filtered)
     :total (count entries)
     :entries (vec filtered)}))

(defn search-prompts
  "Search prompts by keyword in prompt text or accomplishes."
  [query & [{:keys [file-path limit] :or {limit 20}}]]
  (let [file (or file-path default-prompts-file)
        entries (load-prompts-file file)
        query-lower (str/lower-case query)
        matches (filter (fn [e]
                          (or (str/includes? (str/lower-case (:prompt e "")) query-lower)
                              (str/includes? (str/lower-case (:accomplishes e "")) query-lower)))
                        entries)]
    {:count (count matches)
     :query query
     :entries (vec (take limit matches))}))

(defn get-prompt
  "Get a specific prompt by ID."
  [id & [{:keys [file-path]}]]
  (let [file (or file-path default-prompts-file)
        entries (load-prompts-file file)]
    (first (filter #(= (:id %) id) entries))))

(defn update-prompt-quality
  "Update the quality rating of a prompt."
  [id new-quality & [{:keys [file-path]}]]
  (let [file (or file-path default-prompts-file)
        entries (load-prompts-file file)
        updated (mapv (fn [e]
                        (if (= (:id e) id)
                          (assoc e :quality new-quality :updated (now-timestamp))
                          e))
                      entries)]
    (save-prompts-file file updated)
    {:updated true :id id :quality new-quality}))

(defn get-statistics
  "Get statistics about captured prompts."
  [& [{:keys [file-path]}]]
  (let [file (or file-path default-prompts-file)
        entries (load-prompts-file file)]
    {:total (count entries)
     :by-category (frequencies (map :category entries))
     :by-quality (frequencies (map :quality entries))
     :by-source (frequencies (map :source entries))
     :recent (take 5 (reverse (sort-by :created entries)))}))

;;; ============================================================
;;; MCP Tool Handlers
;;; ============================================================

(defn handle-prompt-capture
  "MCP tool handler for /capture command."
  [{:keys [prompt accomplishes well_structured improvements
           category tags quality source model context]}]
  (try
    (let [result (capture-prompt {:prompt prompt
                                  :accomplishes accomplishes
                                  :well-structured well_structured
                                  :improvements improvements
                                  :category (when category (keyword category))
                                  :tags (or tags [])
                                  :quality (if quality (keyword quality) :untested)
                                  :source (or source "user")
                                  :model model
                                  :context context})]
      {:type "text" :text (pr-str result)})
    (catch Exception e
      {:type "text" :text (str "Error capturing prompt: " (.getMessage e)) :isError true})))

(defn handle-prompt-list
  "MCP tool handler for listing prompts."
  [{:keys [category quality limit] :as _entry}]
  (try
    (let [result (list-prompts {:category (when category (keyword category))
                                :quality (when quality (keyword quality))
                                :limit (or limit 20)})]
      {:type "text" :text (pr-str result)})
    (catch Exception e
      {:type "text" :text (str "Error listing prompts: " (.getMessage e)) :isError true})))

(defn handle-prompt-search
  "MCP tool handler for searching prompts."
  [{:keys [query limit]}]
  (try
    (let [result (search-prompts query {:limit (or limit 20)})]
      {:type "text" :text (pr-str result)})
    (catch Exception e
      {:type "text" :text (str "Error searching prompts: " (.getMessage e)) :isError true})))

(defn handle-prompt-analyze
  "MCP tool handler for analyzing a prompt without saving."
  [{:keys [prompt]}]
  (try
    (let [quality (validation/assess-prompt-quality prompt)
          improvements (validation/suggest-improvements prompt)
          category (validation/infer-category prompt)
          result {:analysis {:category category
                             :quality-score (:score quality)
                             :assessment (:assessment quality)
                             :structure {:has-context (:has-context? quality)
                                         :has-specific-task (:has-specific-task? quality)
                                         :has-constraints (:has-constraints? quality)
                                         :has-output-spec (:has-output-spec? quality)}
                             :suggested-improvements improvements
                             :suggested-tags (get validation/category-tags category [])}}]
      {:type "text" :text (pr-str result)})
    (catch Exception e
      {:type "text" :text (str "Error analyzing prompt: " (.getMessage e)) :isError true})))

(defn handle-prompt-stats
  "MCP tool handler for prompt statistics."
  [_]
  (try
    {:type "text" :text (pr-str (get-statistics))}
    (catch Exception e
      {:type "text" :text (str "Error getting stats: " (.getMessage e)) :isError true})))
