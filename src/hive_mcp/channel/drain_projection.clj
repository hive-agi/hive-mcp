(ns hive-mcp.channel.drain-projection
  "Projection policy for the memory piggyback drain: what a buffered entry
   looks like ON THE WIRE.

   `:full` sends every entry's content, which is the historical behaviour.
   `:index` sends floor-lane entries untouched and replaces every pool entry
   with an id, title and tags row that the caller PULLS on demand.

   The asymmetry is structural, not a tuning choice. A floor entry is an axiom:
   inviolable, quoted word for word, so it is never projected. Everything else
   is a pointer until the caller decides it is relevant, which is the whole
   point: ranking reorders a push, it never reduces one, so a session pays for
   every buffered entry eventually no matter how well it is ranked. Projection
   is the only lever that changes the total.

   Pure, deterministic, no IO. The floor test goes through
   `hive-mcp.channel.drain-rank/lane`, so `floor-types` and `:pins` stay the
   single definition of what must never be summarised."
  (:require [clojure.string :as str]
            [hive-mcp.channel.drain-rank :as rank]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:const default-policy
  "Policy applied when config names none."
  :index)

(def policies
  "The closed set of wire policies."
  #{:full :index})

(def ^:const title-chars
  "Chars of the title line kept in an index row. Enough to decide relevance,
   not enough to substitute for reading the entry."
  100)

(def ^:const index-tags
  "Tags kept in an index row."
  5)

(defn title-of
  "The first non-blank line of CONTENT, stripped of markdown heading marks and
   truncated to `title-chars`.

   The first line of a hive entry is written as its claim, so it is the
   cheapest sufficient relevance signal."
  [content]
  (let [line (->> (str/split-lines (str content))
                  (map str/trim)
                  (remove str/blank?)
                  first)
        line (-> (str line)
                 (str/replace #"^#+\s*" "")
                 str/trim)]
    (if (> (count line) title-chars)
      (str (subs line 0 title-chars) "...")
      line)))

(defn index-entry
  "ENTRY reduced to a pull row: id, type, title and a few tags.

   `:ref` marks the row as a pointer so a reader never mistakes a title for the
   entry. The title rides on `:C` deliberately: the ranker scores that key, so
   an indexed entry stays rankable without the ranker knowing about projection."
  [entry]
  (cond-> {:id (:id entry)
           :T (:T entry)
           :C (title-of (:C entry))
           :ref true}
    (seq (:tags entry)) (assoc :tags (vec (take index-tags (:tags entry))))))

(defn project-entry
  "ENTRY as POLICY sends it, given the lane it falls in.

   A floor entry is returned untouched under every policy.

   A pool entry is projected only when the pointer is actually SMALLER than the
   entry. A very short entry costs more as a pointer than as itself, because
   `:ref true` outweighs the body it replaces, and sending a pointer that costs
   more than the content defeats the purpose and forces a pull for nothing.
   Checking is cheaper than reasoning about where the crossover sits."
  [entry {:keys [policy pins] :or {policy default-policy}}]
  (if (or (not= :index policy)
          (= :floor (rank/lane entry (or pins #{})))
          (:ref entry))
    entry
    (let [row (index-entry entry)]
      (if (< (count (pr-str row)) (count (pr-str entry)))
        row
        entry))))

(defn project
  "ENTRIES as POLICY sends them.

   Idempotent: a row already carrying `:ref` is passed through, so projecting a
   buffer that was written back projected changes nothing."
  [entries opts]
  (mapv #(project-entry % opts) entries))

(defn resolve-policy
  "Wire policy for this drain, by the house precedence
   caller-override > config file > default.

   Read at request time on purpose: the coordinator JVM is long lived, so a
   knob that is only read at startup cannot be changed without a restart. See
   memory 20260915185914-3f97bc23.

   Never throws: an unreadable config or an unknown name falls back to
   `default-policy`, because a drain must not fail over a preference."
  ([] (resolve-policy nil))
  ([override]
   (let [cfg-val (try
                   (when-let [f (requiring-resolve 'hive-mcp.config.core/get-in-config)]
                     (f [:services :catchup :drain-policy]))
                   (catch Throwable _ nil))
         raw (or override cfg-val default-policy)
         kw (if (keyword? raw)
              raw
              (keyword (str/replace (str/trim (str raw)) #"^:" "")))]
     (if (contains? policies kw) kw default-policy))))

(defn pull-hint
  "One line telling the caller how to turn index rows back into entries.

   Returned only when the batch actually contains index rows, so a :full drain
   carries no instruction it cannot act on."
  [batch]
  (when (some :ref batch)
    (str "Rows with :ref are POINTERS, not content. Pull one with "
         "`memory get :id <id>`, several with `memory batch-get :ids [...]`, "
         "or follow its edges with `memory kg traverse :start_node <id>`. "
         "Pull what the current task needs; ignore the rest.")))
