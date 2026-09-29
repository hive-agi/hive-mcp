(ns hive-mcp.channel.digest
  "Coalesce a HIVEMIND drain per agent: lifecycle rows fold into the terminal
   row, errors reduce to one line, long messages clip. Pure."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def terminal-events #{"completed" "error" "aborted" "failed" "blocked"})

(def error-events #{"error" "aborted" "failed" "blocked"})

(def foldable-events #{"started" "progress"})

(def defaults {:max-words 60 :error-chars 160 :timeout-ms 5000})

(defn settings
  "Stage options from the [:hivemind :digest] config map."
  [cfg]
  (assoc (merge (select-keys defaults [:max-words :error-chars])
                (select-keys cfg [:max-words :error-chars]))
         :timeout-ms (or (get-in cfg [:model :timeout-ms]) (:timeout-ms defaults))))

(defn- event [row] (some-> (:e row) name))

(defn terminal? [row] (contains? terminal-events (event row)))

(defn error? [row] (contains? error-events (event row)))

(defn- foldable? [row]
  (and (contains? foldable-events (event row))
       (not (:deliberate? row))))

(defn words [s]
  (remove str/blank? (str/split (str s) #"\s+")))

(defn clip-words
  "{:text :dropped} for `s` capped at `max-words`; an uncut `s` is returned verbatim."
  [max-words s]
  (let [ws (vec (words s))
        n (count ws)]
    (if (<= n max-words)
      {:text (str s) :dropped 0}
      {:text (str (str/join " " (subvec ws 0 max-words)) " …")
       :dropped (- n max-words)})))

(defn- cap-chars [n s]
  (if (> (count s) n)
    (str (subs s 0 (max 0 (dec n))) "…")
    s))

(def ^:private class-re
  #"\b(?:[a-z_$][\w$]*\.)*[\w$]*(?:ExceptionInfo|Exception|Error|Throwable)\b")

(def ^:private status-re
  #"(?i)\b(?:status|http)[\s:=\"]*(\d{3})\b")

(def ^:private noise-line-re
  #"(?:at |\.\.\. \d+ more|Caused by|\{).*")

(defn error-line
  "`Class status: first line` for an error message, capped at `max-chars`."
  [max-chars msg]
  (let [s (str msg)
        cls (some-> (re-find class-re s) (str/split #"\.") last)
        status (second (re-find status-re s))
        line (->> (str/split-lines s)
                  (map str/trim)
                  (remove str/blank?)
                  (remove #(re-matches noise-line-re %))
                  first)
        line (some-> line
                     (str/replace (re-pattern (str "^" class-re "[:\\s]*")) "")
                     str/trim)
        head (str/join " " (remove nil? [cls status]))]
    (cap-chars max-chars
               (cond
                 (and (seq head) (seq line)) (str head ": " line)
                 (seq head) head
                 :else (or line "")))))

(defn- min-ts [rows]
  (when-let [ts (seq (keep :ts rows))]
    (apply min ts)))

(defn coalesce
  "Fold each agent's non-deliberate started/progress rows into that agent's
   LAST terminal row. The terminal row carries ::texts, the full messages
   oldest first; a row that absorbed others gains :n and the earliest :ts."
  [rows]
  (let [rows (vec rows)
        term-idx (reduce-kv (fn [acc i r] (if (terminal? r) (assoc acc (:a r) i) acc))
                            {} rows)
        folded (group-by :a (filter #(and (foldable? %) (contains? term-idx (:a %))) rows))]
    (into []
          (keep-indexed
           (fn [i r]
             (let [ti (get term-idx (:a r))]
               (cond
                 (and ti (foldable? r)) nil
                 (= i ti)
                 (let [fs (get folded (:a r))]
                   (cond-> (assoc r ::texts (conj (mapv #(str (:m %)) fs) (str (:m r))))
                     (seq fs) (assoc :n (reduce + (:n r 1) (map #(:n % 1) fs))
                                     ::hidden true)
                     (and (seq fs) (min-ts (conj fs r))) (assoc :ts (min-ts (conj fs r)))))
                 :else r))))
          rows)))

(defn compact-row
  "Errors to one line; every message clipped at :max-words."
  [{:keys [max-words error-chars]} row]
  (let [m (str (:m row))
        line (if (error? row) (error-line error-chars m) m)
        {:keys [text dropped]} (clip-words max-words line)]
    (cond-> row
      (not= line m) (assoc :m line ::hidden true)
      (pos? dropped) (assoc :m text :clip dropped ::hidden true))))

(defn coalesce-rows
  "Stage 1: coalesce then compact. Rows keep stage-private keys until `finalize`."
  [opts rows]
  (let [opts (merge defaults opts)]
    (mapv #(compact-row opts %) (coalesce rows))))

(defn texts [row] (::texts row))

(defn needs-model?
  "A non-error terminal row whose raw texts exceed `max-words`."
  [max-words row]
  (and (terminal? row)
       (not (error? row))
       (> (count (mapcat words (texts row))) max-words)))

(defn with-digest
  "Replace a row's message with a model digest."
  [row digest]
  (-> row
      (assoc :m digest :dg true ::hidden true)
      (dissoc :clip)))

(defn finalize
  "Strip stage-private keys; :ts survives only where something was hidden."
  [row]
  (if (::hidden row)
    (dissoc row ::hidden ::texts)
    (dissoc row ::hidden ::texts :ts)))

(defn strip
  "Today's row shape: no :ts."
  [row]
  (dissoc row :ts))
