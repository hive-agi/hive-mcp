(ns hive-mcp.swarm.coordinator-test
  "Facets for hive-mcp.swarm.coordinator on the unit classpath.

     trifecta  extract-files-from-prompt, pure, driven from its contract with
               prompts built out of the phrasings its patterns read (edit X,
               `X`, \"X\", absolute and ./ paths) mixed with noise and URLs
     golden    extraction over named prompts, and the task queue (queue,
               read, remove, clear), which is host state
   Conflict detection and claims go through hive-mcp.swarm.logic, a compat
   shim over hive-agent; those paths run in
   test-swarm/hive_mcp/swarm/coordinator_test.clj."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.swarm.coordinator :as coordinator]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (coordinator/clear-queue!) (try (f) (finally (coordinator/clear-queue!)))))

(def ^:private files ["src/foo.clj" "core.clj" "/abs/x.edn" "./rel/y.md" "a_b.txt"])

(def ^:private Piece
  "One fragment of a prompt: a file in one of the phrasings the extractor
   reads, or noise it must ignore."
  [:or
   [:tuple [:= :edit] (into [:enum] files)]
   [:tuple [:= :tick] (into [:enum] files)]
   [:tuple [:= :quote] (into [:enum] files)]
   [:tuple [:= :bare] [:enum "/abs/x.edn" "./rel/y.md"]]
   [:tuple [:= :noise] [:enum "please" "and then" "http://x.io/a.html" "v1.2" "done."]]])

(defn- render [[kind x]]
  (case kind
    :edit  (str "edit " x)
    :tick  (str "`" x "`")
    :quote (str "\"" x "\"")
    :bare  x
    :noise x))

(def ^:private Prompt
  [:string {:gen/schema [:vector {:gen/max 5} Piece]
            :gen/fmap   #(str/join " " (map render %))}])

(defn- mentioned
  "The files a prompt built from PIECES mentions, in a form the extractor reads."
  [prompt]
  (set (filter #(str/includes? prompt %) files)))

(hst/deftrifecta-from-schema extract-files-finds-every-mentioned-file
  hive-mcp.swarm.coordinator/extract-files-from-prompt
  {:in  [:cat Prompt]
   :out [:maybe [:vector :string]]
   :rel (fn [[prompt] out]
          (and (= (count out) (count (distinct out)))
               (= (mentioned prompt) (set out))))
   :num-tests 300})

(deftest-golden extract-files-golden
  "test/golden/hive-mcp/swarm/coordinator-extract-files.edn"
  (into (sorted-map)
        (map (fn [[k p]] [k (coordinator/extract-files-from-prompt p)]))
        {:edit-verb  "Please edit src/foo.clj and modify core.clj"
         :backticks  "look at `src/a.clj` then `b.edn`"
         :quoted     "open \"docs/x.md\""
         :absolute   "the file /tmp/z.txt is broken"
         :relative   "see ./rel/y.md and ../up/w.clj"
         :dedup      "edit a.clj, then `a.clj` again"
         :url        "read http://example.com/page.html"
         :no-files   "just think about it"
         :not-string 42}))

(deftest-golden task-queue-golden
  "test/golden/hive-mcp/swarm/coordinator-queue.edn"
  (let [strip   (fn [q] (mapv #(dissoc % :queued-at) q))
        id-a    (coordinator/queue-task! {:id "t-a" :slave-id "s1" :files ["a.clj"]})
        id-b    (coordinator/queue-task! {:id "t-b" :slave-id "s2"})
        two     (strip (coordinator/get-queue))
        _       (coordinator/remove-from-queue! "t-a")
        one     (strip (coordinator/get-queue))
        _       (coordinator/remove-from-queue! "t-absent")
        still   (strip (coordinator/get-queue))
        _       (coordinator/clear-queue!)]
    {:ids [id-a id-b] :two two :after-remove one :remove-absent still
     :cleared (coordinator/get-queue)}))

(deftest queue-task-mints-an-id-test
  (let [id (coordinator/queue-task! {:slave-id "s1"})]
    (is (string? id))
    (is (= [id] (map :id (coordinator/get-queue))))
    (is (int? (:queued-at (first (coordinator/get-queue)))))))
