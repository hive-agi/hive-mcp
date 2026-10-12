(ns hive-mcp.channel.drain-projection-arc-test
  "ARC (arXiv 2607.25066) regression for the drain projection.

   ARC names the same architecture shipped here: the append-only store KEEPS
   every entry, the displayed context holds pointers, recall is on demand. Its
   reported failure mode is that citation stubs carry a fixed overhead and
   degrade under very tight budgets, where an entry is about as short as its
   own pointer. `project-entry` mitigates by displaying whichever of pointer
   and entry is smaller. These tests pin that at the small end, where the
   failure lives: tiny bodies, and bodies right around the crossover."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.channel.drain-projection :as proj]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: MIT

(def ^:private opts
  "Index policy with the full floor policy pinned, so the test never reads
   config for the floor lane."
  {:policy :index :axiom-policy :full})

(defn displayed-size
  "Printed size of ENTRY as the drain displays it under the index policy."
  [entry]
  (count (pr-str (proj/project-entry entry opts))))

(defn- kept-size [entry] (count (pr-str entry)))

(defn- pool-entry [content]
  {:id "e-1" :T :note :C content :tags ["t"]})

(def ^:private gen-tiny-entry
  "Pool entries whose body is 0 to 40 chars: the tight-budget regime ARC
   reports stubs to lose in."
  (gen/fmap (fn [[c tags]] {:id "tiny" :T :note :C c :tags tags})
            (gen/tuple (gen/fmap (partial apply str)
                                 (gen/vector gen/char-alphanumeric 0 40))
                       (gen/vector (gen/not-empty gen/string-alphanumeric) 0 3))))

(defn- never-grows?
  "The pointer overhead is never paid when it costs more than the entry."
  [[entry displayed]]
  (<= displayed (kept-size entry)))

(defn- displayed-with-entry [entry] [entry (displayed-size entry)])

(deftrifecta pointer-overhead-never-exceeds-the-entry
  #'hive-mcp.channel.drain-projection-arc-test/displayed-with-entry
  {:golden-path "test/golden/drain_projection_arc.edn"
   :cases       {:empty-body  (pool-entry "")
                 :one-char    (pool-entry "x")
                 :short-line  (pool-entry "use carto")
                 :long-body   (pool-entry (apply str (repeat 400 "keep ")))}
   :gen         gen-tiny-entry
   :pred        never-grows?
   :num-tests   300
   :mutations   [["always-pointer"
                  (fn [entry] [entry (count (pr-str (proj/index-entry entry)))])]
                 ["always-ten-more"
                  (fn [entry] [entry (+ 10 (count (pr-str entry)))])]]})

(deftest a-stub-that-costs-more-than-its-entry-is-not-sent
  (testing "an entry shorter than its own pointer is displayed whole"
    (let [e (pool-entry "x")]
      (is (> (count (pr-str (proj/index-entry e))) (kept-size e))
          "precondition: at this size the stub carries more than the body")
      (is (= e (proj/project-entry e opts))))))

(deftest a-long-entry-is-displayed-as-a-pointer
  (testing "above the crossover the pointer wins and the store keeps the body"
    (let [e (pool-entry (apply str (repeat 400 "keep ")))
          shown (proj/project-entry e opts)]
      (is (:ref shown))
      (is (< (count (pr-str shown)) (kept-size e))))))

(deftest a-whole-batch-of-tiny-entries-never-grows
  (testing "the per-entry choice bounds the batch, so a tight budget is never worse than :full"
    (let [es (mapv #(pool-entry (subs "abcdefghijklmnop" 0 %)) (range 17))]
      (is (<= (count (pr-str (proj/project es opts)))
              (count (pr-str es)))))))
