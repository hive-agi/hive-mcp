(ns hive-mcp.crystal.fanout-facets-test
  "Facets for the pure parts of the per-scope wrap fan-out: the scope tag,
   its idempotent prepend, and the slice -> legacy-harvested adapter. Kept
   apart from fanout-test, whose synthesiser mock is a with-redefs."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.crystal.fanout :as fan]
            [hive-mcp.crystal.harvest.by-scope :as bs]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Pid [:or [:= :umbrella] [:enum "hive" "funeraria" "a.b"]])

(deftrifecta-from-schema scope-tag-for-names-the-scope
  #'fan/scope-tag-for
  {:in [:cat Pid]
   :out :string
   :rel (fn [[pid] out]
          (= out (if (= pid :umbrella) "scope:multi-project" (str "scope:project:" pid))))
   :classify (fn [[pid] _] (if (= pid :umbrella) :umbrella :project))
   :classify-domain #{:umbrella :project}
   :golden-path "test/golden/crystal/fanout-scope-tag-for.edn"
   :num-tests 200})

(def ^:private Tag [:enum "session-summary" "scope:project:hive" "scope:multi-project" "x"])

(def ^:private Entry
  [:map [:tags {:optional true} [:maybe [:vector {:max 3} Tag]]]
   [:content {:optional true} [:enum "c1" "c2"]]])

(def ^:private EntryAndPid
  ;; Half the cases already carry the pid's own tag, so the idempotent
  ;; branch is reached as often as the prepend.
  [:cat {:gen/schema [:tuple Entry Pid :boolean]
         :gen/fmap (fn [[e pid tagged?]]
                     [(if tagged? (update e :tags #(conj (vec %) (fan/scope-tag-for pid))) e) pid])}
   Entry Pid])

(deftrifecta-from-schema with-scope-tag-prepends-once
  #'fan/with-scope-tag
  {:in EntryAndPid
   :out [:map [:tags [:vector :string]]]
   :rel (fn [[entry pid] out]
          (let [tag (fan/scope-tag-for pid)
                existing (vec (:tags entry))]
            (and (= (dissoc out :tags) (dissoc entry :tags))
                 (if (some #{tag} existing)
                   (= existing (:tags out))
                   (= (into [tag] existing) (:tags out))))))
   :classify (fn [[entry pid] _] (if (some #{(fan/scope-tag-for pid)} (:tags entry)) :already-tagged :prepended))
   :classify-domain #{:already-tagged :prepended}
   :golden-path "test/golden/crystal/fanout-with-scope-tag.edn"
   :num-tests 300})

(deftest with-scope-tag-twice-is-once
  (let [e {:tags ["x"]}]
    (is (= (fan/with-scope-tag e "hive")
           (fan/with-scope-tag (fan/with-scope-tag e "hive") "hive")))))

(deftest-golden slice-and-umbrella-adapters-golden
  "test/golden/crystal/fanout-harvested-adapters.edn"
  {:slice    (fan/slice->harvested
              "hive"
              {:progress-notes ["n1"] :git-commits ["c1" "c2"] :recalls [["k" 1] :solo]
               :kanban-activity [{:id "t"}]
               :kanban-movements [{:kanban-movement/to :done} {:kanban-movement/to :todo}]
               :memory-ids-created [{:id "m" :type "note"} {:id "n" :type "note"}]}
              {:session "s-1" :directory "/d" :agent-id "ling-1"})
   :empty    (fan/slice->harvested nil {} nil)
   :umbrella (fan/umbrella->harvested
              {:umbrella {:cross-pid-edges [{:from "a" :to "b"}]
                          :cross-cutting-decisions ["d1"]
                          :hivemind-shouts-global ["s"]}}
              {:session "s-1"})
   :no-umbrella (fan/umbrella->harvested {} nil)
   :umbrella-sentinel bs/umbrella-sentinel})
