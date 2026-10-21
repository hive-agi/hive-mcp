(ns hive-mcp.tools.kanban.filters-test
  "Pure-fn tests for kanban list filter helpers in
   `hive-mcp.tools.kanban.filters`. Exercises the token-budget
   filters wired into `handle-mem-kanban-list-slim`."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.kanban.filters :as kf]
            [hive-mcp.tools.kanban.schema :as ks]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

(def ^:private fixture
  [{:id "a"
    :tags ["kanban" "todo" "priority-high" "scope:project:x"]
    :updated "2026-04-28T10:00:00-0300"
    :content {:task-type "kanban" :title "Fix grep"
              :description "ripgrep returns empty matches"
              :status "todo" :priority "high"
              :created "2026-04-28T09:00:00-0300"}}
   {:id "b"
    :tags ["kanban" "todo" "priority-medium" "scope:project:y"]
    :updated "2026-04-26T10:00:00-0300"
    :content {:task-type "kanban" :title "Add docs"
              :description "readme update"
              :status "todo" :priority "medium"
              :created "2026-04-26T09:00:00-0300"}}
   {:id "c"
    :tags ["kanban" "todo" "priority-low" "scope:project:x" "infra"]
    :updated "2026-04-25T10:00:00-0300"
    :content {:task-type "kanban" :title "K8s manifest"
              :description "stateful set"
              :status "todo" :priority "low"
              :created "2026-04-25T09:00:00-0300"}}])

(defn- ids [pred]
  (mapv :id (filter pred fixture)))

(deftest entry-matches-query?-test
  (testing "case-insensitive substring match against title"
    (is (= ["a"] (ids #(kf/entry-matches-query? % "GREP")))))
  (testing "matches against description"
    (is (= ["a"] (ids #(kf/entry-matches-query? % "ripgrep")))))
  (testing "blank/nil query → match all"
    (is (= 3 (count (ids #(kf/entry-matches-query? % nil)))))
    (is (= 3 (count (ids #(kf/entry-matches-query? % "")))))
    (is (= 3 (count (ids #(kf/entry-matches-query? % "   ")))))))

(deftest entry-tags-match?-test
  (testing ":any matches when at least one tag present"
    (is (= ["c"] (ids #(kf/entry-tags-match? % ["infra"] :any))))
    (is (= ["a" "b" "c"]
           (ids #(kf/entry-tags-match? % ["todo" "infra"] :any)))))
  (testing ":all matches only when every tag present"
    (is (= ["a"]
           (ids #(kf/entry-tags-match? % ["scope:project:x" "priority-high"] :all))))
    (is (empty? (ids #(kf/entry-tags-match? % ["nope"] :all)))))
  (testing "empty/nil tag set → match all"
    (is (= 3 (count (ids #(kf/entry-tags-match? % [] :all)))))
    (is (= 3 (count (ids #(kf/entry-tags-match? % nil :any)))))))

(deftest entry-after-ts?-test
  (testing ":created threshold (strict)"
    (is (= ["a" "b"]
           (ids #(kf/entry-after-ts? % :created "2026-04-26T00:00:00-0300"))))
    (is (= ["a"]
           (ids #(kf/entry-after-ts? % :created "2026-04-27T00:00:00-0300")))))
  (testing ":updated threshold"
    (is (= ["a"]
           (ids #(kf/entry-after-ts? % :updated "2026-04-27T00:00:00-0300")))))
  (testing "nil threshold → match all"
    (is (= 3 (count (ids #(kf/entry-after-ts? % :created nil)))))))

(deftest paginate-test
  (testing "offset+limit"
    (is (= ["b" "c"] (vec (kf/paginate ["a" "b" "c" "d"] 1 2)))))
  (testing "limit only"
    (is (= ["a" "b"] (vec (kf/paginate ["a" "b" "c" "d"] nil 2)))))
  (testing "offset only"
    (is (= ["c" "d"] (vec (kf/paginate ["a" "b" "c" "d"] 2 nil)))))
  (testing "no offset / no limit → identity"
    (is (= ["a" "b" "c"] (vec (kf/paginate ["a" "b" "c"] nil nil)))))
  (testing "zero offset / zero limit ignored"
    (is (= ["a" "b" "c"] (vec (kf/paginate ["a" "b" "c"] 0 0))))))

(deftest project-fields-test
  (let [task {:id "x" :title "t" :status "todo" :priority "high" :project "x"}]
    (testing "string field names accepted"
      (is (= {:id "x" :title "t"}
             (kf/project-fields task ["id" "title"]))))
    (testing "keyword field names accepted"
      (is (= {:id "x" :status "todo"}
             (kf/project-fields task [:id :status]))))
    (testing "empty/nil fields → identity"
      (is (= task (kf/project-fields task nil)))
      (is (= task (kf/project-fields task []))))))

(deftest post-filters?-test
  (testing "any filter param triggers true"
    (is (kf/post-filters? {:query "x"}))
    (is (kf/post-filters? {:created_after "2026"}))
    (is (kf/post-filters? {:updated_after "2026"}))
    (is (kf/post-filters? {:limit 10}))
    (is (kf/post-filters? {:offset 5}))
    (is (kf/post-filters? {:tags ["a"] :tag_match "any"})))
  (testing "no relevant params → false"
    (is (not (kf/post-filters? {})))
    (is (not (kf/post-filters? {:status "todo"})))
    (is (not (kf/post-filters? {:query ""})))
    (is (not (kf/post-filters? {:query "   "})))))

;; =============================================================================
;; Trifectas over the contracts (generators from hive-mcp.tools.kanban.schema)
;; =============================================================================

(deftrifecta-from-schema substring-ci-is-case-blind-containment
  hive-mcp.tools.kanban.filters/substring-ci?
  ;; nil needle / nil or non-string haystack are pinned in substring-ci?-test
  {:in          [:cat [:enum "GREP" "rip" "" "zz" "Returns"] [:enum "ripgrep returns" "Fix Grep" "x"]]
   :out         :boolean
   :rel         (fn [[n h] out]
                  (= out (str/includes? (str/lower-case h) (str/lower-case n))))
   :num-tests   200
   :n-cases     12
   :golden-path "test/golden/tools/kanban/filters-substring-ci.edn"})

(deftrifecta-from-schema entry-priority-nil-matches-all
  hive-mcp.tools.kanban.filters/entry-priority?
  {:in          [:cat ks/EntryIn [:maybe [:enum "high" "medium" "low" "urgent"]]]
   :out         :boolean
   :rel         (fn [[entry p] out]
                  (let [c (:content entry)]
                    (= out (or (nil? p) (and (map? c) (= p (:priority c)))))))
   :num-tests   200
   :n-cases     12
   :golden-path "test/golden/tools/kanban/filters-entry-priority.edn"})

(def ^:private TagsArg [:maybe [:vector {:max 3} [:enum "kanban" "todo" "priority-high" "infra"]]])

(deftrifecta-from-schema entry-tags-match-all-or-any
  hive-mcp.tools.kanban.filters/entry-tags-match?
  {:in          [:cat [:map [:tags [:vector {:max 4} [:enum "kanban" "todo" "doing" "priority-high" "infra"]]]]
                 TagsArg [:enum :all :any "all"]]
   :out         :boolean
   :rel         (fn [[entry extra mode] out]
                  (let [have (set (:tags entry))]
                    (= out (cond (empty? extra) true
                                 (= mode :any) (boolean (some have extra))
                                 :else (every? have extra)))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/tools/kanban/filters-entry-tags-match.edn"})

(deftrifecta-from-schema paginate-drops-then-takes
  hive-mcp.tools.kanban.filters/paginate
  {:in          [:cat [:vector {:max 8} :int] [:maybe [:int {:min -1 :max 5}]] [:maybe [:int {:min -1 :max 5}]]]
   :out         [:maybe [:sequential :any]]
   :rel         (fn [[coll off lim] out]
                  (= out (cond->> coll
                           (and off (pos? off)) (drop off)
                           (and lim (pos? lim)) (take lim))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/tools/kanban/filters-paginate.edn"})

(deftrifecta-from-schema project-fields-selects-named-keys
  hive-mcp.tools.kanban.filters/project-fields
  {:in          [:cat [:map [:id [:enum "a"]] [:title [:enum "t"]] [:status [:enum "todo"]]]
                 [:maybe [:vector {:max 3} [:enum :id "title" :status "nope"]]]]
   :out         [:maybe [:map-of :any :any]]
   :rel         (fn [[task fields] out]
                  (= out (if (empty? fields) task (select-keys task (map keyword fields)))))
   :num-tests   200
   :n-cases     12
   :golden-path "test/golden/tools/kanban/filters-project-fields.edn"})

(deftrifecta-from-schema narrowing-post-filters-ignore-pagination
  hive-mcp.tools.kanban.filters/narrowing-post-filters?
  {:in          ks/ListParams
   :out         :boolean
   :rel         (fn [in out]
                  (= out (boolean (or (and (string? (:query in)) (not (str/blank? (:query in))))
                                      (:priority in) (:created_after in) (:updated_after in)
                                      (and (#{"any" :any} (:tag_match in)) (seq (:tags in)))))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/tools/kanban/filters-narrowing.edn"})

(deftrifecta-from-schema entry-after-ts-compares-instants
  hive-mcp.tools.kanban.filters/entry-after-ts?
  {:in          [:cat [:map [:content [:map [:created [:enum "2026-04-28T09:00:00-0300" "2026-04-25T09:00:00Z" "bad"]]]]]
                 [:enum :created]
                 [:maybe [:enum "2026-04-26T00:00:00-03:00" "2026-04-29T00:00:00Z" "nope"]]]
   :out         :boolean
   :n-cases     12
   :golden-path "test/golden/tools/kanban/filters-entry-after-ts.edn"})
