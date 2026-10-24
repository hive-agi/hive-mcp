(ns hive-mcp.knowledge-graph.frontier-batch-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.knowledge-graph.protocols :as p]
            [hive-mcp.knowledge-graph.queries :as q]))

(defn scalar-traverse
  "Reference BFS with the old per-node read shape and an explicit reader."
  [start {:keys [direction max-depth relations edge-reader]
          :or {direction :outgoing max-depth 3}}]
  (loop [queue [[start 0 [start]]] visited #{start} rows []]
    (if (empty? queue)
      rows
      (let [[node depth path] (first queue) tail (vec (rest queue))]
        (if (>= depth max-depth)
          (recur tail visited rows)
          (let [adjacent (case direction
                           :outgoing (p/get-edges-from edge-reader node)
                           :incoming (p/get-edges-to edge-reader node)
                           :both (concat (p/get-edges-from edge-reader node)
                                         (p/get-edges-to edge-reader node)))
                neighbors (->> adjacent
                               (filter #(or (nil? relations)
                                            (contains? relations (:kg-edge/relation %))))
                               (map (fn [e] [(if (= node (:kg-edge/from e))
                                               (:kg-edge/to e) (:kg-edge/from e)) e]))
                               (remove #(contains? visited (first %))) vec)]
            (recur (into tail (map (fn [[id _]] [id (inc depth) (conj path id)]) neighbors))
                   (into visited (map first neighbors))
                   (into rows (map (fn [[id edge]] {:node-id id :edge edge :depth (inc depth)
                                                   :path (conj path id)}) neighbors)))))))))

(defn edge [from to relation]
  {:kg-edge/from from :kg-edge/to to :kg-edge/relation relation})

(defn vector-reader [edges calls]
  (let [out (group-by :kg-edge/from edges) in (group-by :kg-edge/to edges)
        batch (fn [dir index ids]
                (swap! calls conj [dir (vec ids)])
                (select-keys index ids))]
    (reify p/IEdgeReader
      (get-edges-from [_ id] (get out id))
      (get-edges-from [_ id scope] (filterv #(= scope (:kg-edge/scope %)) (get out id)))
      (get-edges-to [_ id] (get in id))
      (get-edges-to [_ id scope] (filterv #(= scope (:kg-edge/scope %)) (get in id)))
      (batch-get-edges-from [_ ids] (batch :out out ids))
      (batch-get-edges-from [_ ids _] (batch :out out ids))
      (batch-get-edges-to [_ ids] (batch :in in ids))
      (batch-get-edges-to [_ ids _] (batch :in in ids)))))

(deftest scalar-equivalence
  (let [edges [(edge "a" "a" :refines) (edge "a" "b" :refines)
               (edge "a" "b" :implements) (edge "b" "c" :refines)
               (edge "c" "a" :refines) (edge "d" "b" :refines)
               (edge "d" "e" :implements)]
        reader (vector-reader edges (atom []))]
    (doseq [direction [:outgoing :incoming :both]
            depth [-1 0 1 2 3 5]
            start ["a" "b" "d" "absent"]
            relations [nil #{} #{:refines}]]
      (let [opts {:edge-reader reader :direction direction :max-depth depth :relations relations}]
        (is (= (scalar-traverse start opts) (q/traverse start opts))
            (str [direction depth start relations]))))))

(deftest bounded-frontier-reads
  (let [edges (vec (mapcat (fn [i] [(edge "root" (str "n" i) :refines)
                                   (edge (str "n" i) (str "leaf" i) :refines)]) (range 300)))
        calls (atom []) reader (vector-reader edges calls)
        rows (q/traverse "root" {:edge-reader reader :max-depth 2})]
    (is (= 600 (count rows)))
    (is (= 4 (count @calls)))
    (is (= [1 128 128 44] (mapv (comp count second) @calls)))
    (is (not-any? #(some (fn [id] (.startsWith ^String id "leaf")) (second %)) @calls))
    (reset! calls [])
    (q/traverse "root" {:edge-reader reader :max-depth 0})
    (is (empty? @calls))))

(deftest scope-filter-before-discovery
  (let [calls (atom [])
        reader (vector-reader [(assoc (edge "a" "hidden" :refines) :kg-edge/scope "never-visible-foreign-scope")
                               (edge "a" "b" :refines)
                               (edge "b" "c" :refines)] calls)
        rows (q/traverse "a" {:edge-reader reader :scope "frontier-test" :max-depth 3})]
    (is (= #{"b" "c"} (set (map :node-id rows))))
    (is (not-any? #(some #{"hidden"} (second %)) @calls))))

(deftest batch-failure-is-not-an-empty-graph
  (let [reader (reify p/IEdgeReader
                 (batch-get-edges-from [_ _]
                   (throw (ex-info "read timed out" {:error :timeout}))))]
    (is (thrown-with-msg? clojure.lang.ExceptionInfo #"read timed out"
                         (q/traverse "a" {:edge-reader reader})))))
