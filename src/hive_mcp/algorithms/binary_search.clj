(ns hive-mcp.algorithms.binary-search
  "Binary search implementation for sorted vectors.")

(defn binary-search
  "Performs binary search on a sorted vector to find the index of a target value.

  Args:
    coll: A sorted vector of comparable elements.
    target: The value to search for in the vector.

  Returns:
    The index of the target if found, otherwise nil.

  Examples:
    (binary-search [1 2 3 4 5] 3) ;=> 2
    (binary-search [1 2 3 4 5] 6) ;=> nil
    (binary-search [] 1) ;=> nil
    (binary-search [5] 5) ;=> 0
    (binary-search [1 2 3 4 5] 0) ;=> nil"
  [coll target]
  (when (seq coll)
    (loop [low 0
           high (dec (count coll))]
      (cond
        (> low high) nil
        :else
        (let [mid (quot (+ low high) 2)
              mid-val (get coll mid)]
          (cond
            (= mid-val target) mid
            (< mid-val target) (recur (inc mid) high)
            :else (recur low (dec mid))))))))