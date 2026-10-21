(ns hive-mcp.server.routes.child-pids-memo-test
  "Descendant project-ids are memoized on the identity of the cached
   project tree: same tree, no recomputation; a new tree object (what
   tree/invalidate-tree-cache! plus a rebuild yields) recomputes.

   The compute fn is a counting stub; the cache is a local atom."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.server.routes.middleware :as mw]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- counting-compute [calls result]
  (fn [pid] (swap! calls conj pid) (get result pid)))

(deftest same-tree-computes-once-test
  (let [cache (atom {:tree nil :by-pid {}})
        calls (atom [])
        tree  {:roots ["hive"]}
        f     (counting-compute calls {"hive" ["a" "b"]})]
    (is (= #{"a" "b"} (mw/memoized-child-pids cache tree "hive" f)))
    (is (= #{"a" "b"} (mw/memoized-child-pids cache tree "hive" f)))
    (is (= ["hive"] @calls) "second call served from the memo")))

(deftest new-tree-identity-invalidates-test
  (let [cache (atom {:tree nil :by-pid {}})
        calls (atom [])
        f     (counting-compute calls {"hive" ["a"]})]
    (mw/memoized-child-pids cache {:roots ["hive"]} "hive" f)
    (testing "an equal but distinct tree object is a different cache key"
      (mw/memoized-child-pids cache {:roots ["hive"]} "hive" f)
      (is (= 2 (count @calls))))))

(deftest childless-project-is-memoized-as-nil-test
  (let [cache (atom {:tree nil :by-pid {}})
        calls (atom [])
        tree  {:roots []}
        f     (counting-compute calls {})]
    (is (nil? (mw/memoized-child-pids cache tree "leaf" f)))
    (is (nil? (mw/memoized-child-pids cache tree "leaf" f)))
    (is (= 1 (count @calls)))))

(deftest nil-tree-is-never-memoized-test
  (let [cache (atom {:tree nil :by-pid {}})
        calls (atom [])
        f     (counting-compute calls {"hive" ["a"]})]
    (mw/memoized-child-pids cache nil "hive" f)
    (mw/memoized-child-pids cache nil "hive" f)
    (is (= 2 (count @calls)))))
