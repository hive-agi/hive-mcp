(ns hive-mcp.batch.wave-bindings-test
  "A wave executor that runs ops on its own threads must still hand each op
   the caller's dynamic bindings (the request context among them)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.batch :as batch]
            [hive-mcp.extensions.registry :as ext]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *caller* nil)

(defn- on-fresh-threads
  "A :bx/j stand-in that runs every op on a new raw Thread, which conveys no
   bindings of its own (unlike future)."
  [wave-ops exec-one]
  (let [ps (mapv (fn [op]
                   (let [p (promise)]
                     (doto (Thread. #(deliver p (exec-one op))) (.start))
                     p))
                 wave-ops)]
    (mapv deref ps)))

(defn- with-wave-executor [f body]
  (let [prior (ext/get-extension :bx/j)]
    (try
      (ext/register! :bx/j f)
      (body)
      (finally
        (if prior (ext/register! :bx/j prior) (ext/deregister! :bx/j))))))

(defn- callers-seen [ops]
  (let [seen (atom {})
        handler (fn [params] (swap! seen assoc (:n params) *caller*) {:ok true})]
    (binding [*caller* "coordinator:batch-caller"]
      (batch/run-operations ops {:resolve-handler (constantly handler)}))
    @seen))

(deftest every-op-of-a-parallel-wave-sees-the-callers-bindings
  (testing "ops sharing a wave run on other threads and still see *caller*"
    (with-wave-executor on-fresh-threads
      (fn []
        (is (= {1 "coordinator:batch-caller" 2 "coordinator:batch-caller" 3 "coordinator:batch-caller"}
               (callers-seen [{:id "a" :tool "t" :command "c" :n 1}
                              {:id "b" :tool "t" :command "c" :n 2}
                              {:id "c" :tool "t" :command "c" :n 3}])))))))

(deftest a-later-wave-sees-the-callers-bindings-too
  (testing "a dependent op in wave 2 runs under the same bindings"
    (with-wave-executor on-fresh-threads
      (fn []
        (is (= {1 "coordinator:batch-caller" 2 "coordinator:batch-caller"}
               (callers-seen [{:id "a" :tool "t" :command "c" :n 1}
                              {:id "b" :tool "t" :command "c" :n 2 :depends_on ["a"]}])))))))
