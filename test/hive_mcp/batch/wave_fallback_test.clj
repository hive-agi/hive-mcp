(ns hive-mcp.batch.wave-fallback-test
  "Wave assignment and cycle detection with NO :bx/* extension registered.

   This is the path production takes. Every other multi test installs
   hive-mcp.test.stub.batch-extensions, which registers :bx/i as
   hive.events.multi/assign-waves (the correct implementation), so those
   suites were green against code the unextended host never ran. Here the
   extension lookup is forced to miss."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.batch :as batch]
            [hive-mcp.test.stub.batch-extensions :as stub]
            [hive-mcp.extensions.registry :as ext]
            [hive-test.trifecta :refer [deftrifecta]]
            [clojure.test.check.generators :as gen]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- no-extensions
  "Force every :bx/* extension lookup to miss, whatever the global registry
   holds. Hermetic: independent of test order and of any stub another
   namespace installed."
  [f]
  (stub/with-no-batch-extensions f))

(deftest fallback-fixture-restores-batch-registrations
  (let [batch-key :bx/fixture-test
        other-key :unrelated/fixture-test
        new-key :bx/fixture-new
        prior (into {} (map (fn [k] [k (ext/get-extension k)]))
                    [batch-key other-key new-key])
        original (fn [] :original)
        other (fn [] :other)
        temporary (fn [] :temporary)]
    (try
      (ext/register! batch-key original)
      (ext/register! other-key other)
      (is (= "fixture exit"
             (try
               (no-extensions
                (fn []
                  (is (nil? (ext/get-extension batch-key)))
                  (is (identical? other (ext/get-extension other-key)))
                  (ext/register! new-key temporary)
                  (throw (ex-info "fixture exit" {}))))
               (catch clojure.lang.ExceptionInfo e (.getMessage e)))))
      (is (identical? original (ext/get-extension batch-key)))
      (is (nil? (ext/get-extension new-key)))
      (is (identical? other (ext/get-extension other-key)))
      (finally
        (doseq [[k v] prior]
          (if v (ext/register! k v) (ext/deregister! k)))))))

(deftrifecta batch-extension-keys-trifecta
  hive-mcp.test.stub.batch-extensions/batch-extension-keys
  {:golden-path "test/golden/batch/extension-keys.edn"
   :cases {:mixed [:bx/a :other/x :bx/new]
           :empty []
           :no-batch [:other/x :tools/y]}
   :gen (gen/vector (gen/elements [:bx/a :bx/new :other/x :tools/y]) 0 20)
   :pred (fn [result] (and (vector? result) (every? #(= "bx" (namespace %)) result)))
   :num-tests 100
   :mutations [["keep-all" (fn [keys] (vec keys))]
               ["drop-all" (fn [_] [])]]
   :assert (fn []
             (is (= [:bx/a :bx/new]
                    (stub/batch-extension-keys [:other/x :bx/a :bx/new]))))})

(defn- waves-by-id [ops]
  (into {} (map (juxt :id :wave)) (batch/assign-waves ops)))

(deftest fallback-assigns-a-chain-to-successive-waves
  (testing "a -> b -> c occupies three waves, not one"
    (no-extensions
     (fn []
       (let [w (waves-by-id [{:id "a" :depends_on []}
                             {:id "b" :depends_on ["a"]}
                             {:id "c" :depends_on ["b"]}])]
         (is (= 1 (get w "a")))
         (is (= 2 (get w "b")))
         (is (= 3 (get w "c"))
             (str "with every op in wave 1, check-deps-satisfied (which only "
                  "consults PRIOR-wave results and reads a missing result as "
                  "failed) errors every dependent op with 'dependencies failed'")))))))

(deftest fallback-keeps-independent-ops-in-one-wave
  (testing "no dependencies means no serialisation"
    (no-extensions
     (fn []
       (let [w (waves-by-id [{:id "a" :depends_on []}
                             {:id "b" :depends_on []}
                             {:id "c" :depends_on []}])]
         (is (= #{1} (set (vals w)))))))))

(deftest fallback-handles-a-diamond
  (testing "b and c both depend on a, d on both: three waves, b and c share one"
    (no-extensions
     (fn []
       (let [w (waves-by-id [{:id "a" :depends_on []}
                             {:id "b" :depends_on ["a"]}
                             {:id "c" :depends_on ["a"]}
                             {:id "d" :depends_on ["b" "c"]}])]
         (is (= 1 (get w "a")))
         (is (= (get w "b") (get w "c")) "independent siblings run together")
         (is (< (get w "b") (get w "d")) "the join waits for both branches"))))))

(deftest fallback-detects-a-real-cycle
  (testing "x <-> y is reported, not validated as acyclic"
    (no-extensions
     (fn []
       (let [errs (#'batch/detect-cycles
                   [{:id "x" :tool "t" :command "c" :depends_on ["y"]}
                    {:id "y" :tool "t" :command "c" :depends_on ["x"]}])]
         (is (seq errs)
             "returning [] unconditionally let a genuine cycle pass as {:valid true}")
         (is (some #(re-find #"(?i)circular" (str %)) errs)))))))

(deftest cycle-check-is-staged-behind-structural-errors
  (testing "ops missing :tool report that, and the cycle only surfaces once they are well-formed"
    (no-extensions
     (fn []
       (is (empty? (#'batch/detect-cycles [{:id "x" :depends_on ["y"]}
                                           {:id "y" :depends_on ["x"]}]))
           (str "hive.events.multi/validate-ops short-circuits on the first error "
                "class, so a malformed op hides the cycle. batch makes its own "
                "structural checks alongside this one, so nothing is lost: the "
                "caller fixes :tool, re-runs, and then sees the cycle."))))))

(deftest fallback-reports-no-cycle-for-an-acyclic-graph
  (testing "the cycle check does not fire on a plain chain"
    (no-extensions
     (fn []
       (is (empty? (#'batch/detect-cycles
                    [{:id "a" :tool "t" :command "c" :depends_on []}
                     {:id "b" :tool "t" :command "c" :depends_on ["a"]}])))))))
