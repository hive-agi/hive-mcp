#!/usr/bin/env bb
;; bin/test-heap-cap-check.bb — fails when a :test* alias in deps.edn states
;; no -Xmx heap ceiling.
;;
;;   bin/test-heap-cap-check.bb               ; checks ./deps.edn
;;   bin/test-heap-cap-check.bb path/deps.edn ; checks that file
;;   bin/test-heap-cap-check.bb --self-test   ; proves the check on fixtures
;;
;; Exit 0 when every :test* alias carries -Xmx, 1 otherwise.
(ns test-heap-cap-check
  (:require [clojure.edn :as edn]
            [clojure.string :as str]))

(defn test-alias? [k]
  (str/starts-with? (name k) "test"))

(defn capped? [alias-map]
  (boolean (some #(and (string? %) (str/starts-with? % "-Xmx"))
                 (:jvm-opts alias-map))))

(defn uncapped-test-aliases
  "Sorted :test* alias keys of deps map `deps` whose :jvm-opts carry no -Xmx."
  [deps]
  (->> (:aliases deps)
       (filter (fn [[k v]] (and (test-alias? k) (not (capped? v)))))
       (map key)
       sort
       vec))

(defn test-aliases [deps]
  (->> (:aliases deps) keys (filter test-alias?) sort vec))

(defn check-file [path]
  (let [deps     (edn/read-string (slurp path))
        all      (test-aliases deps)
        uncapped (uncapped-test-aliases deps)]
    (cond
      (empty? all)
      (do (println "test-heap-cap-check: FAIL no :test* alias found in" path)
          false)

      (seq uncapped)
      (do (println "test-heap-cap-check: FAIL :test* aliases without -Xmx in"
                   path ":" (str/join " " uncapped))
          false)

      :else
      (do (println "test-heap-cap-check: ok" (count all) "test aliases capped:"
                   (str/join " " all))
          true))))

(defn self-test []
  (let [cases [["all capped passes" []
                {:aliases {:test {:jvm-opts ["-Xmx2g"]}
                           :test-unit {:jvm-opts ["-Dx=y" "-Xmx3g"]}
                           :dev {}}}]
               ["missing jvm-opts fails" [:test-new]
                {:aliases {:test {:jvm-opts ["-Xmx2g"]} :test-new {}}}]
               ["jvm-opts without Xmx fails" [:test]
                {:aliases {:test {:jvm-opts ["-Dhive.kg.backend=datascript"]}}}]
               ["Xms is not a ceiling" [:test-focus]
                {:aliases {:test-focus {:jvm-opts ["-Xms2g"]}}}]
               ["non-test alias ignored" []
                {:aliases {:mcp {} :test {:jvm-opts ["-Xmx2g"]}}}]]
        results (for [[nm want deps] cases
                      :let [got (uncapped-test-aliases deps)]]
                  (do (if (= want got)
                        (println "ok  " nm)
                        (println "FAIL" nm "want" want "got" got))
                      (= want got)))
        n-fail (count (remove true? (doall results)))]
    (println "test-heap-cap-check self-test:" (- (count cases) n-fail) "passed," n-fail "failed")
    (zero? n-fail)))

(let [[arg] *command-line-args*
      ok? (if (= arg "--self-test")
            (self-test)
            (check-file (or arg "deps.edn")))]
  (System/exit (if ok? 0 1)))
