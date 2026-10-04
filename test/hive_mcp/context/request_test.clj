(ns hive-mcp.context.request-test
  "The HCR caller-directory chain: which directory a scope-aware handler
   works in, and which slot of the chain supplied it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.context.request :as req]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private server-cwd (System/getProperty "user.dir"))

(defn- resolve-in
  "resolve-caller-directory and caller-directory-source for `args`, with the
   request context's directory bound to `ctx-dir`."
  [args ctx-dir]
  (req/with-request-context {:directory ctx-dir}
    [(req/resolve-caller-directory args) (req/caller-directory-source args)]))

(deftest the-chain-picks-the-highest-filled-slot
  (doseq [[args ctx-dir expected]
          [[{:directory "/x" :_caller_cwd "/c"} "/r" ["/x" :explicit]]
           [{:_caller_cwd "/c"}                 "/r" ["/c" :caller-cwd]]
           [{}                                   "/r" ["/r" :request-ctx]]
           [{}                                   nil  [server-cwd :server-cwd]]
           [nil                                  nil  [server-cwd :server-cwd]]]]
    (is (= expected (resolve-in args ctx-dir)) (pr-str [args ctx-dir]))))

(deftest blank-slots-fall-through
  (doseq [blank [nil "" "   " "\t\n"]]
    (testing (pr-str blank)
      (is (= ["/c" :caller-cwd] (resolve-in {:directory blank :_caller_cwd "/c"} "/r")))
      (is (= ["/r" :request-ctx] (resolve-in {:directory blank :_caller_cwd blank} "/r")))
      (is (= [server-cwd :server-cwd] (resolve-in {:directory blank :_caller_cwd blank} blank))
          "a blank request-ctx directory is not reported as :request-ctx"))))

(deftest a-non-string-directory-does-not-throw
  (let [f (java.io.File. "/tmp")]
    (is (= [f :explicit] (resolve-in {:directory f} nil)))))

(def ^:private gen-dir
  (gen/elements [nil "" "  " "/a" "/b/c"]))

(defn- slot-value
  "The value the chain holds in slot `source`."
  [source args ctx-dir]
  (case source
    :explicit    (:directory args)
    :caller-cwd  (:_caller_cwd args)
    :request-ctx ctx-dir
    :server-cwd  server-cwd))

(defspec source-names-the-slot-the-value-came-from 300
  (prop/for-all [dir gen-dir caller gen-dir ctx-dir gen-dir]
    (let [args             {:directory dir :_caller_cwd caller}
          [value source]   (resolve-in args ctx-dir)]
      (= value (slot-value source args ctx-dir)))))
