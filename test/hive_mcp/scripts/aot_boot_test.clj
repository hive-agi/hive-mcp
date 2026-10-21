(ns hive-mcp.scripts.aot-boot-test
  "Golden over the AOT boot script's pure surface: where it writes, which
   protocol namespaces it compiles first, and the shape of the constructor
   namespace list discovered from the classpath manifests. The list itself
   depends on which addons are on the classpath, so the golden records its
   shape (symbols, distinct, every one names an init namespace) rather than
   its members. compile-boot-closure! ends in System/exit and has no exit
   port, so it is not driven here."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.scripts.aot-boot :as aot]
            [hive-test.golden :refer [deftest-golden]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-golden aot-boot-surface-golden
  "test/golden/scripts/aot-boot-surface.edn"
  (let [ctors (aot/ctor-namespaces)]
    {:out-dir              aot/out-dir
     :stamp-file           aot/stamp-file
     :stamp-under-out-dir? (str/starts-with? aot/stamp-file aot/out-dir)
     :protocols-first      @#'aot/protocol-namespaces
     :ctors-vector?        (vector? ctors)
     :ctors-symbols?       (every? symbol? ctors)
     :ctors-distinct?      (= (count ctors) (count (distinct ctors)))}))

(deftest ctor-namespaces-is-repeatable
  (is (= (aot/ctor-namespaces) (aot/ctor-namespaces))))
