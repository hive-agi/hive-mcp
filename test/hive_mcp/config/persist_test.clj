(ns hive-mcp.config.persist-test
  "set-config-value! persists by read-modify-write against the file, so keys
   edited on disk while the server runs survive a runtime write."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.config.core :as config]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private global-config @#'config/global-config)

(defn- isolate-global-config [f]
  (let [saved @global-config]
    (try (f) (finally (reset! global-config saved)))))

(use-fixtures :each isolate-global-config)

(defn- tmp-config [content]
  (let [f (doto (java.io.File/createTempFile "hive-config-persist" ".edn") .deleteOnExit)]
    (spit f content)
    (.getPath f)))

(deftest runtime-write-keeps-keys-edited-on-disk-after-boot
  (let [path (tmp-config (pr-str {:services {:addons {:mount-compose? true}}}))]
    (reset! global-config {:services {:addons {:mount-compose? true}}})
    (spit path (pr-str {:services {:addons {:mount-compose? true
                                            :lifecycle {:enabled? true}}}}))
    (config/set-config-value! "memory.type-registry.extensions" {:project {:abstraction 2}} path)
    (let [on-disk (edn/read-string (slurp path))]
      (testing "the key edited on disk after boot survives"
        (is (= {:enabled? true} (get-in on-disk [:services :addons :lifecycle]))))
      (testing "the runtime write lands"
        (is (= {:project {:abstraction 2}}
               (get-in on-disk [:memory :type-registry :extensions])))))
    (testing "the in-memory config carries the write"
      (is (= {:project {:abstraction 2}}
             (get-in @global-config [:memory :type-registry :extensions]))))))

(deftest runtime-write-leaves-an-unreadable-file-alone
  (let [broken "{:services {:addons"
        path   (tmp-config broken)]
    (reset! global-config {:services {}})
    (config/set-config-value! "memory.default-store" :milvus path)
    (is (= broken (slurp (io/file path))))
    (is (= :milvus (get-in @global-config [:memory :default-store])))))
