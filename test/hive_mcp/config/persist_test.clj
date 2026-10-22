(ns hive-mcp.config.persist-test
  "set-config-value! persists by read-modify-write against the file, so keys
   edited on disk while the server runs survive a runtime write."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.io :as config-io]
            [hive-mcp.config.path :as config-path]))
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

(defn- boot-load-at
  "Run a boot load with `path` as THE canonical config path, so the write-back
   branch runs against a temp file instead of the developer's real config."
  [path]
  (with-redefs [config-io/config-path        path
                config-io/legacy-config-path (str path ".no-legacy")]
    (config/load-global-config! path)))

(deftest boot-write-plan-only-generates-when-the-file-is-missing
  (is (= :generate (config/boot-write-plan false {:ok nil})))
  (is (= :merge-back (config/boot-write-plan true {:ok {:a 1}})))
  (is (= :refuse (config/boot-write-plan true {:ok nil})))
  (is (= :refuse (config/boot-write-plan true {:error :io/config-read}))))

(deftest boot-load-never-overwrites-an-unreadable-config
  (doseq [content ["{:services {:datahike {:path \"/real\"}"   ; half written
                   ""                                          ; empty
                   "[:not :a :map]"]]                          ; not a map
    (let [path (tmp-config content)]
      (boot-load-at path)
      (testing (str "the file is left exactly as it was: " (pr-str content))
        (is (= content (slurp (io/file path))))))))

(deftest boot-load-keeps-the-operator-config
  (let [path (tmp-config (pr-str {:services {:datahike {:path "/real/kg"}}
                                  :memory   {:default-store :milvus}}))
        loaded (boot-load-at path)
        on-disk (edn/read-string (slurp path))]
    (testing "the in-memory config is the file's, not the defaults"
      (is (= "/real/kg" (get-in loaded [:services :datahike :path])))
      (is (= :milvus (get-in loaded [:memory :default-store]))))
    (testing "the write-back keeps the operator's values"
      (is (= "/real/kg" (get-in on-disk [:services :datahike :path])))
      (is (= :milvus (get-in on-disk [:memory :default-store]))))))

(deftest boot-load-generates-a-missing-config
  (let [path (str (tmp-config "") ".missing")]
    (boot-load-at path)
    (is (map? (edn/read-string (slurp path))))
    (io/delete-file path true)))

(deftest test-jvms-never-write-the-operator-config
  (testing "every test alias marks the JVM and moves the config path off ~/.config"
    (is (config-path/test-jvm?))
    (is (not= config-path/operator-config-path config-io/config-path)))
  (testing "a write aimed at the operator's config is refused and leaves it alone"
    (let [real (tmp-config "{:operator :config}")]
      (with-redefs [config-path/operator-config-path real]
        (is (= :io/config-write-refused (:error (config-io/write-config! {:defaults true} real))))
        (is (= "{:operator :config}" (slurp (io/file real))))
        (testing "a boot load at that path cannot clobber it either"
          (boot-load-at real)
          (is (= "{:operator :config}" (slurp (io/file real)))))))))
