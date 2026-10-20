(ns hive-mcp.tools.magit-cli-fallback-test
  (:require [clojure.data.json :as json]
            [clojure.java.shell :as sh]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.magit :as magit]))

(defn- temp-repo []
  (let [dir (str (java.nio.file.Files/createTempDirectory
                  "magit-cli" (make-array java.nio.file.attribute.FileAttribute 0)))]
    (sh/sh "git" "init" "-q" :dir dir)
    (sh/sh "git" "config" "user.name" "test" :dir dir)
    (sh/sh "git" "config" "user.email" "test@example.invalid" :dir dir)
    (sh/sh "git" "config" "commit.gpgsign" "false" :dir dir)
    (spit (str dir "/a.txt") "a\n")
    dir))

(def ^:private cli-op #'magit/cli-op)

(deftest commit-path-ops-run-on-plain-git
  (let [dir (temp-repo)]
    (testing "stage-verify answers the same JSON verdicts the vessel op does"
      (is (= {:status "missing" :path "nope.txt"}
             (json/read-str (:ok (cli-op {:op :magit/stage-verify :directory dir
                                          :paths ["nope.txt"]}))
                            :key-fn keyword)))
      (is (= {:status "ok"}
             (json/read-str (:ok (cli-op {:op :magit/stage-verify :directory dir
                                          :paths ["a.txt"]}))
                            :key-fn keyword))))
    (testing "commit records the message from stdin"
      (is (contains? (cli-op {:op :magit/commit :directory dir :message "first commit"}) :ok))
      (is (= "first commit" (str/trim (:out (sh/sh "git" "log" "-1" "--format=%s" :dir dir))))))
    (testing "ops without a plain-git twin stay unsupported"
      (is (nil? (cli-op {:op :magit/push :directory dir}))))))
