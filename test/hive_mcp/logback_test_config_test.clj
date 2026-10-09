(ns hive-mcp.logback-test-config-test
  "Pins test/resources/logback-test.xml: a test JVM logs to the console only.

   logback loads logback-test.xml before logback.xml, and test/resources is on
   every test alias's path. resources/logback.xml appends to
   ${user.home}/.config/hive-mcp/server.json.log, the file the live server
   writes; a test REPL started with the operator's HOME must not write or roll
   that file. The subject summarises a logback config as data; the assertion
   is made on the summary of the file the test classpath actually resolves.

   Mutants are self-contained: none calls the subject var."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [clojure.xml :as xml]
            [hive-test.trifecta :refer [deftrifecta]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- parse-xml [s]
  (xml/parse (java.io.ByteArrayInputStream. (.getBytes ^String s "UTF-8"))))

(defn config-summary
  "Summarise logback config XML (a string) as
   {:appender-classes [sorted class names] :file-appender? bool
    :user-home? bool}. :file-appender? is true when any appender class is a
   FileAppender (RollingFileAppender included); :user-home? when the config
   references ${user.home} anywhere."
  [xml-str]
  (let [classes (->> (xml-seq (parse-xml xml-str))
                     (filter #(= :appender (:tag %)))
                     (keep #(get-in % [:attrs :class]))
                     sort
                     vec)]
    {:appender-classes classes
     :file-appender?   (boolean (some #(str/includes? % "FileAppender") classes))
     :user-home?       (str/includes? xml-str "${user.home}")}))

(def ^:private console-only
  "<configuration><appender name=\"C\" class=\"ch.qos.logback.core.ConsoleAppender\"/><root level=\"WARN\"><appender-ref ref=\"C\"/></root></configuration>")

(def ^:private prod-like
  "<configuration><property name=\"LOG_DIR\" value=\"${user.home}/.config/hive-mcp\"/><appender name=\"C\" class=\"ch.qos.logback.core.ConsoleAppender\"/><appender name=\"J\" class=\"ch.qos.logback.core.rolling.RollingFileAppender\"><file>${LOG_DIR}/server.json.log</file></appender></configuration>")

(def ^:private plain-file
  "<configuration><appender name=\"F\" class=\"ch.qos.logback.core.FileAppender\"/></configuration>")

(def ^:private gen-config
  (gen/let [n-console (gen/choose 0 2)
            n-file    (gen/choose 0 2)
            home?     gen/boolean]
    (str "<configuration>"
         (when home? "<property name=\"D\" value=\"${user.home}/x\"/>")
         (apply str (for [i (range n-console)]
                      (str "<appender name=\"c" i "\" class=\"ch.qos.logback.core.ConsoleAppender\"/>")))
         (apply str (for [i (range n-file)]
                      (str "<appender name=\"f" i "\" class=\"ch.qos.logback.core.FileAppender\"/>")))
         "</configuration>")))

(deftrifecta config-summary-contract
  hive-mcp.logback-test-config-test/config-summary
  {:golden-path "test/golden/logback/config-summary.edn"
   :cases       {:console-only console-only
                 :prod-like    prod-like
                 :plain-file   plain-file}
   :gen         gen-config
   :pred        (fn [{:keys [appender-classes file-appender? user-home?]}]
                  (and (vector? appender-classes)
                       (boolean? file-appender?)
                       (boolean? user-home?)))
   :num-tests   100
   :mutations   [["never reports a file appender"
                  (fn [s] {:appender-classes [] :file-appender? false
                           :user-home? (str/includes? s "${user.home}")})]
                 ["only RollingFileAppender counts as a file appender"
                  (fn [s] {:appender-classes []
                           :file-appender? (str/includes? s "RollingFileAppender")
                           :user-home? false})]]})

(deftest test-classpath-logback-is-console-only
  (let [res (io/resource "logback-test.xml")]
    (is (some? res) "logback-test.xml must be on the test classpath")
    (when res
      (let [{:keys [appender-classes file-appender? user-home?]}
            (config-summary (slurp res))]
        (is (= ["ch.qos.logback.core.ConsoleAppender"] appender-classes))
        (is (false? file-appender?)
            "a test JVM must not write a log file")
        (is (false? user-home?)
            "a test JVM must resolve no ${user.home} path")))))
