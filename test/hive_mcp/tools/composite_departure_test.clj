(ns hive-mcp.tools.composite-departure-test
  "A call to a contributed subdomain while its addon remounts (retracted, not
   yet re-contributed) waits for the subdomain to return, and answers a
   retryable error naming the subdomain when it does not, never
   `Unknown command`. A root that never existed is still unknown."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.extensions.departures :as departures]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.composite :as composite]
            [hive-mcp.tools.composite.departed :as departed]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private canonical
  {:status (fn [_] {:type "text" :text "core status"})})

(defn- ss-contribution
  "A stub `ss` subdomain whose handler answers TAG with the command it saw."
  [tag]
  {"ss" {:handler (fn [{:keys [command]}] {:type "text" :text (str tag " " command)})
         :description "stub subdomain"}})

(defn- text [response] (str (:text response)))

(defn- with-tool
  "Run F with a fresh tool name, retracting the stub addon afterwards."
  [f]
  (let [tool (str "departure-test-" (random-uuid))]
    (try (f tool)
         (finally (ext/retract-commands! tool :stub-addon)))))

(deftest a-miss-on-a-departed-subdomain-is-retryable
  (with-tool
    (fn [tool]
      (ext/contribute-commands! tool :stub-addon (ss-contribution "v1"))
      (let [handler (composite/build-merged-handler tool canonical)]
        (is (= "v1 ss wait" (text (handler {:command "ss wait"}))))
        (ext/retract-all-by-addon! :stub-addon)
        (binding [departed/*return-wait* {:budget-ms 60 :poll-ms 10
                                          :reload-window-ms 60000}]
          (let [res (handler {:command "ss wait"})]
            (is (:isError res))
            (is (not (str/includes? (text res) "Unknown command")))
            (is (str/includes? (text res) "Subdomain 'ss' is reloading"))
            (is (str/includes? (text res) "Retryable"))
            (is (str/includes? (text res) "status")
                "the commands valid now are listed")))))))

(deftest a-never-contributed-root-is-still-unknown
  (with-tool
    (fn [tool]
      (ext/contribute-commands! tool :stub-addon (ss-contribution "v1"))
      (let [handler (composite/build-merged-handler tool canonical)
            res     (handler {:command "nope wait"})]
        (is (:isError res))
        (is (str/includes? (text res) "Unknown command"))
        (is (not (str/includes? (text res) "Retryable")))))))

(deftest a-long-departed-subdomain-answers-not-loaded-without-waiting
  (with-tool
    (fn [tool]
      (departures/record! tool :stub-addon ["ss"] 0)
      (let [handler (composite/build-composite-handler tool)
            t0      (System/currentTimeMillis)
            res     (handler {:command "ss wait"})]
        (is (< (- (System/currentTimeMillis) t0) 1000) "no wait on an old departure")
        (is (:isError res))
        (is (str/includes? (text res) "Subdomain 'ss' is not loaded on this server"))
        (is (not (str/includes? (text res) "Retryable")))
        (is (not (str/includes? (text res) "Unknown command")))))))

(deftest concurrent-dispatch-during-remount-never-sees-unknown-command
  (with-tool
    (fn [tool]
      (ext/contribute-commands! tool :stub-addon (ss-contribution "v1"))
      (let [handler (composite/build-merged-handler tool canonical)]
        (binding [departed/*return-wait* {:budget-ms 5000 :poll-ms 5
                                          :reload-window-ms 60000}]
          ;; the remount's teardown half: the subdomain is gone
          (ext/retract-all-by-addon! :stub-addon)
          (let [calls (doall (for [_ (range 8)] (future (handler {:command "ss wait"}))))]
            (Thread/sleep 50)
            ;; the mount half: the new instance contributes the subdomain again
            (ext/contribute-commands! tool :stub-addon (ss-contribution "v2"))
            (let [results (mapv #(deref % 6000 ::timeout) calls)]
              (is (not-any? #{::timeout} results))
              (is (every? #(= "v2 ss wait" (text %)) results)
                  "every call that landed in the gap ran on the new instance"))))))))

(deftest a-retraction-records-the-departure
  (with-tool
    (fn [tool]
      (ext/contribute-commands! tool :stub-addon (ss-contribution "v1"))
      (testing "retract-commands!"
        (ext/retract-commands! tool :stub-addon)
        (is (= :stub-addon (:addon (departures/departure tool "ss")))))
      (testing "retract-all-by-addon!"
        (let [tool2 (str tool "-all")]
          (ext/contribute-commands! tool2 :stub-addon-2 (ss-contribution "v1"))
          (ext/retract-all-by-addon! :stub-addon-2)
          (is (= :stub-addon-2 (:addon (departures/departure tool2 "ss"))))))
      (testing "a command another addon owns is not recorded for this one"
        (is (nil? (departures/departure tool "status")))))))
