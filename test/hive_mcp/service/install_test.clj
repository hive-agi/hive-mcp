(ns hive-mcp.service.install-test
  "The unit install plan is pure, and running it goes through recording
   stub effects: nothing touches ~/.config or systemctl."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-mcp.service.install :as install]
            [hive-test.trifecta :refer [deftrifecta]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private unit "[Unit]\nDescription=x\n")

(defn- plan-ops
  "The ops and paths of the plan for `current` against `unit`."
  [current]
  (mapv (juxt :op :path)
        (install/install-plan {:unit-text unit :dir "/h/.config/systemd/user"
                               :current current})))

(deftrifecta install-plan
  plan-ops
  {:apply? true
   :golden-path "test/golden/service/install-plan.edn"
   :cases {:fresh [nil] :stale ["old"] :unchanged [unit]}
   :gen (gen/fmap vector (gen/one-of [(gen/return nil) (gen/return unit) gen/string]))
   :num-tests 50
   :pred (fn [ops] (or (empty? ops) (= [:mkdir :write :run] (mapv first ops))))
   :mutations [["always-writes"
                (fn [_] [[:mkdir "/h/.config/systemd/user"]
                         [:write "/h/.config/systemd/user/hive-mcp.service"]
                         [:run nil]])]
               ["skips-reload"
                (fn [c] (if (= c unit) []
                            [[:mkdir "/h/.config/systemd/user"]
                             [:write "/h/.config/systemd/user/hive-mcp.service"]]))]]})

(defn- recording-fx
  "Effects that record into `log`; :run-cmd! answers `exit`."
  [log exit]
  {:mkdir! #(swap! log conj [:mkdir %])
   :write! (fn [p _] (swap! log conj [:write p]))
   :run-cmd! (fn [argv] (swap! log conj [:run argv]) {:exit exit :err "boom"})})

(deftest run-plan-performs-steps-in-order-and-never-enables
  (let [steps (install/install-plan {:unit-text unit :dir "/h/u" :current nil})]
    (testing "every step runs, the only command is daemon-reload"
      (let [log (atom [])
            r (install/run-plan! (recording-fx log 0) steps)]
        (is (nil? (:failed r)))
        (is (= [[:mkdir "/h/u"] [:write "/h/u/hive-mcp.service"]
                [:run ["systemctl" "--user" "daemon-reload"]]]
               @log))
        (is (not-any? #(some #{"enable" "start" "restart"} (second %))
                      (filter (comp #{:run} first) @log)))))
    (testing "a failing daemon-reload is reported"
      (let [r (install/run-plan! (recording-fx (atom []) 1) steps)]
        (is (= :run (:op (:failed r))))
        (is (= "boom" (:err (:result r))))))))

(deftest unit-dir-is-the-systemd-user-dir
  (is (= "/home/u/.config/systemd/user" (install/unit-dir "/home/u"))))
