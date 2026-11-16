(ns hive-mcp.plan.status-test
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.plan.status :as status]))

(deftest status-reads-field-then-tag-then-default
  (testing "the field wins when a backend kept it"
    (is (= "active" (status/status-of {:plan-status "active"
                                        :tags ["plan-status:draft"]}))))
  (testing "the tag is the portable fallback"
    (is (= "completed" (status/status-of {:tags ["x" "plan-status:completed"]}))))
  (testing "an entry from before the lifecycle reads as draft"
    (is (= "draft" (status/status-of {:tags ["plan"]})))))

(deftest with-status-keeps-exactly-one
  (is (= ["x" "plan-status:active"]
         (status/with-status ["x" "plan-status:draft"] "active"))))

(deftest conversion-tags-round-trip
  (let [tags (status/with-conversion ["x" "plan-task:old" "plan-wave:a=9"]
                                     ["t1" "t2"] {"a" 0 "b" 1})]
    (is (= ["t1" "t2"] (status/task-ids-of {:tags tags})))
    (is (= {"a" 0 "b" 1} (status/waves-of {:tags tags})))
    (is (= ["x"] (filterv #(not (or (status/task-tag? %)
                                    (status/wave-tag? %)))
                          tags)))))

(deftest fields-are-preferred-over-tags
  (is (= ["t9"] (status/task-ids-of {:task-ids ["t9"] :tags ["plan-task:t1"]})))
  (is (= {"z" 2} (status/waves-of {:waves {"z" 2} :tags ["plan-wave:a=0"]}))))
