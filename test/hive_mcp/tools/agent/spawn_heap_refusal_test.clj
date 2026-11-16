(ns hive-mcp.tools.agent.spawn-heap-refusal-test
  "The Layer 4 spawn refusal (friction 20261111145728-573b7979). Nothing is
   queued on a heap-pressure refusal and the brief is dropped, so the refusal
   must not claim to defer, must say the brief was not kept, and must carry
   the measured figures and a retry hint."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.system.heap-admission :as heap-admission]
            [hive-mcp.tools.agent.spawn]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private heap-refusal @#'hive-mcp.tools.agent.spawn/heap-refusal)

(def ^:private hard-post-gc
  {:level :hard :basis :post-gc :heap-pct 94 :now-pct 97 :post-gc-pct 94
   :pool "G1 Old Gen" :watermarks {:soft 0.80 :hard 0.92}})

(deftest the-refusal-says-what-happened-to-the-brief
  (let [r (heap-refusal hard-post-gc)]
    (testing "a refusal, and nothing claimed to be queued"
      (is (false? (:success r)))
      (is (true? (:refused r)))
      (is (not (contains? r :deferred)))
      (is (false? (:brief_kept r)))
      (is (str/includes? (:message r) "Nothing was queued"))
      (is (str/includes? (:message r) "brief was NOT kept")))
    (testing "the measured figures and the retry hint"
      (is (= {:reason "heap-pressure" :level "hard" :basis "post-gc"
              :heap_post_gc_pct 94 :heap_now_pct 97 :hard_watermark_pct 92
              :retry_after_s heap-admission/retry-after-s}
             (select-keys r [:reason :level :basis :heap_post_gc_pct :heap_now_pct
                             :hard_watermark_pct :retry_after_s])))
      (is (pos? (:retry_after_s r)))
      (is (str/includes? (:message r) "94% full after its last old-gen collection (G1 Old Gen)"))
      (is (str/includes? (:message r) "97% in use right now"))
      (is (str/includes? (:message r) (str "Retry after " heap-admission/retry-after-s "s"))))))

(deftest the-instant-basis-is-named
  (let [r (heap-refusal {:level :hard :basis :instant :heap-pct 95 :now-pct 95
                         :watermarks {:hard 0.92}})]
    (is (= "instant" (:basis r)))
    (is (nil? (:heap_post_gc_pct r)))
    (is (= 95 (:heap_now_pct r)))
    (is (str/includes? (:message r) "no post-GC reading"))))
