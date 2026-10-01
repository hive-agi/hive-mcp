(ns hive-mcp.config.ling-defaults-test
  "The default ling spawn mode: shipped :headless, operator-settable.
   Precedence is tested on the PURE `pick` — config.edn value and env value
   are inputs, so no var is redefined and no file is touched."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.config.ling-defaults :as ling-defaults]
            [hive-mcp.config.schema :as config-schema]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest shipped-default-is-headless
  (is (= :headless ling-defaults/shipped-spawn-mode))
  (is (= :headless (ling-defaults/pick nil nil))
      "nothing configured anywhere -> headless"))

(deftest config-edn-beats-env-beats-shipped
  (testing "config.edn wins over env"
    (is (= :claude (ling-defaults/pick :claude :hive-agent))))
  (testing "env used when config.edn is absent"
    (is (= :hive-agent (ling-defaults/pick nil :hive-agent))))
  (testing "string values from config.edn coerce, leading colon tolerated"
    (is (= :claude (ling-defaults/pick "claude" nil)))
    (is (= :vterm (ling-defaults/pick ":vterm" nil))))
  (testing "garbage at a tier falls through to the next, never to nil"
    (is (= :hive-agent (ling-defaults/pick "  " :hive-agent)))
    (is (= :headless (ling-defaults/pick 42 "")))
    (is (= :headless (ling-defaults/pick {} nil)))))

(defspec pick-always-answers-a-keyword 200
  (prop/for-all [edn-val gen/any-printable-equatable
                 env-val gen/any-printable-equatable]
    (keyword? (ling-defaults/pick edn-val env-val))))

(defspec a-keyword-in-config-edn-always-wins 200
  (prop/for-all [k gen/keyword env-val gen/any-printable-equatable]
    (= k (ling-defaults/pick k env-val))))

(deftest config-schema-accepts-the-ling-section
  (is (:valid? (config-schema/validate-config {:ling {:default-spawn-mode :claude}})))
  (is (:valid? (config-schema/validate-config {:ling {:default-spawn-mode "headless"}})))
  (is (not (:valid? (config-schema/validate-config {:ling {:default-spawn-mode 7}})))))
