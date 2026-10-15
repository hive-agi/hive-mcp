(ns hive-mcp.channel.payload-ref-trifecta-test
  "Reference elision preserves readable previews and fetchable full payloads."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.channel.payload-ref :as subject]
            [hive-test.trifecta :refer [deftrifecta]]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Generator is derived from the structural input schema, not hand-written.
(def ^:private payload-input [:string {:gen/max 1000}])

(deftrifecta reference-needed
  hive-mcp.channel.payload-ref/needs-ref?
  {:golden-path "test/golden/channel/payload-ref-needed.edn"
   :cases {:short "hello" :edge (apply str (repeat 400 "a"))
           :long (apply str (repeat 401 "a"))}
   :gen (mg/generator payload-input)
   :pred boolean?
   :num-tests 200
   :mutations [["never elide" (constantly false)]
               ["always elide" (constantly true)]]
   :assert (fn [] (is (false? (subject/needs-ref? "hi")))
              (is (true? (subject/needs-ref? (apply str (repeat 401 "a"))))))})

(deftest elision-keeps-payload-recoverable
  (let [full (apply str (repeat 430 "ab"))
        stored (atom nil)
        out (subject/elide! full 100 (fn [body] (reset! stored body) "ctx-1"))]
    (is (= full @stored))
    (is (= "ctx-1" (subject/parse-ref (:text out))))
    (is (= "ctx-1" (:ref out)))
    (is (<= (count (:text out)) 100))))
