(ns hive-mcp.diag.sizer-test
  "The clj-memory-meter IRetainedSizer agrees with itself: when it reports
   itself unavailable, `retained-size` is `err :diag/sizer-unavailable`; when
   available, it is an ok `{:diag/bytes n}`. Either way, never a throw."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.diag.sizer :as sizer]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.diag.ports :as p]
            [malli.core :as m]))

;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(def ^:private Sized [:map [:ok [:map [:diag/bytes nat-int?]]]])
(def ^:private Unavailable [:map [:error [:= :diag/sizer-unavailable]] [:reason :string]])

(defn- agrees? [s]
  (let [res (p/retained-size s [1 2 3])]
    (if (p/sizer-available? s)
      (m/validate Sized res)
      (m/validate Unavailable res))))

(deftrifecta-from-schema make-sizer-trifecta sizer/make-sizer
  {:in  [:cat]
   :out [:and [:fn #(satisfies? p/IRetainedSizer %)] [:map]]
   :rel (fn [_ s] (agrees? s))
   :num-tests 5})

(deftest availability-and-sizing-agree
  (let [s (sizer/make-sizer)]
    (is (boolean? (p/sizer-available? s)))
    (is (agrees? s))
    (is (not (and (m/validate Sized (p/retained-size s {}))
                  (m/validate Unavailable (p/retained-size s {}))))
        "the two arms never both hold")))
