(ns hive-mcp.dns.result.spec-test
  "instrument!/unstrument! switch clojure.spec checking on and off for the
   Result DSL. Both answer the instrumented symbols, and while instrumented a
   call outside its fdef throws; once unstrumented it does not."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.dns.result :as r]
            [hive-mcp.dns.result.spec :as rspec]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private dsl-syms
  #{`r/ok `r/err `r/ok? `r/err? `r/bind `r/map-ok `r/map-err
    `hive-mcp.dns.result.taxonomy/known-error?})

(deftrifecta-from-schema instrument!-trifecta rspec/instrument!
  {:in  [:cat]
   :out [:sequential :symbol]
   :rel (fn [_ syms]
          (try (= dsl-syms (set syms))
               (finally (rspec/unstrument!))))
   :num-tests 5})

(deftrifecta-from-schema unstrument!-trifecta rspec/unstrument!
  {:in  [:cat]
   ;; nothing instrumented: unstrument! answers the symbols it released, a
   ;; subset of the DSL
   :out [:sequential :symbol]
   :rel (fn [_ syms] (every? dsl-syms syms))
   :num-tests 5})

(deftest instrumentation-is-switched-on-and-off
  (try
    (rspec/instrument!)
    (is (thrown? clojure.lang.ExceptionInfo (r/err "not-a-keyword"))
        "instrumented: a string category violates the err fdef")
    (finally (rspec/unstrument!)))
  (is (= {:error "not-a-keyword"} (r/err "not-a-keyword"))
      "unstrumented: the same call goes through"))
