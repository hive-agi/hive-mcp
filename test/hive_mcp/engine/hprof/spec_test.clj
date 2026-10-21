(ns hive-mcp.engine.hprof.spec-test
  "Facets for the hprof policy defaults: caller overrides layered onto
   `default-policy`, driven by the merge-policy contract."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.engine.hprof.spec :as spec]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Overrides
  "Override maps: any subset of the policy keys at their declared types.
   :dir is always overridden because the default reads HIVE_HPROF_DIR and
   user.home, which would make the golden host-specific; the nil arm is
   pinned in `nil-overrides-are-the-defaults`."
  [:map
   [:dir [:enum "/tmp/hprof-a" "/var/hprof"]]
   [:keep-n {:optional true} [:int {:min 0 :max 9}]]
   [:gzip? {:optional true} :boolean]
   [:min-free-gb {:optional true} [:int {:min 0 :max 50}]]])

(deftrifecta-from-schema merge-policy-caller-wins
  #'spec/merge-policy
  {:in [:cat Overrides]
   :out [:map [:dir :string] [:keep-n :int] [:gzip? :boolean] [:min-free-gb :int]]
   :rel (fn [[overrides] out]
          (and (= out (merge spec/default-policy overrides))
               (every? (fn [[k v]] (= v (get out k))) overrides)
               (= #{:dir :keep-n :gzip? :min-free-gb} (set (keys out)))))
   :classify (fn [[overrides] _] (if (contains? overrides :keep-n) :keep-n-overridden :keep-n-default))
   :classify-domain #{:keep-n-overridden :keep-n-default}
   :golden-path "test/golden/engine/hprof-merge-policy.edn"
   :num-tests 200})

(deftest nil-overrides-are-the-defaults
  (is (= spec/default-policy (spec/merge-policy nil)))
  (is (= 7 (:keep-n (spec/merge-policy {:keep-n 7})))))
