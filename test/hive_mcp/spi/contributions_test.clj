(ns hive-mcp.spi.contributions-test
  "Facets for hive-mcp.spi.contributions (cov-facets f07). The registry is
   driven under a private kind (:f07/*) and reset with reset-kind!, so no
   real kernel contribution is touched. load-manifest! reads a fixture
   resource (test/resources/hive-mcp/f07-contributions-fixture.edn) whose
   entries resolve, fail to resolve, and partially resolve."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.spi.contributions :as contrib]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private fixture "hive-mcp/f07-contributions-fixture.edn")

(defn- project-entry
  "An entry without its process-global :contrib/seq (an ever-increasing
   counter) and with resolved fns/vars projected to their name, so the golden
   is plain, stable EDN."
  [e]
  (some-> e
          (dissoc :contrib/seq)
          (update-vals #(cond (var? %) (str (symbol %))
                              (fn? %)  :fn
                              :else    %))))

(deftest-golden contributions-registry-golden
  "test/golden/hive-mcp/f07-contributions-registry.edn"
  (let [ran (atom [])]
    (contrib/reset-kind! :f07/kind)
    (try
      ;; Every step is sequenced in the let: a map literal of more than 8
      ;; entries does not evaluate its values in the order written.
      (let [k1 (contrib/contribute! :f07/kind :b {:install! #(swap! ran conj :b)})
            k2 (contrib/contribute! :f07/kind :a {:install! #(swap! ran conj :a) :addon :x})
            _  (contrib/contribute! :f07/kind :boom {:install! #(throw (ex-info "boom" {}))})
            _  (contrib/contribute! :f07/kind :inert {:payload 1})
            ;; re-contributing :b keeps its original position
            _  (contrib/contribute! :f07/kind :b {:install! #(swap! ran conj :b2)})
            order      (mapv key (contrib/ordered :f07/kind))
            report     (contrib/register-all! :f07/kind)
            entry-a    (project-entry (contrib/entry :f07/kind :a))
            entry-none (contrib/entry :f07/kind :none)
            forgot     (contrib/forget! :f07/kind :a)
            after-forget (vec (sort (keys (contrib/contributed :f07/kind))))
            reset      (contrib/reset-kind! :f07/kind)
            after-reset (contrib/contributed :f07/kind)]
        {:returned    [k1 k2]
         :order       order
         :ran         (:ran report)
         :failed      (vec (sort (keys (:failed report))))
         :side-effect @ran
         :entry-a     entry-a
         :entry-none  entry-none
         :forget      [forgot after-forget]
         :reset       [reset after-reset]})
      (finally (contrib/reset-kind! :f07/kind)))))

(deftest-golden contributions-manifest-golden
  "test/golden/hive-mcp/f07-contributions-manifest.edn"
  (do
    (contrib/reset-kind! :f07/tools)
    (try
      {:read-missing (contrib/read-manifest "hive-mcp/f07-no-such-manifest.edn")
       :read-kinds   (vec (keys (contrib/read-manifest fixture)))
       :load         (contrib/load-manifest! fixture
                                             {:resolve-keys #{:install! :describe}
                                              :required-key :install!})
       :present      (project-entry (contrib/entry :f07/tools :present))
       :partial      (project-entry (contrib/entry :f07/tools :partial))
       :absent       (contrib/entry :f07/tools :absent)}
      (finally (contrib/reset-kind! :f07/tools)))))

(deftest contribute-requires-a-keyword-kind-and-a-map
  (is (thrown? AssertionError (contrib/contribute! "tools" :k {})))
  (is (thrown? AssertionError (apply contrib/contribute! [:f07/kind :k :not-a-map]))))
