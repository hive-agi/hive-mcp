(ns hive-mcp.hot.claims-test
  "Owner-scoped dir claims: pure planning, and claim! over a stub extend fn."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.dns.result :as r]
            [hive-mcp.hot.claims :as claims]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def plan
  {:hot/no-reload #{'p}
   :hot/registered [{:addon/id "z.b" :hot/source {:hot/source-dir "/z/src"}}
                    {:addon/id "a.a" :hot/source {:hot/source-dir "/a/src"}}
                    {:addon/id "n.none" :hot/source {}}]})

(deftest owner-claims-is-one-request-per-addon-with-source
  (is (= [{:dirs ["/a/src"] :owner "a.a" :no-reload #{'p}}
          {:dirs ["/z/src"] :owner "z.b" :no-reload #{'p}}]
         (claims/owner-claims plan))))

(deftest base-opts-never-carries-a-dir
  (is (= {:dirs [] :no-reload #{'p} :since 7} (claims/base-opts ['p] 7)))
  (is (not (contains? (claims/base-opts nil nil) :since))))

(deftest claim-runs-each-owner-and-folds-failures
  (let [seen (atom [])
        out  (claims/claim! (fn [req]
                              (swap! seen conj (:owner req))
                              (if (= "z.b" (:owner req))
                                (throw (ex-info "nope" {}))
                                {:dirs (:dirs req) :added (:dirs req)}))
                            (claims/owner-claims plan))]
    (is (= ["a.a" "z.b"] @seen))
    (is (= {"a.a" ["/a/src"]} (:claims out)))
    (is (= ["/a/src"] (:added out)))
    (is (= 1 (count (:errors out))))
    (is (re-find #"^z.b: .*nope" (first (:errors out))))))

(deftest claims-report-reads-an-err-result
  (is (= ["o: gone"]
         (:errors (claims/claims-report [{:owner "o" :dirs ["/d"]}]
                                        [(r/err :x {:message "gone"})])))))

(deftest a-claimed-dir-is-released-by-its-owner-only
  ;; Against the real hive-hot (seam/remove-dirs): what this rule buys.
  (let [extend! (try (requiring-resolve 'hive-hot.core/extend-init!) (catch Throwable _ nil))
        remove! (try (requiring-resolve 'hive-hot.core/remove-dirs!) (catch Throwable _ nil))
        plan!   (try (requiring-resolve 'hive-hot.dirs/plan-removal) (catch Throwable _ nil))]
    (if-not (and extend! remove! plan!)
      (is true "skipped: this hive-hot predates owner claims")
      (testing "pure: a dir claimed by two owners is kept for the other; core is never removed"
        (let [claim  (requiring-resolve 'hive-hot.dirs/claim)
              cl     (-> {} (claim "a.a" ["/a"]) (claim "b.b" ["/a" "/b"]))]
          (is (= ["/b"] (:removed (plan! ["/a" "/b"] ["/a" "/b"] #{} cl "b.b"))))
          (is (= {"/a" ["a.a"]} (:shared (plan! ["/a" "/b"] ["/a" "/b"] #{} cl "b.b"))))
          (is (= [] (:removed (plan! ["/a"] ["/a"] #{"/a"} (claim {} "a.a" ["/a"]) "a.a")))
              "a dir handed to init! (core) is never released, which is why boot claims instead"))))))

;; ---------------------------------------------------------------------------
;; Trifectas over the pure planning fns
;; ---------------------------------------------------------------------------

(def ^:private Owner [:enum :hive.a :hive.b "hive.c"])

(def ^:private Req
  [:map [:owner Owner] [:dirs [:vector {:max 2} [:enum "/a/src" "/b/src" "/c/src"]]]])

(def ^:private Answer
  "What one extend call answered: a map of added dirs, or an err Result."
  [:or [:map [:added [:vector {:max 2} [:enum "/a/src" "/b/src"]]]]
   [:map [:error [:enum :hot/extend-failed]] [:message [:enum "gone" "denied"]]]])

(deftrifecta-from-schema claims-report-folds-answers-per-owner
  #'claims/claims-report
  {:in [:cat [:vector {:max 4} Req] [:vector {:max 4} Answer]]
   :out [:map [:claims [:map-of :string [:vector :string]]]
         [:added [:vector :string]] [:errors [:vector :string]]]
   :rel (fn [[reqs answers] out]
          (let [pairs (map vector reqs answers)
                ok    (remove (comp :error second) pairs)
                bad   (filter (comp :error second) pairs)]
            (= out {:claims (into (sorted-map) (map (fn [[q _]] [(str (:owner q)) (:dirs q)])) ok)
                    :added  (vec (mapcat (comp :added second) ok))
                    :errors (mapv (fn [[q a]] (str (:owner q) ": " (:message a))) bad)})))
   :classify (fn [_ out] (if (seq (:errors out)) :some-errors :clean))
   :classify-domain #{:some-errors :clean}
   :golden-path "test/golden/hive-mcp/hot/claims-report.edn"
   :num-tests 300})

(deftrifecta-from-schema base-opts-never-carries-a-dir
  #'claims/base-opts
  {:in [:cat [:maybe [:or [:set [:enum 'p 'q]] [:vector [:enum 'p 'q]]]] [:maybe [:int {:min 0 :max 99}]]]
   :out [:map [:dirs [:= []]] [:no-reload [:set :any]]]
   :rel (fn [[no-reload since] out]
          (= out (cond-> {:dirs [] :no-reload (set no-reload)} since (assoc :since since))))
   :classify (fn [[_ since] _] (if since :since :no-since))
   :classify-domain #{:since :no-since}
   :num-tests 200})
