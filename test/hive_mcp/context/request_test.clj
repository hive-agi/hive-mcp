(ns hive-mcp.context.request-test
  "The HCR caller-directory chain: which directory a scope-aware handler
   works in, and which slot of the chain supplied it."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.context.request :as req]
            [clojure.string :as str]
            [malli.core :as m]
            [hive-test.golden :as golden]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private server-cwd (System/getProperty "user.dir"))

(defn- resolve-in
  "resolve-caller-directory and caller-directory-source for `args`, with the
   request context's directory bound to `ctx-dir`."
  [args ctx-dir]
  (req/with-request-context {:directory ctx-dir}
    [(req/resolve-caller-directory args) (req/caller-directory-source args)]))

(deftest the-chain-picks-the-highest-filled-slot
  (doseq [[args ctx-dir expected]
          [[{:directory "/x" :_caller_cwd "/c"} "/r" ["/x" :explicit]]
           [{:_caller_cwd "/c"}                 "/r" ["/c" :caller-cwd]]
           [{}                                   "/r" ["/r" :request-ctx]]
           [{}                                   nil  [server-cwd :server-cwd]]
           [nil                                  nil  [server-cwd :server-cwd]]]]
    (is (= expected (resolve-in args ctx-dir)) (pr-str [args ctx-dir]))))

(deftest blank-slots-fall-through
  (doseq [blank [nil "" "   " "\t\n"]]
    (testing (pr-str blank)
      (is (= ["/c" :caller-cwd] (resolve-in {:directory blank :_caller_cwd "/c"} "/r")))
      (is (= ["/r" :request-ctx] (resolve-in {:directory blank :_caller_cwd blank} "/r")))
      (is (= [server-cwd :server-cwd] (resolve-in {:directory blank :_caller_cwd blank} blank))
          "a blank request-ctx directory is not reported as :request-ctx"))))

(deftest a-non-string-directory-does-not-throw
  (let [f (java.io.File. "/tmp")]
    (is (= [f :explicit] (resolve-in {:directory f} nil)))))

(def ^:private gen-dir
  (gen/elements [nil "" "  " "/a" "/b/c"]))

(defn- slot-value
  "The value the chain holds in slot `source`."
  [source args ctx-dir]
  (case source
    :explicit    (:directory args)
    :caller-cwd  (:_caller_cwd args)
    :request-ctx ctx-dir
    :server-cwd  server-cwd))

(defspec source-names-the-slot-the-value-came-from 300
  (prop/for-all [dir gen-dir caller gen-dir ctx-dir gen-dir]
    (let [args             {:directory dir :_caller_cwd caller}
          [value source]   (resolve-in args ctx-dir)]
      (= value (slot-value source args ctx-dir)))))

;; ── Attribution and context construction facets ──────────────────────────

(def ^:private MaybeId
  "An id slot as writers fill it: absent-ish (nil, blank) or a real id."
  [:maybe [:enum "" "  " "agent-a" "coordinator:1"]])

(deftrifecta-from-schema attribution-precedence hive-mcp.context.request/attribution
  {:in [:cat [:map [:verified-id MaybeId] [:caller-id MaybeId] [:legacy-id MaybeId]]]
   :out req/Attribution
   :rel (fn [[{:keys [verified-id caller-id legacy-id]}] out]
          (let [present (fn [s] (when (and s (not (str/blank? s))) s))]
            (cond
              (present verified-id) (= out {:id verified-id :verified? true :source :verified})
              (present caller-id)   (= out {:id caller-id :verified? false :source :caller})
              :else                 (= out {:id (present legacy-id) :verified? false
                                            :source :legacy}))))
   :num-tests 300})

(deftest attribution-derivatives
  (golden/assert-golden
   (golden/anchor 'hive-mcp.context.request-test "test/golden/context/attribution.edn")
   (vec (for [a [{:id "v" :verified? true :source :verified}
                 {:id "c" :verified? false :source :caller}
                 {:id "l" :verified? false :source :legacy}
                 {:id nil :verified? false :source :legacy}]]
          {:tags       (req/attribution-tags a)
           :created-by (req/attribution-created-by a "system")}))))

(deftest context-construction-and-reads
  (let [ctx (req/make-request-ctx {:agent-id "a" :caller-id "coordinator:9"
                                   :project-id "p" :directory "/d"
                                   :session-id "s" :timestamp #inst "2026-01-01"})]
    (is (m/validate req/RequestCtx ctx))
    (is (= 1 (:depth ctx)))
    (is (= 2 (:depth (req/increment-depth ctx))))
    (is (= 1 (:depth (req/increment-depth {}))))
    (is (nil? (req/request-cache-stats)))
    (req/with-request-context ctx
      (is (= ["a" "p" "s" 1 #inst "2026-01-01" "coordinator:9" ctx]
             [(req/current-agent-id) (req/current-project-id) (req/current-session-id)
              (req/current-depth) (req/current-timestamp) (req/current-caller-id)
              (req/request-ctx)]))
      (is (nil? (req/current-identity)))
      (is (nil? (req/current-verified-caller-id)))
      (is (= {:id "coordinator:9" :verified? false :source :caller}
             (req/current-attribution "legacy")))
      (is (= "a" (req/current-session-agent-id)))
      (let [calls (atom 0)
            f     #(swap! calls inc)]
        (req/request-memoize :k f)
        (req/request-memoize :k f)
        (is (= 1 @calls))
        (is (= {:entry-count 1 :keys [:k]} (update (req/request-cache-stats) :keys vec)))))
    (req/with-request-context (assoc ctx :identity {:verified? true :caller-id "ling-1"})
      (is (= "ling-1" (req/current-verified-caller-id) (req/current-caller-id))))))
