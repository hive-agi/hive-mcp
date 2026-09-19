(ns hive-mcp.addons.tool-claims-test
  "Tests for the pure addon tool-name resolution.

   `resolve-claims` is values-in / values-out, so every case here is built
   from literals: no registry, no addon instances, no redefinition. The four
   rules it implements, in the order it applies them:

   1. CLAIM            provide N + exclude N  -> hold N over core and others
   2. EXCLUSION        exclude N, no provider -> refuse other addons, keep core
   3. CORE             core N wins            -> :shadows-core
   4. FIRST PROVIDER   otherwise              -> :duplicate for the rest"
  (:require [clojure.test :refer [deftest testing is]]
            [hive-mcp.addons.tool-claims :as claims]))

;; =============================================================================
;; Builders
;; =============================================================================

(defn- tool
  ([n] {:name n})
  ([n extra] (merge {:name n} extra)))

(defn- contribution
  "One addon's contribution. `tools` are names or tool maps."
  [id tools & {:keys [excluded addon-type]}]
  (cond-> {:addon-id id
           :tools    (mapv #(if (string? %) (tool %) %) tools)}
    excluded   (assoc :excluded (set excluded))
    addon-type (assoc :addon-type addon-type)))

(defn- installed-names [res] (mapv :name (:installed res)))

(defn- refusal
  "The single refusal for `tool-name`, or nil."
  [res tool-name]
  (first (filter #(= tool-name (:tool %)) (:refused res))))

;; =============================================================================
;; Rule 4: first provider wins
;; =============================================================================

(deftest first-provider-holds-a-contested-name
  (let [res (claims/resolve-claims
             []
             [(contribution :a ["shared" "only-a"])
              (contribution :b ["shared" "only-b"])])]
    (testing "the winner keeps the name, tagged with its source"
      (is (= ["shared" "only-a" "only-b"] (installed-names res)))
      (is (= :a (:addon-source (first (:installed res))))))
    (testing "the loser is refused as a duplicate, naming who holds it"
      (is (= {:addon-id :b :tool "shared" :reason :duplicate :holder :a}
             (refusal res "shared"))))
    (is (empty? (:claims res)))
    (is (claims/valid-resolution? res))))

(deftest registration-order-decides-and-nothing-else
  (testing "the same two addons in the other order give the other holder"
    (let [res (claims/resolve-claims
               []
               [(contribution :b ["shared"])
                (contribution :a ["shared"])])]
      (is (= :b (:addon-source (first (:installed res)))))
      (is (= :a (:addon-id (refusal res "shared")))))))

(deftest a-name-repeated-inside-one-addon-keeps-its-first-definition
  (let [res (claims/resolve-claims
             []
             [(contribution :a [(tool "dup" {:v 1}) (tool "dup" {:v 2})])])]
    (is (= ["dup"] (installed-names res)))
    (is (= 1 (:v (first (:installed res)))) "the FIRST definition survives")
    (is (= {:addon-id :a :tool "dup" :reason :duplicate :holder :a}
           (refusal res "dup"))
        "the repeat is refused against the addon itself")))

;; =============================================================================
;; Rule 3: core wins
;; =============================================================================

(deftest an-addon-tool-never-silently-shadows-a-core-tool
  (let [res (claims/resolve-claims
             [{:name "memory"}]
             [(contribution :a ["memory" "novel"])])]
    (is (= ["novel"] (installed-names res)) "the core name is not installed")
    (is (= {:addon-id :a :tool "memory" :reason :shadows-core :holder :core}
           (refusal res "memory")))
    (is (empty? (:claims res)))))

(deftest the-legacy-consolidated-supertool-still-stands-in-for-core
  (testing "a :native addon's consolidated tool over a consolidated core root"
    (let [res (claims/resolve-claims
               [{:name "code" :consolidated true}]
               [(contribution :native-addon [(tool "code" {:consolidated true})]
                              :addon-type :native)])]
      (is (= ["code"] (installed-names res)))
      (is (= [{:addon-id :native-addon :tool "code"
               :how :legacy-consolidated :over :core}]
             (:claims res)))))

  (testing "but only when BOTH sides are consolidated and the addon is :native"
    (doseq [[label core addon]
            [["addon not consolidated"
              {:name "code" :consolidated true}
              (contribution :x [(tool "code")] :addon-type :native)]
             ["core not consolidated"
              {:name "code"}
              (contribution :x [(tool "code" {:consolidated true})] :addon-type :native)]
             ["addon not :native"
              {:name "code" :consolidated true}
              (contribution :x [(tool "code" {:consolidated true})] :addon-type :mcp-bridge)]]]
      (testing label
        (let [res (claims/resolve-claims [core] [addon])]
          (is (empty? (installed-names res)))
          (is (= :shadows-core (:reason (refusal res "code")))))))))

;; =============================================================================
;; Rule 2: exclusion without a provider
;; =============================================================================

(deftest an-exclusion-with-no-provider-refuses-other-addons
  (let [res (claims/resolve-claims
             []
             [(contribution :guard [] :excluded ["banned"])
              (contribution :b ["banned" "fine"])])]
    (is (= ["fine"] (installed-names res)))
    (is (= {:addon-id :b :tool "banned" :reason :excluded :holder :guard}
           (refusal res "banned")))))

(deftest an-exclusion-with-no-provider-never-removes-a-core-tool
  (testing "core keeps the name; the addon providing it is still refused"
    (let [res (claims/resolve-claims
               [{:name "memory"}]
               [(contribution :guard [] :excluded ["memory"])
                (contribution :b ["memory"])])]
      (is (empty? (installed-names res)))
      (is (= {:addon-id :b :tool "memory" :reason :shadows-core :holder :core}
             (refusal res "memory"))
          "the refusal names CORE as the holder, not the excluding addon"))))

;; =============================================================================
;; Rule 1: claim
;; =============================================================================

(deftest a-claim-takes-the-name-over-a-core-tool
  (let [res (claims/resolve-claims
             [{:name "memory"}]
             [(contribution :a ["memory"] :excluded ["memory"])])]
    (is (= ["memory"] (installed-names res)))
    (is (= :a (:addon-source (first (:installed res)))))
    (is (= [{:addon-id :a :tool "memory" :how :declared :over :core}]
           (:claims res)))
    (is (empty? (:refused res)))))

(deftest a-claim-takes-the-name-over-an-earlier-addon
  (testing "even when the claimant registered second"
    (let [res (claims/resolve-claims
               []
               [(contribution :first ["shared"])
                (contribution :claimant ["shared"] :excluded ["shared"])])]
      (is (= :claimant (:addon-source (first (:installed res)))))
      (is (= [{:addon-id :claimant :tool "shared" :how :declared :over :addon}]
             (:claims res)))
      (is (= {:addon-id :first :tool "shared" :reason :excluded :holder :claimant}
             (refusal res "shared"))))))

(deftest two-claimants-contest-and-the-first-one-holds
  (let [res (claims/resolve-claims
             []
             [(contribution :a ["shared"] :excluded ["shared"])
              (contribution :b ["shared"] :excluded ["shared"])])]
    (is (= :a (:addon-source (first (:installed res)))))
    (is (= {:addon-id :b :tool "shared" :reason :contested-claim :holder :a}
           (refusal res "shared"))
        "the loser of a contest is distinguishable from a plain exclusion")))

;; =============================================================================
;; Shape and grouping
;; =============================================================================

(deftest every-contributed-tool-is-either-installed-or-refused
  (testing "no name is silently dropped"
    (let [contribs [(contribution :a ["one" "two"] :excluded ["three"])
                    (contribution :b ["two" "three"])
                    (contribution :c ["four"] :excluded ["four"])]
          res      (claims/resolve-claims [{:name "one"}] contribs)
          offered  (count (mapcat :tools contribs))]
      (is (= offered (+ (count (:installed res)) (count (:refused res))))))))

(deftest refusals-group-by-the-addon-that-lost
  (let [res (claims/resolve-claims
             [{:name "core-one"}]
             [(contribution :a ["core-one"])
              (contribution :b ["core-one" "kept"])])]
    (is (= #{:a :b} (set (keys (claims/refusals-by-addon res)))))
    (is (= ["kept"] (installed-names res)))))

(deftest claimed-core-names-are-exactly-the-ones-the-host-must-drop
  (testing "a claim over core is reported; a claim over another addon is not"
    (let [res (claims/resolve-claims
               [{:name "memory"}]
               [(contribution :a ["memory"] :excluded ["memory"])
                (contribution :b ["shared"])
                (contribution :c ["shared"] :excluded ["shared"])])]
      (is (= #{"memory"} (claims/claimed-core-names res))
          "\"shared\" was claimed over addon :b, not over a core tool")))

  (testing "a core tool the host keeps is never reported"
    (let [res (claims/resolve-claims
               [{:name "memory"}]
               [(contribution :a ["memory"])])]
      (is (empty? (claims/claimed-core-names res))
          "a plain shadow is refused, so core keeps the name")))

  (testing "the legacy consolidated supertool displaces core, so it counts"
    (let [res (claims/resolve-claims
               [{:name "code" :consolidated true}]
               [(contribution :x [(tool "code" {:consolidated true})]
                              :addon-type :native)])]
      (is (= #{"code"} (claims/claimed-core-names res)))))

  (testing "an empty resolution reports nothing"
    (is (empty? (claims/claimed-core-names (claims/resolve-claims [] []))))))

(deftest an-empty-world-resolves-to-an-empty-resolution
  (let [res (claims/resolve-claims [] [])]
    (is (= {:installed [] :refused [] :claims []} res))
    (is (claims/valid-resolution? res))))
