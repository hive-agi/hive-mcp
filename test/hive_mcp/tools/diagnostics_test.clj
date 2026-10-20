(ns hive-mcp.tools.diagnostics-test
  "Shapes of a protocol \"No implementation of method\" failure, on a
   stand-in protocol: a missing method key (the adapter extended under the
   wrong key), a class that never extended the protocol, a stale same-named
   class (real dispatch poisoning), and a healthy dispatch. Only the
   stale-class shape may advise a restart."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.diagnostics :as diag]))

(defprotocol StandInStore
  (-create-collection [this n]))

(deftype MisKeyedStore [])

;; The hive-milvus defect: extended under :create-collection, while the
;; protocol's method key is :-create-collection.
(extend MisKeyedStore
  StandInStore
  {:create-collection (fn [_ n] n)})

(deftype HealthyStore [])

(extend HealthyStore
  StandInStore
  {:-create-collection (fn [_ n] n)})

(defn- dispatch-error
  "The message protocol dispatch throws for `x`, or nil when it dispatches."
  [f x]
  (try (f x) nil
       (catch IllegalArgumentException e (.getMessage e))))

(deftest a-missing-method-key-is-not-called-poisoning
  (let [msg       (dispatch-error #(-create-collection % "c") (->MisKeyedStore))
        diagnosis (diag/detect-protocol-poisoning msg)
        hint      (diag/recovery-hint msg)]
    (is (some? msg) "the mis-keyed extend leaves the method unimplemented")
    (is (= :missing-method (:diagnosis diagnosis)))
    (is (false? (:poisoned? diagnosis)))
    (is (not (str/includes? hint "poisoned")))
    (is (not (re-find #"(?i)restart" hint)))
    (testing "the hint names the method and the keys the adapter did extend"
      (is (str/includes? hint "does not implement"))
      (is (str/includes? hint ":-create-collection"))
      (is (str/includes? hint "[:create-collection]")))))

(deftest a-class-that-never-extended-the-protocol-is-not-poisoning
  (let [msg       (dispatch-error #(-create-collection % "c") (Object.))
        diagnosis (diag/detect-protocol-poisoning msg)]
    (is (= :not-extended (:diagnosis diagnosis)))
    (is (false? (:poisoned? diagnosis)))
    (is (not (re-find #"(?i)restart" (diag/recovery-hint msg))))))

(deftest a-stale-same-named-class-is-poisoning
  (let [ns-sym    'hive-mcp.tools.diagnostics-test.stale
        ev        (fn [form] (binding [*ns* (create-ns ns-sym)]
                               (eval '(clojure.core/refer-clojure))
                               (eval form)))
        define!   (fn []
                    (ev '(defprotocol StaleP (stale-m [this])))
                    (ev '(deftype StaleT [] StaleP (stale-m [_] :ok))))
        _         (define!)
        old       (ev '(StaleT.))
        ;; :reload-all of protocol and type: a NEW protocol interface and a
        ;; NEW Class object under the same name. The old instance still
        ;; implements the old interface, which nothing dispatches on.
        _         (define!)
        f         @(ns-resolve ns-sym 'stale-m)
        fresh     (ev '(StaleT.))
        msg       (dispatch-error f old)
        diagnosis (diag/detect-protocol-poisoning msg)
        hint      (diag/recovery-hint msg)]
    (try
      (is (= :ok (f fresh)) "the fresh instance dispatches")
      (is (some? msg) "the old instance cannot dispatch")
      (is (= :stale-class (:diagnosis diagnosis)))
      (is (true? (:poisoned? diagnosis)))
      (is (str/includes? hint "poisoned"))
      (is (re-find #"(?i)restart" hint))
      (finally (remove-ns ns-sym)))))

(deftest a-healthy-dispatch-carries-no-hint
  (is (nil? (dispatch-error #(-create-collection % "c") (->HealthyStore))))
  (is (= "c" (-create-collection (->HealthyStore) "c")))
  (is (nil? (diag/recovery-hint "Connection refused")))
  (is (nil? (diag/detect-protocol-poisoning "some other failure"))))

(deftest an-unresolvable-protocol-does-not-assert-poisoning
  (let [msg (str "No implementation of method: :m of protocol: "
                 "#'no.such.ns/Proto found for class: no.such.Klass")
        diagnosis (diag/detect-protocol-poisoning msg)]
    (is (= :unknown (:diagnosis diagnosis)))
    (is (false? (:poisoned? diagnosis)))
    (is (not (str/includes? (diag/recovery-hint msg) "poisoned (server restart required)")))))

(deftest the-hint-cites-no-deleted-memory
  (let [msg (dispatch-error #(-create-collection % "c") (->MisKeyedStore))]
    (is (not (str/includes? (diag/recovery-hint msg) "20260429195405-2425b388")))))
