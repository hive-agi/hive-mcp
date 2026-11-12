(ns hive-mcp.protocols.dispatch-spi-facade-test
  "hive-mcp.protocols.dispatch is a facade over hive-spi.swarm.dispatch-context:
   ONE protocol, so a context built on either side satisfies the other. Two
   defprotocols of the same name split satisfies? (the event-backbone defect,
   kanban 20260919175501-48925ef1)."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.protocols.dispatch :as dispatch]
            [hive-spi.swarm.dispatch-context :as spi]))

(deftest the-facade-protocol-is-the-spi-protocol
  (is (identical? spi/IDispatchContext dispatch/IDispatchContext)))

(deftest facade-contexts-satisfy-the-spi-protocol
  (testing "every facade constructor answers a hive-spi IDispatchContext"
    (doseq [ctx [(dispatch/->text-context "t")
                 (dispatch/->ref-context "r" {})
                 (dispatch/->ref-context "r" {:reconstruct-fn (constantly "x")})
                 (dispatch/ensure-context "s")
                 (dispatch/->graph-context "node-1" nil)]]
      (is (satisfies? spi/IDispatchContext ctx))
      (is (string? (:prompt (spi/resolve-context ctx)))))))

(deftest spi-contexts-satisfy-the-facade-protocol
  (let [ctx (spi/->text-context "from-spi")]
    (is (satisfies? dispatch/IDispatchContext ctx))
    (is (identical? ctx (dispatch/ensure-context ctx)))
    (is (= {:prompt "from-spi"} (dispatch/resolve-context ctx)))
    (is (= :text (dispatch/context-type ctx)))))

(deftest the-records-are-the-spi-records
  (is (instance? hive_spi.swarm.dispatch_context.TextContext
                 (dispatch/->text-context "t")))
  (is (instance? hive_spi.swarm.dispatch_context.RefContext
                 (dispatch/->ref-context "r" {}))))

(deftest the-host-ref-context-injects-the-host-reconstruction
  (testing "with no :reconstruct-fn the host default is injected, never nil"
    (is (fn? (:reconstruct-fn (dispatch/->ref-context "r" {})))))
  (testing "an explicit :reconstruct-fn is kept"
    (let [f (constantly "explicit")]
      (is (identical? f (:reconstruct-fn (dispatch/->ref-context "r" {:reconstruct-fn f})))))))
