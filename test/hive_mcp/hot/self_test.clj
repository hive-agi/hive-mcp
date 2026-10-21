(ns hive-mcp.hot.self-test
  "The protocol interlock for hive-mcp's own hot reload.

   The set has to be DERIVED and it has to be derived WITHOUT dereferencing
   anything, so both properties get a test that fails if they regress."
  (:require [clojure.test :refer [deftest is testing]]
            [clojure.string]
            [hive-mcp.dispatch.handler]
            [hive-mcp.hot.self :as self]
            [hive-mcp.protocols.dispatch]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defprotocol ADefinedProtocol
  (a-method [this]))

(def ^:private forced?
  "Flipped if anything dereferences the delay below."
  (atom false))

(def a-delay-that-must-not-be-forced
  "A public var holding a delay, which is the trap the metadata approach
   avoids. Forcing it is observable, so a predicate that dereferences public
   vars to inspect a namespace cannot hide."
  (delay (reset! forced? true) :forced))

(deftest a-namespace-that-defines-a-protocol-is-detected
  (is (self/defines-protocol? 'hive-mcp.hot.self-test)
      "this namespace defines ADefinedProtocol, so it must be protected")
  (is (self/defines-protocol? 'hive-mcp.protocols.dispatch)
      "and so must a real core protocol namespace"))

(deftest a-namespace-with-no-protocol-is-not-detected
  (is (not (self/defines-protocol? 'hive-mcp.dispatch.handler))
      "a plain leaf must stay reloadable, or the interlock freezes the tree")
  (is (not (self/defines-protocol? 'hive-mcp.hot.self))
      "including this feature's own namespace"))

(deftest asking-the-question-never-dereferences-a-var
  (testing "a delay in a public var must not be forced by inspecting the ns"
    (reset! forced? false)
    (self/defines-protocol? 'hive-mcp.hot.self-test)
    (self/protocol-namespaces)
    (is (false? @forced?)
        "the predicate read :protocol off the var's METADATA; a deref-based one would have forced this delay, and on a var holding a promise it would have blocked the watcher instead")))

(deftest the-derived-set-is-non-empty-and-prefix-scoped
  (let [nss (self/protocol-namespaces)]
    (testing "vacuity guard: an empty set would disable the interlock silently"
      (is (pos? (count nss))))
    (testing "it finds the real core protocol namespaces"
      (is (contains? nss 'hive-mcp.protocols.dispatch)))
    (testing "and nothing outside the prefix it was asked for"
      (is (every? #(clojure.string/starts-with? (str %) "hive-mcp.") nss))
      (is (empty? (self/protocol-namespaces "no.such.prefix."))
          "an unmatched prefix yields nothing rather than everything"))))

(deftest the-set-is-derived-not-listed
  (testing "a namespace defining a protocol is picked up with no edit anywhere"
    (is (contains? (self/protocol-namespaces "hive-mcp.hot.") 'hive-mcp.hot.self-test)
        "this test namespace was never added to any list, and is found because it defines a protocol")))

;; ---------------------------------------------------------------------------
;; Trifectas: the predicate reads protocol METADATA off the publics, and the
;; derived set is exactly the prefix-scoped loaded namespaces that pass it.
;; ---------------------------------------------------------------------------

(def ^:private LoadedNs
  [:enum 'hive-mcp.hot.self-test 'hive-mcp.protocols.dispatch 'hive-mcp.dispatch.handler
   'hive-mcp.hot.self 'clojure.string 'clojure.core])

(defn- protocol-oracle
  "Independent reading: some public var carries :protocol metadata."
  [ns-sym]
  (boolean (some (comp :protocol meta) (vals (ns-publics ns-sym)))))

(deftrifecta-from-schema defines-protocol?-reads-var-metadata
  #'self/defines-protocol?
  {:in [:cat LoadedNs]
   :out :boolean
   :rel (fn [[ns-sym] out] (= out (protocol-oracle ns-sym)))
   :classify (fn [_ out] out)
   :classify-domain #{true false}
   ;; distinct from contract-test's self-defines-protocol.edn (other inputs)
   :golden-path "test/golden/hive-mcp/hot/self-defines-protocol-loaded.edn"
   :num-tests 100})

(deftrifecta-from-schema protocol-namespaces-is-the-filtered-loaded-set
  #'self/protocol-namespaces
  {:in [:cat [:enum "hive-mcp.protocols" "hive-mcp.hot." "hive-mcp.dispatch." "no.such.prefix."]]
   :out [:set :symbol]
   :rel (fn [[prefix] out]
          (= out (into #{} (comp (map ns-name)
                                 (filter #(clojure.string/starts-with? (str %) prefix))
                                 (filter protocol-oracle))
                       (all-ns))))
   :classify (fn [_ out] (if (empty? out) :empty :found))
   :classify-domain #{:empty :found}
   :num-tests 40})
