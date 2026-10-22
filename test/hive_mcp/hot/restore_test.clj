(ns hive-mcp.hot.restore-test
  "A failed core reload pass must not leave the image without the namespaces
   it unloaded: restore! loads them back, and reload! runs it on failure."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.hot.core :as core]
            [hive-mcp.hot.restore :as restore]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def fixture 'hive-mcp.hot.restore-fixture)

(defn- unload! [ns-sym]
  (remove-ns ns-sym)
  (dosync (alter @#'clojure.core/*loaded-libs* disj ns-sym)))

(deftest absent-is-the-unloaded-namespaces-the-image-lacks-in-load-order
  (require fixture)
  (is (= '[no.such.b no.such.a]
         (restore/absent ["no.such.a" 'hive-mcp.hot.restore-fixture "no.such.b"]))))

(deftest restore-loads-back-what-a-pass-left-unloaded
  (require fixture)
  (testing "absent from the image AND from *loaded-libs*, as clj-reload leaves it"
    (unload! fixture)
    (is (= {:restored ["hive-mcp.hot.restore-fixture"] :still-unloaded []}
           (restore/restore! [fixture])))
    (is (= 1 @(resolve 'hive-mcp.hot.restore-fixture/value))))
  (testing "absent from the image but still listed in *loaded-libs*"
    (remove-ns fixture)
    (is (= ["hive-mcp.hot.restore-fixture"] (:restored (restore/restore! [fixture]))))
    (is (some? (find-ns fixture)))))

(deftest a-namespace-that-cannot-load-is-reported-not-thrown
  (let [{:keys [restored still-unloaded]} (restore/restore! ["hive-mcp.hot.no-such-namespace"])]
    (is (empty? restored))
    (is (= ["hive-mcp.hot.no-such-namespace"] (mapv :ns still-unloaded)))
    (is (string? (:error (first still-unloaded))))))

(defn- ports [reload-result seen]
  {:hot/ensure-init!    (fn [_] {:initialized? true})
   :hot/status          (fn [] {:dirs ["/r/src"]})
   :hot/scope-plan      (fn [_] {:want ["/r/src/hive_mcp/a.clj"] :cascade []})
   :hot/prepare-pass!   (fn [& _] {})
   :hot/reload-scoped!  (fn [_] reload-result)
   :host/restore!       (fn [unloaded] (swap! seen conj unloaded) {:restored ["x"] :still-unloaded []})})

(deftest reload-restores-only-after-a-failed-pass
  (testing "failed pass: the unloaded namespaces are handed to :host/restore!"
    (let [seen (atom [])
          r    (core/reload! {:roots ["/r/src"]
                              :ports (ports {:success false :failed 'hive-mcp.a
                                             :loaded [] :unloaded ['hive-mcp.a 'hive-mcp.b]}
                                            seen)})]
      (is (false? (:ok? r)))
      (is (= [['hive-mcp.a 'hive-mcp.b]] @seen))
      (is (= {:restored ["x"] :still-unloaded []} (:restored r)))))
  (testing "successful pass: nothing to restore"
    (let [seen (atom [])
          r    (core/reload! {:roots ["/r/src"]
                              :ports (ports {:success true :loaded [] :unloaded []} seen)})]
      (is (true? (:ok? r)))
      (is (empty? @seen))
      (is (nil? (:restored r))))))
