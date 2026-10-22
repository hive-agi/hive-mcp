(ns hive-mcp.server.config-reload-test
  "config reload re-runs the boot steps its config change affects, through
   ports. Every port here is a recording stub: nothing touches the live
   embedder, store registry or addon registry."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.server.config-reload :as sut]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def broken
  "The shape of the 2026-10-22 clobbered config: defaults only."
  {:memory   {:default-store :chroma}
   :services {:datahike {:path "data/kg"}}})

(def restored
  "The operator's config after the file was put back."
  {:memory     {:default-store :milvus
                :stores {:milvus {:addon :hive-milvus}}}
   :embeddings {:ollama {:host "http://localhost:11434" :model "nomic-embed-text"}}
   :services   {:datahike {:path "data/kg"}
                :memory-store {:backend "milvus"}
                :seal {:layers {:work true}}}})

(defn- recording-ports
  "Ports that record the order they ran in. `answers` overrides a step's
   return value (a fn of no args)."
  ([calls] (recording-ports calls {}))
  ([calls answers]
   (into {}
         (map (fn [step]
                [step (fn []
                        (swap! calls conj step)
                        (if-let [a (get answers step)] (a) true))]))
         [:embeddings :memory-store :redecorate])))

(deftest plan-is-empty-for-an-unchanged-config
  (is (= [] (sut/plan restored restored)))
  (is (= [] (sut/plan restored (assoc restored :project-roots ["/x"])))
      "a key no boot step reads re-wires nothing"))

(deftest clobber-recovery-reruns-every-affected-step-in-boot-order
  (let [calls  (atom [])
        report (sut/rewire! broken restored (recording-ports calls))]
    (is (= [:embeddings :memory-store :redecorate] @calls)
        "embedder, then store wiring, then the decorator chains")
    (is (= [:embeddings :memory-store :redecorate] (:ran report)))
    (is (= {} (:failed report)))
    (is (some #{"[:memory]"} (:changed report)))))

(deftest a-seal-change-alone-only-redecorates
  (let [calls (atom [])]
    (sut/rewire! restored (assoc-in restored [:services :seal :layers :work] false)
                 (recording-ports calls))
    (is (= [:redecorate] @calls))))

(deftest a-store-change-always-redecorates
  (testing "a store rewired anew leaves the other slots' chains to rebuild"
    (let [calls (atom [])]
      (sut/rewire! restored (assoc-in restored [:services :milvus :host] "milvus.lan")
                   (recording-ports calls))
      (is (= [:memory-store :redecorate] @calls)))))

(deftest an-embedder-change-alone-only-reinits-embeddings
  (let [calls (atom [])]
    (sut/rewire! restored (assoc-in restored [:embeddings :ollama :model] "bge-m3")
                 (recording-ports calls))
    (is (= [:embeddings] @calls))))

(deftest a-failing-step-is-reported-and-does-not-stop-the-rest
  (let [calls  (atom [])
        report (sut/rewire! broken restored
                            (recording-ports calls {:embeddings (constantly false)
                                                    :memory-store #(throw (ex-info "milvus down" {}))}))]
    (is (= [:embeddings :memory-store :redecorate] @calls)
        "every step is attempted; the one after the throw still runs")
    (is (= [:redecorate] (:ran report)))
    (is (= #{:embeddings :memory-store} (set (keys (:failed report)))))
    (is (re-find #"milvus down" (get-in report [:failed :memory-store])))))

(deftest the-report-never-carries-config-values
  (let [secret "sk-or-very-secret"
        report (sut/rewire! {} {:secrets {:openrouter-api-key secret}}
                            (recording-ports (atom [])))]
    (is (= [:embeddings] (:ran report)))
    (is (not (re-find #"very-secret" (pr-str report))))))

(deftest reload-diffs-the-config-before-and-after-the-load
  (let [calls  (atom [])
        cfg    (atom broken)
        out    (sut/reload! {:current #(deref cfg)
                             :load!   #(reset! cfg restored)
                             :ports   (recording-ports calls)})]
    (is (= restored (:loaded out)))
    (is (= [:embeddings :memory-store :redecorate] @calls))
    (is (= [:embeddings :memory-store :redecorate] (get-in out [:rewire :ran])))))

(deftest default-ports-name-real-boot-steps
  (doseq [[step sym] sut/default-ports]
    (is (some? (requiring-resolve sym)) (str step " -> " sym))))
