(ns hive-mcp.kernel.census-contract-test
  "Contract facets for hive-mcp.kernel.census, and the coverage gate for
   src/hive_mcp/kernel.

   These facets check the census against its own contracts. They never touch
   the kernel allowlist or loosen a gate: the allowlist is read only by the
   golden below, and the gate tests (census-gate-test, frozen-extraction-test)
   stay the authority over what the tree may contain."
  (:require [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.kernel.census :as c]
            [hive-schemas.coverage :refer [deftest-contract-coverage]]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]
            [malli.generator :as mg]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest-contract-coverage kernel-contracts-gate
  {:paths ["src/hive_mcp/kernel"]
   :exempt {}})

(deftest every-kernel-contract-generates-both-sides
  (doseq [[fn-sym {:keys [schema]}] (get (m/function-schemas) 'hive-mcp.kernel.census)
          arity (let [s (m/schema schema)] (if (= :function (m/type s)) (m/children s) [s]))
          side (take 2 (m/children arity))]
    (is (= 3 (count (mg/sample side {:size 3 :seed 7})))
        (str fn-sym " " (pr-str (m/form side))))))

;; ---------------------------------------------------------------------------
;; Pure classification

(defn- rel-name [n]
  (let [s (str n)]
    (when (.startsWith s "hive-mcp.") (subs s (count "hive-mcp.")))))

(deftrifecta-from-schema relative-name-strips-the-root
  #'c/relative-name
  {:in [:cat c/NsName]
   :out [:maybe :string]
   :rel (fn [[n] out] (= out (rel-name n)))
   :golden-path "test/golden/hive-mcp/kernel/census-relative-name.edn"})

(deftrifecta-from-schema segment-is-the-first-dotted-part
  #'c/segment
  {:in [:cat c/NsName]
   :out [:maybe :string]
   :rel (fn [[n] out]
          (= out (some-> (rel-name n) (clojure.string/split #"\.") first)))
   :golden-path "test/golden/hive-mcp/kernel/census-segment.edn"})

(deftrifecta-from-schema classify-agrees-with-the-allowlist
  #'c/classify
  {:in [:cat c/Allowlist c/NsName]
   :out c/Classification
   :rel (fn [[al n] {:keys [class target]}]
          (case class
            :foreign (nil? (rel-name n))
            :kernel (some? (rel-name n))
            :unknown (some? (rel-name n))
            :extract (contains? (set (vals (:extraction al))) target)))
   :classify (fn [_ out] (:class out))
   :classify-domain c/ClassKw
   :classify-floor 10
   :num-tests 600
   :golden-path "test/golden/hive-mcp/kernel/census-classify.edn"})

(deftrifecta-from-schema classified-merges-every-row
  #'c/classified
  {:in [:cat c/Allowlist [:sequential c/RowIn]]
   :out [:vector [:map [:ns c/NsName] [:class c/ClassKw]]]
   :rel (fn [[al rows] out]
          (and (= (count rows) (count out))
               (every? true? (map (fn [r o] (= (c/classify al (:ns r)) (select-keys o [:class :target])))
                                  rows out))))})

(deftrifecta-from-schema by-class-partitions-the-rows
  #'c/by-class
  {:in [:cat [:sequential [:map [:class c/ClassKw] [:ns c/NsName]]]]
   :out [:map-of c/ClassKw [:vector c/NsName]]
   :rel (fn [[rows] out]
          (= (frequencies (map :class rows))
             (update-vals out count)))})

;; ---------------------------------------------------------------------------
;; Edges and waivers

(deftrifecta-from-schema kernel-edges-leave-from-kernel-namespaces
  #'c/kernel-edges
  {:in [:cat c/Allowlist [:sequential c/RowIn]]
   :out [:set c/Edge]
   :rel (fn [[al rows] out]
          (every? (fn [{:keys [from to]}]
                    (and (= :kernel (:class (c/classify al from)))
                         (some #(and (= from (:ns %)) (some #{to} (:requires %))) rows)))
                  out))
   :num-tests 150})

(deftrifecta-from-schema edge-key-reads-symbols
  #'c/edge-key
  {:in [:cat c/EdgeRef]
   :out [:map {:closed true} [:from :symbol] [:to :symbol]]
   :rel (fn [[e] out] (and (= (str (:from e)) (str (:from out)))
                           (= (str (:to e)) (str (:to out)))))
   :golden-path "test/golden/hive-mcp/kernel/census-edge-key.edn"})

(deftrifecta-from-schema uncovered-edges-are-the-unwaived-ones
  #'c/uncovered-edges
  {:in [:cat c/EdgeRefs c/EdgeRefs]
   :out [:sequential c/EdgeRef]
   :rel (fn [[waivers edges] out]
          (let [w (set (map c/edge-key waivers))]
            (= (set out) (set (remove #(w (c/edge-key %)) edges)))))})

(deftrifecta-from-schema stale-waivers-match-no-edge
  #'c/stale-waivers
  {:in [:cat c/EdgeRefs c/EdgeRefs]
   :out [:sequential c/EdgeRef]
   :rel (fn [[waivers edges] out]
          (let [e (set (map c/edge-key edges))]
            (= (set out) (set (remove #(e (c/edge-key %)) waivers)))))})

;; ---------------------------------------------------------------------------
;; Frozen extractions

(deftrifecta-from-schema listed-extractions-are-symbol-sets
  #'c/listed-extractions
  {:in [:cat c/Allowlist]
   :out [:map-of :keyword [:set :symbol]]
   :rel (fn [[al] out] (= (set (keys out)) (set (keys (:extraction/listed al)))))})

(deftrifecta-from-schema unlisted-extractions-are-frozen-and-unlisted
  #'c/unlisted-extractions
  {:in [:cat c/Allowlist [:sequential c/RowIn]]
   :out [:vector c/OwnedNs]
   :rel (fn [[al _] out]
          (let [listed (c/listed-extractions al)]
            (every? (fn [{:keys [ns target]}]
                      (and (contains? listed target)
                           (not (contains? (get listed target) ns))))
                    out)))
   :num-tests 150})

(deftrifecta-from-schema stale-listings-are-listed
  #'c/stale-listings
  {:in [:cat c/Allowlist [:sequential c/RowIn]]
   :out [:vector c/OwnedNs]
   :rel (fn [[al _] out]
          (let [listed (c/listed-extractions al)]
            (every? (fn [{:keys [ns target]}] (contains? (get listed target) ns)) out)))})

(deftrifecta-from-schema segment-matrix-counts-cross-segment-requires
  #'c/segment-matrix
  {:in [:cat [:sequential c/RowIn]]
   :out [:map-of [:maybe :string] [:map-of :string pos-int?]]
   :rel (fn [[rows] out]
          (= (reduce + 0 (mapcat vals (vals out)))
             (count (for [{:keys [ns requires]} rows
                          req requires
                          :let [ts (c/segment req)]
                          :when (and ts (not= ts (c/segment ns)))]
                      req))))})

(deftrifecta-from-schema class-matrix-counts-every-edge
  #'c/class-matrix
  {:in [:cat [:sequential c/Edge]]
   :out [:map-of [:maybe :string] [:map-of :keyword pos-int?]]
   :rel (fn [[edges] out] (= (count edges) (reduce + 0 (mapcat vals (vals out)))))})

;; ---------------------------------------------------------------------------
;; Reading (the only IO): ns forms and the allowlist resource

(deftrifecta-from-schema required-namespaces-reads-every-libspec
  #'c/required-namespaces
  {:in [:cat [:maybe c/NsForm]]
   :out [:vector :symbol]
   :rel (fn [[form] out] (= (count out) (count (distinct out))))
   :golden-path "test/golden/hive-mcp/kernel/census-required-namespaces.edn"})

(deftrifecta-from-schema ns-form-reads-the-first-ns
  #'c/ns-form
  {:in [:cat c/SourceFile]
   :out [:maybe c/NsForm]
   :rel (fn [[f] out]
          (= (second out)
             (symbol (-> (.getPath ^java.io.File f)
                         (subs (count "src/"))
                         (clojure.string/replace #"\.clj$" "")
                         (clojure.string/replace "/" ".")
                         (clojure.string/replace "_" "-")))))
   :num-tests 20})

(deftrifecta-from-schema source-files-are-sorted-sources
  #'c/source-files
  {:in [:cat c/SourceRoot]
   :out [:sequential c/SourceFile]
   :rel (fn [_ out]
          (and (every? #(re-find #"\.cljc?$" (.getName ^java.io.File %)) out)
               (= (map #(.getPath ^java.io.File %) out)
                  (sort (map #(.getPath ^java.io.File %) out)))))
   :num-tests 20})

(deftrifecta-from-schema census-has-one-row-per-ns-file
  #'c/census
  {:in [:cat c/SourceRoot]
   :out [:vector c/CensusRow]
   :rel (fn [[root] out] (<= (count out) (count (c/source-files root))))
   :num-tests 20})

(golden/deftest-golden census-of-the-kernel-dir-golden
  "test/golden/hive-mcp/kernel/census-kernel-dir.edn"
  (mapv #(update % :requires sort) (c/census "src/hive_mcp/kernel")))

(deftest unreadable-inputs-read-as-nothing
  (testing "a file that does not exist has no ns form"
    (is (nil? (c/ns-form (io/file "no/such/file.clj")))))
  (testing "a root that does not exist has no sources"
    (is (empty? (c/source-files "no/such/root")))
    (is (= [] (c/census "no/such/root")))))

(deftest the-allowlist-reads-against-its-schema
  (let [al (c/load-allowlist)]
    (is (m/validate c/Allowlist al) (pr-str (m/explain c/Allowlist al)))
    (testing "the frozen owner keeps its namespaces listed"
      (is (contains? (c/listed-extractions al) :hive-agent))
      (is (set/subset? '#{hive-mcp.hivemind.state hive-mcp.hivemind.core}
                       (get (c/listed-extractions al) :hive-agent))))))

(deftest classification-arms-validate-only-their-own-arm
  (let [extract {:class :extract :target :hive-agent}
        kernel {:class :kernel}]
    (is (m/validate c/Classification extract))
    (is (m/validate c/Classification kernel))
    (is (not (m/validate c/Classification {:class :extract})) "an extract names its target")
    (is (not (m/validate c/Classification {:class :kernel :target :hive-agent}))
        "only an extract carries a target")))
