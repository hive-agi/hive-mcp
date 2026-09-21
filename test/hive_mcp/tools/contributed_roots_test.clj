;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(ns hive-mcp.tools.contributed-roots-test
  "The tool surface is assembled from what the domains contributed, so three
   things must hold: the in-core manifest names only things this build has,
   the roots it contributes keep MANIFEST ORDER (a tool array that reshuffles
   between boots costs the caller its whole cached prompt prefix), and a
   domain that is not in the build simply has no root rather than throwing.

   The last case is bound through `soft/*resolve*`, which is the classpath an
   extracted domain leaves behind (HIVE-KERNEL E1/E3/E4/E5/E6/E7)."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.spi.contributions :as contrib]
            [hive-mcp.swarm.adapters.soft :as soft]
            [hive-mcp.tools.registry :as registry]))

(def ^:private manifest-opts
  {:resolve-keys #{:load :tool-defs} :required-key :load})

(defn- reload-real-manifest! []
  (contrib/reset-kind! :tools)
  (contrib/load-manifest! registry/tool-manifest-resource manifest-opts))

(defn- with-restored-registry [f]
  (try (f) (finally (reload-real-manifest!))))

(use-fixtures :each with-restored-registry)

(deftest the-manifest-names-only-things-this-build-has
  (let [{:keys [contributed absent]} (:tools (reload-real-manifest!))]
    (is (empty? absent)
        (str "the manifest names domains this build does not have: " absent))
    (is (seq contributed))))

(deftest contributed-roots-keep-manifest-order
  (reload-real-manifest!)
  (let [manifest-order (->> (get (contrib/read-manifest registry/tool-manifest-resource) :tools)
                            (filter :tool-defs)
                            (mapv :key))
        contributed    (->> (contrib/ordered :tools)
                            (filter (comp :tool-defs val))
                            (mapv first))]
    (is (= manifest-order contributed)
        "contribution order is the manifest's, not the registry map's hash order")
    (testing "and re-contributing a domain does not move it"
      (let [before (mapv first (contrib/ordered :tools))
            entry  (contrib/entry :tools (first before))]
        (contrib/contribute! :tools (first before) entry)
        (is (= before (mapv first (contrib/ordered :tools))))))))

(deftest every-contributed-root-reaches-the-tool-surface
  (reload-real-manifest!)
  (let [root-names (mapv :name (registry/contributed-roots))
        all-names  (set (map :name (registry/get-all-tools)))]
    (is (seq root-names))
    (doseq [n root-names]
      (testing n
        (is (contains? all-names n))))))

(deftest a-domain-that-is-not-in-the-build-contributes-no-root
  (contrib/reset-kind! :tools)
  (binding [soft/*resolve* (constantly nil)]
    (let [{:keys [contributed absent]} (:tools (contrib/load-manifest!
                                                registry/tool-manifest-resource
                                                manifest-opts))]
      (is (empty? contributed))
      (is (seq absent))))
  (is (= [] (registry/contributed-roots))
      "no root, no throw: the surface shrinks to what is present")
  (is (seq (registry/get-all-tools))
      "the kernel's own roots are unaffected"))
