(ns hive-mcp.addons.placement-test
  "Placement vocabulary, resolver and declaration gate
   (kanban 20260920154039-002c8d03). The gate's universe is the manifests on
   disk: every META-INF/hive-addons/*.edn on the classpath, plus every sibling
   repo's resources/META-INF/hive-addons when HIVE_WORKSPACE (or the
   hive.workspace property) names the workspace root. It is never the waiver
   list it is checked against."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.addons.manifest :as manifest]
            [hive-mcp.addons.placement :as placement]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private KnownPlacement
  [:map {:closed true}
   [:requires [:set (into [:enum] (keys placement/reasons))]]
   [:prefers {:optional true} placement/Side]])

(defn resolve-on-split
  "resolve-side over the split topology, as a one-argument trifecta subject."
  [p]
  (placement/resolve-side placement/split p))

(deftrifecta-from-schema resolve-side-split
  hive-mcp.addons.placement-test/resolve-on-split
  {:in  KnownPlacement
   :out placement/Resolution
   :rel (fn [{:keys [requires prefers]} out]
          (let [sides (set (map (:reasons placement/split) requires))]
            (cond
              (> (count sides) 1) (= :conflicting-reasons (:error out))
              (seq sides)         (= {:side (first sides) :why :required} out)
              prefers             (= {:side prefers :why :preferred} out)
              :else               (= {:side :cluster :why :default} out))))
   :num-tests 200})

(deftest resolver-cases
  (is (= {:side :local :why :required}
         (placement/resolve-side placement/split {:requires #{:display} :prefers :cluster})))
  (is (= {:side :cluster :why :required}
         (placement/resolve-side placement/split {:requires #{:gpu :bulk-store}})))
  (is (= {:error :conflicting-reasons :reasons #{:display :gpu}}
         (placement/resolve-side placement/split {:requires #{:display :gpu}})))
  (is (= {:error :unknown-reason :reasons #{:quantum}}
         (placement/resolve-side placement/split {:requires #{:quantum}})))
  (testing "the workstation topology places every known reason locally"
    (doseq [r (keys placement/reasons)]
      (is (= :local (:side (placement/resolve-side placement/workstation {:requires #{r}})))))))

(deftest manifest-schema-accepts-placement-and-rejects-a-side-literal
  (let [base {:addon/id "x" :addon/type :native :addon/init-ns "a.b" :addon/init-fn "c"}]
    (is (manifest/valid-manifest? (assoc base :addon/placement {:requires #{:display}})))
    (is (manifest/valid-manifest? base) "placement stays optional in the schema; the gate enforces it")
    (is (not (manifest/valid-manifest? (assoc base :addon/placement :local))))))

(deftest gap-and-waiver-arithmetic
  (let [mfs [{:addon/id "a"}
             {:addon/id "b" :addon/placement {:requires #{}}}
             {:addon/id "c" :addon/placement :local}]
        gaps (placement/placement-gaps mfs)]
    (is (= [{:addon/id "a" :gap :undeclared} {:addon/id "c" :gap :invalid}] gaps))
    (is (= [{:addon/id "c" :gap :invalid}] (placement/unwaived-gaps #{"a" "c"} gaps))
        "an invalid declaration is never waived")
    (is (= ["b"] (placement/stale-waivers #{"a" "b"} mfs)))))

;; --- the gate over manifests on disk ----------------------------------------

(defn- waivers []
  (edn/read-string (slurp (io/resource "hive-mcp/placement-waivers.edn"))))

(defn- workspace-root []
  (some-> (or (System/getProperty "hive.workspace") (System/getenv "HIVE_WORKSPACE"))
          io/file
          (#(when (.isDirectory ^java.io.File %) %))))

(defn- workspace-manifests [root]
  (for [repo  (sort (.listFiles ^java.io.File root))
        :let  [dir (io/file repo "resources" "META-INF" "hive-addons")]
        :when (.isDirectory dir)
        f     (sort (.listFiles dir))
        :when (.endsWith (.getName ^java.io.File f) ".edn")
        :let  [m (try (edn/read-string (slurp f)) (catch Exception _ nil))]
        :when (string? (:addon/id m))]
    m))

(defn- disk-manifests []
  (let [cp (:manifests (manifest/scan-classpath-manifests))
        ws (some-> (workspace-root) workspace-manifests)]
    (vals (into {} (map (juxt :addon/id identity)) (concat cp ws)))))

(deftest every-addon-on-disk-declares-placement-or-is-waived
  (let [{:keys [waived baseline]} (waivers)
        mfs (disk-manifests)]
    (println "placement gate:" (count mfs) "manifests on disk,"
             (count waived) "waived of baseline" baseline
             (if (workspace-root) "(workspace scanned)" "(classpath only; set HIVE_WORKSPACE for the fleet)"))
    (testing "every undeclared addon is waived, and no invalid declaration exists"
      (let [open (placement/unwaived-gaps waived (placement/placement-gaps mfs))]
        (is (empty? open) (str "declare :addon/placement {:requires #{...}} in: " (pr-str open)))))
    (testing "a waiver for an addon that now declares is dropped"
      (is (empty? (placement/stale-waivers waived mfs))))
    (testing "the waiver list ratchets down"
      (is (<= (count waived) baseline)))))
