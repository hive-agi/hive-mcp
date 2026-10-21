(ns hive-mcp.engine.hprof.boot-test
  "Golden over the boot-time rotation and the disk-floor guard. Rotation runs
   against a temp storage dir; the abort port of enforce-disk-floor! is
   satisfied by a recording stub, never System/exit."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]
            [hive-mcp.engine.hprof.boot :as boot]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "hprof-boot-test" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- seed! [dir specs]
  (doseq [[n body mtime] specs]
    (let [f (io/file dir n)]
      (spit f body)
      (.setLastModified f mtime))))

(defn- run-rotation [overrides specs]
  (let [dir (temp-dir)]
    (try
      (seed! dir specs)
      (let [summary (boot/rotate! (assoc overrides :dir (.getPath dir)))]
        {:summary (dissoc summary :dir)
         :files   (vec (sort (.list dir)))})
      (finally (rm-rf dir)))))

(def ^:private four
  [["a.hprof" "aaaa" 1000000] ["b.hprof" "bbbbbbbb" 2000000]
   ["c.hprof.gz" "cc" 3000000] ["d.hprof" "dddddd" 4000000]])

(defn- floor-calls [overrides]
  (let [calls (atom [])]
    {:ret   (boot/enforce-disk-floor! overrides #(swap! calls conj %))
     :calls @calls}))

(defn- scenario []
  {:empty           (run-rotation {} [])
   :keep-2-gzip     (run-rotation {:keep-n 2 :gzip? true} four)
   :keep-2-raw      (run-rotation {:keep-n 2 :gzip? false} four)
   :keep-0          (run-rotation {:keep-n 0 :gzip? false} four)
   :floor-zero      (floor-calls {:dir (System/getProperty "java.io.tmpdir") :min-free-gb 0})
   :floor-absurd    (floor-calls {:dir (System/getProperty "java.io.tmpdir") :min-free-gb 1000000000})})

(deftest-golden-fn boot-rotation-golden
  "test/golden/engine/hprof-boot-rotation.edn"
  scenario)

(deftest boot-with-zero-floor-returns-nil
  (let [dir (temp-dir)]
    (try
      (is (nil? (boot/boot! {:dir (.getPath dir) :min-free-gb 0})))
      (finally (rm-rf dir)))))
