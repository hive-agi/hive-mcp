(ns hive-mcp.crystal.wrap-handler-test
  "Golden over the native multi-scope wrap handler. The IMemoryStore port is
   satisfied by a recording stub registered through the store registry (no
   with-redefs); the harvest runs against an empty temp directory, so the
   session has nothing to fan out. Arms: no store registered (the kernel's
   store-not-configured error) and a registered store with an empty harvest.
   The :session value is today's date, so the golden records its presence."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is]]
            [hive-mcp.crystal.wrap-handler :as wrap]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "wrap-handler" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- with-store [store f]
  (let [before (mem-proto/registered-stores)]
    (mem-proto/reset-registry!)
    (try
      (when store (mem-proto/register-store! :default store))
      (f)
      (finally
        (mem-proto/reset-registry!)
        (doseq [[k s] before] (mem-proto/register-store! k s))))))

(defn- project [dir {:keys [type text isError]}]
  (let [body (json/read-str text :key-fn keyword)]
    (cond-> {:type type :isError (boolean isError)
             :body (cond-> body
                     (contains? body :session)   (update :session string?)
                     (contains? body :directory) (update :directory #(= % dir)))})))

(defn- scenario []
  (let [dir   (temp-dir)
        path  (.getPath dir)
        calls (atom [])
        stub  (reify mem-proto/IMemoryStore
                (add-entry! [_ e] (swap! calls conj e) (str "id-" (count @calls))))
        args  {:directory path :agent_id "ling-f08"}]
    (try
      {:no-store      (project path (with-store nil #(wrap/handle-native-wrap args)))
       :empty-harvest (project path (with-store stub #(wrap/handle-native-wrap args)))
       :writes        (count @calls)}
      (finally (.delete dir)))))

(deftest-golden-fn native-wrap-golden
  "test/golden/crystal/wrap-handler-native-wrap.edn"
  scenario)

(deftest handler-never-throws-on-nil-args
  (is (map? (with-store nil #(wrap/handle-native-wrap nil)))))
