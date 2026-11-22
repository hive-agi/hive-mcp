(ns hive-mcp.await.fire-isolation-test
  "`await fire` isolation (friction 20261122150418-7fef4903): one
   coordinator's fire (no awaiter id, data.to=pv01-sync) woke every manual
   await with a nil filter, including other coordinators' lings, and handed
   each one a payload meant for someone else.

   Contract now:
     - a manual awaiter whose owner names a ling gets the filter
       {:to <ling-id>} at create time when the caller passes none;
     - a fire that names no awaiter is offered only to FILTERED awaiters;
     - an awaiter with no filter hears only a fire that names its id;
     - a fire that names an id is addressed to it: the target's filter :to
       is filled in when the data names none."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.await.ports :as ports]
            [hive-mcp.await.registry :as registry]
            [hive-mcp.await.sources.manual :as manual]
            [hive-mcp.tools.consolidated.await :as await-tool]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private woken (atom []))

(defmethod ports/open-resumer :iso-stub
  [_owner]
  (fn [req] (swap! woken conj (get-in req [:awaiter :id])) :resumed))

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(defn- wait-until [ms pred]
  (let [end (+ (System/currentTimeMillis) ms)]
    (loop []
      (let [v (pred)]
        (if (or v (> (System/currentTimeMillis) end)) v (do (Thread/sleep 10) (recur)))))))

(defn- with-temp-registry [f]
  (let [d   (.toFile (java.nio.file.Files/createTempDirectory
                      "await-iso" (make-array java.nio.file.attribute.FileAttribute 0)))
        reg (registry/make-registry (str (io/file d "registry.edn")))]
    (registry/init! reg)
    (reset! woken [])
    (try (with-redefs [await-tool/current-registry (constantly reg)] (f reg))
         (finally (registry/shutdown! reg)))))

(deftest default-filter-at-create
  (testing "a manual awaiter naming a ling defaults to {:to <ling-id>}"
    (is (= {:to "L1"}
           (get-in (:ok (await-tool/create-request {:owner_kind "iso-stub" :ling_id "L1"}))
                   [:subscription :filter])))
    (is (= {:to "L2"}
           (get-in (:ok (await-tool/create-request {:owner {"kind" "iso-stub" "ling-id" "L2"}}))
                   [:subscription :filter]))
        "the owner object's ling id counts too")
    (is (= {:to "L3"}
           (get-in (:ok (await-tool/create-request {:owner_kind "iso-stub" :ling_id "L3"
                                                    :filter {}}))
                   [:subscription :filter]))
        "an empty filter is no filter"))
  (testing "an explicit filter is kept as given"
    (is (= {:sha "a"}
           (get-in (:ok (await-tool/create-request {:owner_kind "iso-stub" :ling_id "L1"
                                                    :filter {"sha" "a"}}))
                   [:subscription :filter]))))
  (testing "no ling id, or a non-manual source: no default"
    (is (nil? (get-in (:ok (await-tool/create-request {:owner_kind "iso-stub"}))
                      [:subscription :filter])))
    (is (nil? (get-in (:ok (await-tool/create-request {:owner_kind "iso-stub" :ling_id "L1"
                                                       :source "ling"}))
                      [:subscription :filter])))))

(deftest addressed-data-fills-to-from-the-target
  (let [target {:subscription {:type :manual :filter {:to "L1"}}}]
    (is (= {:to "L1" :x 1} (await-tool/addressed-data {:x 1} target)))
    (is (= {:to "other"} (await-tool/addressed-data {:to "other"} target))
        "an explicit :to is never overwritten, so a wrong address still misses")
    (is (= {:x 1} (await-tool/addressed-data {:x 1} nil)))
    (is (= {:x 1} (await-tool/addressed-data {:x 1} {:subscription {:type :manual}})))))

(deftest broadcast-fire-skips-unfiltered-awaiters
  (with-temp-registry
    (fn [reg]
      (doseq [id ["u-a" "u-b"]]
        (registry/create! reg {:id id :owner {:kind :iso-stub}
                               :subscription {:type :manual}})
        (registry/arm! reg id (str "run-" id)))
      (registry/create! reg {:id "f-a" :owner {:kind :iso-stub}
                             :subscription {:type :manual :filter {:to "ling-a"}}})
      (registry/arm! reg "f-a" "run-f-a")
      (testing "a fire naming no awaiter reaches only the filtered one"
        (is (= ["f-a"] (manual/fire! {:to "pv01-sync"})) "offered only to f-a")
        (Thread/sleep 100)
        (is (= [] @woken) "and its filter rejects a payload for another ling")
        (manual/fire! {:to "ling-a"})
        (is (wait-until 2000 #(= ["f-a"] @woken)))
        (is (= :armed (:status (registry/lookup reg "u-a"))))
        (is (= :armed (:status (registry/lookup reg "u-b")))))
      (testing "an unfiltered awaiter still wakes on a fire naming its id"
        (is (= ["u-a"] (manual/fire! {} "u-a")))
        (is (wait-until 2000 #(= ["f-a" "u-a"] @woken)))
        (is (= :armed (:status (registry/lookup reg "u-b"))))))))

(deftest verb-fire-is-isolated-per-ling
  (with-temp-registry
    (fn [_reg]
      (doseq [[id ling] [["w-a" "ling-a"] ["w-b" "ling-b"] ["w-n" nil]]]
        (let [c (await-tool/handle-create (cond-> {:id id :owner_kind "iso-stub"}
                                            ling (assoc :ling_id ling)))]
          (is (not (:isError c)))
          (is (= (when ling {:to ling})
                 (get-in (body c) [:awaiter :subscription :filter])))
          (await-tool/handle-arm {:id id :run_id (str "run-" id)})))
      (testing "the 15:05 shape: a fire with data.to for one ling wakes nobody else"
        (let [f (body (await-tool/handle-fire {:data {"to" "pv01-sync"}}))]
          (is (= ["w-a" "w-b"] (:offered f)) "the unfiltered w-n is not even offered it")
          (is (= [] (:woke f)))))
      (testing "data.to addresses one ling"
        (let [f (body (await-tool/handle-fire {:data {"to" "ling-b" "verdict" "green"}}))]
          (is (= ["w-b"] (mapv :id (:woke f)))))
        (is (wait-until 2000 #(= ["w-b"] @woken))))
      (testing "naming the id is enough: :to is filled from the target's filter"
        (let [f (body (await-tool/handle-fire {:id "w-a" :data {"verdict" "green"}}))]
          (is (= ["w-a"] (mapv :id (:woke f))))))
      (testing "an unfiltered awaiter wakes only on a fire naming it"
        (let [f (body (await-tool/handle-fire {:id "w-n"}))]
          (is (= ["w-n"] (mapv :id (:woke f))))))
      (is (wait-until 2000 #(= #{"w-a" "w-b" "w-n"} (set @woken)))))))
