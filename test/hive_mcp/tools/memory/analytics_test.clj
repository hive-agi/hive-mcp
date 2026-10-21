(ns hive-mcp.tools.memory.analytics-test
  "Facets for the memory analytics handlers, with the memory port satisfied
   by the stub store (no with-redefs).

   helpfulness-ratio reads only: a trifecta over its contract on a seeded
   store (e1 has counts, e2 none, \"nope\" is absent). log-access and
   feedback write: a golden over their answers and the store afterwards
   (wall-clock :last-accessed and :updated are not pinned)."
  (:require [clojure.data.json :as json]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.memory :as mp]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.core :as core]
            [hive-mcp.tools.memory.analytics :as an]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private entries
  [{:id "e1" :type "note" :content "one" :project-id "proj-a" :tags ["a"]
    :helpful-count 3 :unhelpful-count 1 :access-count 2}
   {:id "e2" :type "note" :content "two" :project-id "proj-a" :tags ["xpoll:project:proj-b"]}])

(use-fixtures :each
  (fn [t] (stub/with-stub-store (fn [] (stub/seed! (mp/get-store) entries) (t)))))

(def ^:private seeded
  {"e1" {:ratio 0.75 :helpful 3 :unhelpful 1}
   "e2" {:ratio nil :helpful 0 :unhelpful 0}})

(defn- body [resp] (json/read-str (:text resp) :key-fn keyword))

(deftrifecta-from-schema helpfulness-ratio-reads-the-counts
  hive-mcp.tools.memory.analytics/handle-helpfulness-ratio
  {:in          [:map [:id [:enum "e1" "e2" "nope"]]]
   :out         core/McpResponse
   :rel         (fn [{:keys [id]} out]
                  (= (body out) (get seeded id {:error "Entry not found"})))
   :num-tests   100
   :n-cases     6
   :golden-path "test/golden/tools/memory/analytics-helpfulness-ratio.edn"})

(defn- stored [id]
  (select-keys (get (stub/entries (mp/get-store)) id)
               [:id :access-count :helpful-count :unhelpful-count :tags]))

(defn- answer [resp]
  (let [b (body resp)]
    (if (map? b) (dissoc b :last-accessed :last_accessed :updated :created :expires) b)))

;; NOTE (reported, not fixed): handle-log-access writes :tags as a
;; comma-joined STRING on a cross-project access. A store that keeps the
;; string as given (the stub does) then reads it back as a string, and a
;; second cross-project access splits it into characters. The port does not
;; document the :tags update shape, so only the first access is pinned here.
(deftest-golden analytics-writes
  "test/golden/tools/memory/analytics-writes.edn"
  {:log-same-project  (answer (an/handle-log-access {:id "e1"}))
   :log-cross-project (answer (an/handle-log-access {:id "e2" :directory "/tmp/proj-c"}))
   :log-missing       (answer (an/handle-log-access {:id "nope"}))
   :fb-helpful        (answer (an/handle-feedback {:id "e2" :feedback "helpful"}))
   :fb-unhelpful      (answer (an/handle-feedback {:id "e1" :feedback "unhelpful"}))
   :fb-missing        (answer (an/handle-feedback {:id "nope" :feedback "helpful"}))
   :stored            [(stored "e1") (stored "e2")]})

(deftest same-project-access-adds-no-xpoll-tag
  (let [b (body (an/handle-log-access {:id "e2" :directory "/tmp/proj-a"}))]
    (is (nil? (:xpoll b)))
    (is (= 1 (:access-count b)))))

(deftest invalid-feedback-is-refused
  (is (:isError (an/handle-feedback {:id "e1" :feedback "bogus"}))))
