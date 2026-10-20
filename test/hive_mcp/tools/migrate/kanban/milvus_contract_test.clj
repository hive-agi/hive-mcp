(ns hive-mcp.tools.migrate.kanban.milvus-contract-test
  "Contract facets for the milvus adapter constructors: the id lister keeps
   the given tag filter and page cap, and falls back to the defaults."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.migrate.kanban.milvus :as mv]
            [hive-mcp.tools.migrate.kanban.ports :as ports]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn lister-config
  "Subject: the filter and cap a lister built from OPTS carries."
  [opts]
  (select-keys (mv/make-id-lister nil opts) [:tag-filter :page-cap]))

(deftrifecta-from-schema id-lister-defaults-what-is-not-given
  #'lister-config
  {:in [:map [:tag-filter {:optional true} [:enum "tags like \"%x%\"" "t"]]
        [:page-cap {:optional true} [:int {:min 1 :max 9}]]]
   :out [:map [:tag-filter :string] [:page-cap [:int {:min 1}]]]
   :rel (fn [opts out]
          (= out {:tag-filter (or (:tag-filter opts) mv/default-tag-filter)
                  :page-cap (or (:page-cap opts) mv/default-page-cap)}))
   :num-tests 60})

(golden/deftest-golden milvus-lister-defaults-golden
  "test/golden/hive-mcp/tools/migrate/kanban-milvus-lister.edn"
  {:default (lister-config {})
   :capped  (lister-config {:page-cap 5})})

(deftest constructors-build-their-ports
  (is (satisfies? ports/IIdLister (mv/make-id-lister nil)))
  (is (satisfies? ports/IEntryReader (mv/make-entry-reader nil))))
