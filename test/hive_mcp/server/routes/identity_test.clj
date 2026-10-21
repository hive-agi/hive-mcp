(ns hive-mcp.server.routes.identity-test
  "Facets for hive-mcp.server.routes.identity (cov-facets f07). The content
   helpers are pure and are driven from their contracts; the identity
   extractors are driven on args that carry an explicit override, so the
   result never depends on the vessel registry or the server cwd."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-dsl.context.identity :as ctx-id]
            [hive-mcp.server.routes.identity :as id]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private TextItem [:map [:type [:= "text"]] [:text [:string {:max 8}]]])
(def ^:private ImageItem [:map [:type [:= "image"]] [:data [:string {:max 4}]]])
(def ^:private Item [:or TextItem ImageItem])
(def ^:private ContentVec [:vector {:gen/max 4} Item])

(deftrifecta-from-schema normalize-content-contract id/normalize-content
  {:in  [:cat [:or ContentVec [:map [:content ContentVec]] TextItem :int :string :nil]]
   :out [:sequential :any]
   :rel (fn [[r] out]
          (cond (sequential? r)                    (= out r)
                (and (map? r) (:content r))       (= out (:content r))
                (map? r)                          (= out [r])
                :else                             (= out [{:type "text" :text (str r)}])))
   :golden-path "test/golden/hive-mcp/f07-identity-normalize-content.edn"})

(deftrifecta-from-schema find-last-text-idx-contract id/find-last-text-idx
  {:in  [:cat [:maybe ContentVec]]
   :out [:maybe [:int {:min 0}]]
   :rel (fn [[c] out]
          (let [rev (vec (reverse c))]
            (if (some #(= "text" (:type %)) c)
              (and (= "text" (:type (nth rev out)))
                   (not-any? #(= "text" (:type %)) (subvec rev 0 out)))
              (nil? out))))
   :golden-path "test/golden/hive-mcp/f07-identity-find-last-text-idx.edn"})

(deftrifecta-from-schema wrap-delimited-block-contract id/wrap-delimited-block
  {:in  [:cat ContentVec [:enum "HIVEMIND" "MEMORY"] [:maybe [:string {:max 6}]]]
   :out [:sequential :any]
   :rel (fn [[c tag body] out]
          (if (seq body)
            (and (str/includes? (apply str (keep :text out))
                                (str "---" tag "---\n" body "\n---/" tag "---"))
                 (<= (count c) (count out) (inc (count c))))
            (= out c)))
   :golden-path "test/golden/hive-mcp/f07-identity-wrap-delimited-block.edn"})

(deftest-golden identity-extractors-golden
  "test/golden/hive-mcp/f07-identity-extractors.edn"
  {:agent-id        [(id/extract-agent-id {:agent_id "a"} "d")
                     (id/extract-agent-id {:agent-id "b"} "d")
                     (id/extract-agent-id nil "d")]
   :caller-id       [(id/extract-caller-id {:_caller_id "ling-1" :agent_id "target"})
                     (id/extract-caller-id {})]
   :directory       [(id/extract-directory {:directory "/a" :_caller_cwd "/b"})
                     (id/extract-directory {:_caller_cwd "/b"})]
   :project-id      [(id/extract-project-id {:project_id "p1"})
                     (id/extract-project-id {:project-id "p2"})]
   :caller-identity [(id/extract-caller-identity {})
                     (id/extract-caller-identity {:_caller_id "ling-x"})]
   :project-scope   (id/extract-project-scope {:project_id "hive"})
   :piggyback       [(id/wrap-piggyback [{:type "text" :text "t"}] [{:a 1}])
                     (id/wrap-piggyback [{:type "text" :text "t"}] [])
                     (id/wrap-memory-piggyback-content {:type "text" :text "m"} {:n 1})
                     (id/wrap-memory-piggyback-content [] nil)]})

(deftest caller-identity-conforms
  (is (ctx-id/caller-id? (id/extract-caller-identity {:_caller_id "ling-x"}))))
