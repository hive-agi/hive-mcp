(ns hive-mcp.channel.blocks-golden-test
  "Facets for the block registry: a trifecta on the pure `tag-of`, and a
   golden over `render` with stub emitters injected through the extension
   registry: ordering by cue hits, blank and throwing emitters, truncation,
   the response budget and the strict caller budget that defers blocks."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.blocks :as blocks]
            [hive-mcp.extensions.registry :as ext]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- isolate-emitters
  "Run f with no block emitter but the ones it registers, restoring the
   emitters that were registered before."
  [f]
  (let [prior (into {} (map (juxt identity ext/get-extension)) (blocks/emitter-keys))]
    (run! ext/deregister! (keys prior))
    (try (f)
         (finally
           (run! ext/deregister! (blocks/emitter-keys))
           (doseq [[k v] prior] (ext/register! k v))))))

(use-fixtures :each isolate-emitters)

(deftrifecta-from-schema tag-of-trifecta
  hive-mcp.channel.blocks/tag-of
  {:in          [:or [:enum :block/frontier :block/kanban-hints :x/y] [:enum "abc" "Mixed-Case"] [:enum 'sym 'a/b]]
   :out         :string
   :rel         (fn [k out] (and (= out (str/upper-case out)) (= (str/lower-case out) (str/lower-case (name k)))))
   :n-cases     8
   :golden-path "test/golden/channel/blocks-tag-of.edn"})

(def ^:private emitters
  {:block/alpha  (fn [_] "alpha body mentions kanban")
   :block/beta   (fn [{:keys [tool-name]}] (str "beta for " tool-name " axiom kanban"))
   :block/blank  (fn [_] "   ")
   :block/nilly  (fn [_] nil)
   :block/boom   (fn [_] (throw (ex-info "boom" {})))
   :block/datum  (fn [_] {:n 1})
   :block/huge   (fn [_] (apply str (repeat 4100 "z")))})

(deftest-golden-fn render-golden
  "test/golden/channel/blocks-render.edn"
  (fn []
    (run! ext/deregister! (blocks/emitter-keys))
    (doseq [[k f] emitters] (ext/register! k f))
    (let [shorten (fn [bs] (mapv (fn [[tag body]] [tag (count body) (subs body 0 (min 30 (count body)))]) bs))
          ctx     {:tool-name "memory" :cues #{"axiom" "kanban"} :caller-id "c-1"}]
      {:keys        (blocks/emitter-keys)
       :no-cues     (shorten (blocks/render {:tool-name "memory"}))
       :cue-ordered (shorten (blocks/render ctx))
       :zero-budget (blocks/render ctx 0)
       :strict-tiny (shorten (blocks/render ctx 80))
       ;; what the strict call deferred is delivered on the caller's next call
       :next-call   (shorten (blocks/render ctx 5000 true))
       :nil-ctx     (shorten (blocks/render nil))})))

(deftest only-block-namespace-keys-are-emitters-test
  (ext/register! :block/one (constantly "x"))
  (ext/register! :not-a-block/two (constantly "y"))
  (try
    (is (= [:block/one] (blocks/emitter-keys)))
    (finally (ext/deregister! :not-a-block/two))))
