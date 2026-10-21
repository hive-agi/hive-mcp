(ns hive-mcp.protocols.editor-contract-test
  "Contract facets for the editor slot and its headless fallback. Every test
   owns the slot and puts the prior editor back."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.protocols.editor :as ed]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden])
  (:import [hive_mcp.protocols.editor NoopEditor]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- own-the-slot [f]
  (let [prior (when (ed/editor-set?) (ed/get-editor))]
    (ed/clear-editor!)
    (try (f)
         (finally (ed/clear-editor!)
                  (when prior (ed/set-editor! prior))))))

(use-fixtures :each own-the-slot)

(defrecord StubEditor []
  ed/IEditor
  (editor-id [_] :m17-stub)
  (available? [_] true)
  (eval-expr [_ code] {:ok code})
  (eval-expr [_ code _] {:ok code})
  (feature-available? [_ f] (= f "hive-mcp-swarm"))
  (send-to-terminal [_ _ _] {:ok true}))

(deftrifecta-from-schema noop-editor-is-headless
  #'ed/noop-editor
  {:in [:cat]
   :out ed/Editor
   :rel (fn [_ out] (and (instance? NoopEditor out) (= :noop (ed/editor-id out))
                         (false? (ed/available? out))
                         (= :editor/not-available (:error (ed/eval-expr out "(+ 1 1)")))))
   :num-tests 10})

(deftrifecta-from-schema an-empty-slot-answers-the-noop-editor
  #'ed/get-editor
  {:in [:cat]
   :out ed/Editor
   :rel (fn [_ out] (= :noop (ed/editor-id out)))
   :num-tests 10})

(golden/deftest-golden slot-lifecycle-golden
  "test/golden/hive-mcp/protocols/editor-lifecycle.edn"
  (let [before {:set? (ed/editor-set?) :id (ed/editor-id (ed/get-editor))}
        installed (ed/set-editor! (->StubEditor))
        during {:set? (ed/editor-set?) :id (ed/editor-id (ed/get-editor))
                :same? (identical? installed (ed/get-editor))
                :eval (ed/eval-expr (ed/get-editor) "(+ 1 1)")
                :swarm? (ed/feature-available? (ed/get-editor) "hive-mcp-swarm")}
        cleared (ed/clear-editor!)]
    {:before before :during during :cleared cleared
     :after {:set? (ed/editor-set?) :id (ed/editor-id (ed/get-editor))}}))

(deftest set-rejects-a-non-editor
  (is (thrown? AssertionError (ed/set-editor! {:editor-id :fake})))
  (is (false? (ed/editor-set?))))
