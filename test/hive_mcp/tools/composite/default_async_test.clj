(ns hive-mcp.tools.composite.default-async-test
  "Trifectas for the default-async fold, driven by the contracts in
   `hive-mcp.tools.composite.default-async`.

   The answers are keywords with a space in them (`:kg batch`), which EDN
   cannot read back, so the trifectas carry no :golden-path; the golden below
   pins the same answers spelled as strings."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.tools.composite.default-async :as da]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Spec
  "A contribution spec over every :default-async arm: true, false, nil, a verb
   vector or a verb set. (A nil spec and a missing key are pinned in the
   deftest below.)"
  [:map [:default-async
         [:or :boolean :nil
          [:vector {:min 1 :max 3} [:enum :run "scan" 'list]]
          [:set {:min 1 :max 3} [:enum :run "scan" 'list]]]]])

(def ^:private Cmds
  [:map-of {:min 1 :max 4} [:enum :kg "memory" 'carto :agent] Spec])

(defn- oracle [cmds]
  (set (for [[c {d :default-async}] cmds
             k (cond (true? d) [(keyword (name c))]
                     (coll? d) (map #(keyword (str (name c) " " (name %))) d))]
         k)))

(deftrifecta-from-schema contributed-commands-spells-command-or-verbs
  hive-mcp.tools.composite.default-async/contributed-commands
  {:in        Cmds
   :out       [:set :keyword]
   :rel       (fn [in out] (= out (oracle in)))
   :num-tests 300})

(def ^:private ToolDef
  [:maybe [:map [:name [:= "kg"]]
           [:default-async-commands {:optional true}
            [:maybe [:set {:max 2} [:enum :batch :traverse]]]]]])

(deftrifecta-from-schema fold-joins-contributed-commands
  hive-mcp.tools.composite.default-async/fold
  {:in        [:cat ToolDef Cmds]
   :out       da/FoldableToolDef
   :rel       (fn [[tool cmds] out]
                (let [c (oracle cmds)]
                  (if (seq c)
                    (= (:default-async-commands out)
                       (into (or (:default-async-commands tool) #{}) c))
                    (= out tool))))
   :num-tests 300})

(defn- spelled [ks] (into (sorted-set) (map #(subs (str %) 1)) ks))

(deftest-golden default-async-answers
  "test/golden/tools/composite/default-async-answers.edn"
  {:nil-cmds      (spelled (da/contributed-commands nil))
   :true-command  (spelled (da/contributed-commands {:kg {:default-async true}}))
   :false-command (spelled (da/contributed-commands {:kg {:default-async false}}))
   :verb-vector   (spelled (da/contributed-commands {"memory" {:default-async [:add "search"]}}))
   :verb-set      (spelled (da/contributed-commands {'carto {:default-async #{'scan}}}))
   :mixed         (spelled (da/contributed-commands {:kg {:default-async true}
                                                     :agent {:default-async [:spawn]}
                                                     :x nil :y {}}))
   :fold-joins    (update (da/fold {:name "kg" :default-async-commands #{:batch}}
                                   {:agent {:default-async [:spawn]}})
                          :default-async-commands spelled)
   :fold-noop     (da/fold {:name "kg"} {:x {:default-async false}})})

(deftest fold-is-a-no-op-without-contributions
  (is (= #{} (da/contributed-commands nil)))
  (is (= #{} (da/contributed-commands {:x nil :y {}})) "a nil spec or no key queues nothing")
  (is (= {:name "kg"} (da/fold {:name "kg"} {:x {:default-async false}})))
  (is (= {:name "kg" :default-async-commands #{:x (keyword "y run")}}
         (da/fold {:name "kg"} {:x {:default-async true} :y {:default-async [:run]}}))
      "a verb collection is spelled `<command> <verb>`"))
