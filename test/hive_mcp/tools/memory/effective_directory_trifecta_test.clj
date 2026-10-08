(ns hive-mcp.tools.memory.effective-directory-trifecta-test
  "Golden + property + mutation pinning for scope/effective-directory.

   `memory add` defaulted an omitted :directory to the request directory,
   `memory check_duplicate` passed nil on and resolved \"global\", so the
   duplicate check searched a different project than add wrote to (kanban
   20260728110541-3fa9f5f1). Both now resolve through effective-directory.

   Mutants are self-contained and never call the subject var."
  (:require [clojure.string :as str]
            [clojure.test :refer [is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.tools.memory.scope :as scope]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private gen-dir
  (gen/one-of [(gen/return nil)
               (gen/return "")
               (gen/return "   ")
               (gen/fmap #(str "/home/u/" %) gen/string-alphanumeric)]))

(def ^:private gen-input
  (gen/hash-map :directory gen-dir :current gen-dir))

(def ^:private input-of (atom nil))

(defn- run-effective
  "Subject wrapper: remembers its input so :pred can check the output."
  [input]
  (reset! input-of input)
  (scope/effective-directory input))

(defn- given? [d] (and (string? d) (not (str/blank? d))))

(defn- caller-wins-else-current? [out]
  (let [{:keys [directory current]} @input-of]
    (= out (if (given? directory) directory current))))

(deftrifecta effective-directory-contract
  hive-mcp.tools.memory.effective-directory-trifecta-test/run-effective
  {:golden-path "test/golden/tools/memory/effective-directory.edn"
   :cases       {:caller-given   {:directory "/home/u/a" :current "/home/u/b"}
                 :omitted        {:directory nil :current "/home/u/b"}
                 :blank          {:directory "  " :current "/home/u/b"}
                 :both-absent    {:directory nil :current nil}}
   :gen         gen-input
   :pred        caller-wins-else-current?
   :num-tests   200
   :mutations   [["pass-through — the original bug, nil reaches project-id"
                  (fn [{:keys [directory]}] directory)]
                 ["current-always — ignores an explicit directory"
                  (fn [{:keys [current]}] current)]
                 ["blank-counts — an empty string shadows the default"
                  (fn [{:keys [directory current]}] (or directory current))]]
   :assert      (fn []
                  (is (= "/req" (scope/effective-directory {:directory nil :current "/req"}))
                      "an omitted directory resolves to the request directory, as add does")
                  (is (= "/mine" (scope/effective-directory {:directory "/mine" :current "/req"}))
                      "an explicit directory wins"))})
