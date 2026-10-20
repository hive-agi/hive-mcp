(ns hive-mcp.channel.memory-piggyback-golden-test
  "Memory piggyback buffers and the all-channel tap under instrumentation:
   one enqueue/drain cycle and one tap drain pinned as goldens, plus a
   trifecta over the drained batch for generated entry lists."
  (:require [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.channel.memory-piggyback :as mp]
            [hive-mcp.channel.piggyback-tap :as tap]
            [hive-test.golden :refer [deftest-golden]]
            [hive-test.trifecta :refer [deftrifecta]]
            [clojure.test.check.generators :as gen]
            [malli.core :as m]
            [malli.instrument :as mi]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private contracted
  #{#'mp/enqueue! #'mp/drain! #'mp/has-pending? #'mp/clear-buffer!
    #'mp/reset-all! #'mp/adopt-buffer! #'mp/evict-orphaned-buffers!
    #'tap/drain-all!})

(use-fixtures :each
  (fn [f]
    (mp/reset-all!)
    (mi/instrument! {:filters [(mi/-filter-var contracted)]})
    (try (f)
         (finally
           (mi/unstrument! {:filters [(mi/-filter-var contracted)]})
           (mp/reset-all!)))))

(def ^:private entries
  [{:id "ax-1" :type "axiom" :content "Never delete data" :severity "INVIOLABLE" :tags ["axiom"]}
   {:id "c-1" :type "convention" :content "Use carto" :tags ["carto"]}
   {:id "n-1" :type "note" :content "A note" :tags ["memory"]}])

(deftest-golden enqueue-then-drain
  "test/golden/channel/memory-piggyback-drain.edn"
  (let [caller "m05-golden-caller"]
    (mp/enqueue! caller entries)
    (let [pending? (mp/has-pending? caller)
          d1 (mp/drain! caller {:tokens #{"carto"}})
          d2 (mp/drain! caller)]
      {:pending? pending?
       :batch (mapv (juxt :id :T) (:batch d1))
       :delivered (:delivered d1)
       :done (:done d1)
       :second d2
       :after (mp/has-pending? caller)})))

(deftest-golden tap-drains-the-memory-channel
  "test/golden/channel/piggyback-tap-memory.edn"
  (do (mp/enqueue! "m05-tap-agent" (take 1 entries))
      (let [out (tap/drain-all! "m05-tap-agent" nil)]
        {:string? (string? out)
         :has-memory-block? (boolean (some-> out (.contains "MEMORY")))
         :carries-id? (boolean (some-> out (.contains "ax-1")))})))

(defn- drained-ids
  "Unary subject: ids drained for one entry list with no drain ctx."
  [es]
  (mp/reset-all!)
  (mp/enqueue! "m05-trifecta" es)
  (loop [acc []]
    (if-let [d (mp/drain! "m05-trifecta")]
      (recur (into acc (map :id) (:batch d)))
      acc)))

(def ^:private gen-entry
  (gen/hash-map :id (gen/fmap #(str "e" %) (gen/choose 0 20))
                :type (gen/elements ["axiom" "note" "decision"])
                :content (gen/fmap #(apply str %) (gen/vector gen/char-alpha 0 30))))

(deftrifecta drain-order
  hive-mcp.channel.memory-piggyback-golden-test/drained-ids
  {:golden-path "test/golden/channel/memory-piggyback-order.edn"
   :cases {:empty [] :mixed entries
           :axiom-last [{:id "n" :type "note" :content "x"} {:id "a" :type "axiom" :content "y"}]}
   :gen (gen/vector gen-entry 0 6)
   :pred (fn [ids] (and (vector? ids) (every? string? ids)))
   :num-tests 50
   :mutations [["drops-all" (constantly [])]
               ["reversed" (fn [es] (vec (reverse (drained-ids es))))]]})

(deftest nothing-pending-tap-is-nil
  (is (nil? (tap/drain-all! "m05-nobody" nil nil))))
