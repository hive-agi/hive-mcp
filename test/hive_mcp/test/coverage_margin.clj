(ns hive-mcp.test.coverage-margin
  "Margin for a classification-coverage property.

   A `:classify` facet fails when any variant is reached fewer than
   num-tests / 4k times over k variants. An :in generator whose rarest
   variant sits near that floor fails on a fraction of random seeds, so the
   facet flakes without any code changing. `thin-variants` measures the
   generator itself over a large sample and names every variant whose share
   falls below half an even split (twice the default floor), which is the
   margin that keeps a seeded run clear of the floor."
  (:require [hive-schemas.strength :as strength]
            [hive-schemas.test :as hs-test]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn thin-variants
  "variant -> count for every variant of `domain` reached fewer than
   n / 2k times over `n` inputs generated from `?in` (k variants). An empty
   map means every variant has twice the default classify floor."
  [?in subject classify domain n]
  (let [variants (strength/classification-domain domain)
        freqs    (hs-test/classification-frequencies ?in subject classify n)
        need     (/ n (* 2 (count variants)))]
    (into (sorted-map)
          (keep (fn [v] (let [c (get freqs v 0)] (when (< c need) [v c]))))
          variants)))
