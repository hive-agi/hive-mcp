(ns hive-mcp.hot.restore
  "Bring back the namespaces a FAILED core reload pass unloaded and left
   absent. clj-reload unloads the whole cascade before it loads anything, so
   a pass that fails on one namespace leaves every later one unloaded, the
   server's own namespaces included, and every tool call that resolves them
   fails until something requires them again.

   `restore!` requires each absent namespace, in load order, one at a time;
   one that still fails stays unloaded and is reported. It never throws."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn absent
  "The namespaces of `unloaded` (clj-reload's unload order) that the image
   no longer has, in load order (the reverse)."
  [unloaded]
  (into [] (comp (map symbol) (distinct) (remove find-ns)) (reverse unloaded)))

(m/=> absent [:=> [:cat [:sequential [:or :symbol :string]]] [:vector :symbol]])

(defn- root-message [^Throwable t]
  (let [c (last (take-while some? (iterate #(.getCause ^Throwable %) t)))]
    (or (.getMessage ^Throwable c) (.getName (class c)))))

(defn restore!
  "Load every absent namespace of `unloaded` (`:reload`, so a name still
   listed in *loaded-libs* is loaded too). Returns
   {:restored [ns-str ...] :still-unloaded [{:ns ns-str :error str} ...]}."
  [unloaded]
  (reduce (fn [acc ns-sym]
            (if (find-ns ns-sym)
              acc
              (try
                (require ns-sym :reload)
                (update acc :restored conj (str ns-sym))
                (catch Throwable t
                  (update acc :still-unloaded conj {:ns (str ns-sym) :error (root-message t)})))))
          {:restored [] :still-unloaded []}
          (absent unloaded)))

(m/=> restore! [:=> [:cat [:sequential [:or :symbol :string]]]
                [:map [:restored [:vector :string]]
                 [:still-unloaded [:vector [:map [:ns :string] [:error :string]]]]]])
