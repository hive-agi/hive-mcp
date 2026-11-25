(ns hive-mcp.await.adapters.ednl-journal
  "The default ports/IWakeJournal adapter: one EDN map per line, appended
   with an fsync before append-wake! answers.

   Reader contract (see `read-journal`): an entry may repeat under crash
   recovery, so a reader keeps the first entry per [:awaiter-id :attempt];
   a crash in the middle of an append can leave a torn LAST line, which a
   reader skips. Any other unreadable line is skipped and counted, never
   fatal, so one bad line cannot hide the rest of the ledger."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-mcp.await.ports :as ports]
            [malli.core :as m])
  (:import (java.io File FileOutputStream)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn path-beside
  "wakes.ednl in the directory of the registry file `registry-path`; the
   hive.await.journal system property wins when set. Pure over its inputs."
  [registry-path]
  (or (System/getProperty "hive.await.journal")
      (str (io/file (.getParentFile (.getAbsoluteFile (io/file registry-path))) "wakes.ednl"))))

(m/=> path-beside [:=> [:cat :string] :string])

(defn append-line!
  "Append `line` and a newline to `path`, then fsync, so the bytes are on
   the device before this answers. Creates the parent directory."
  [path ^String line]
  (let [f (io/file path)]
    (.mkdirs (.getParentFile (.getAbsoluteFile f)))
    (with-open [out (FileOutputStream. ^File f true)]
      (.write out (.getBytes (str line "\n") "UTF-8"))
      (.flush out)
      (.sync (.getFD out)))
    path))

(m/=> append-line! [:=> [:cat :string :string] :string])

(defrecord EdnlJournal [path]
  ports/IWakeJournal
  (append-wake! [_ entry]
    (locking EdnlJournal
      (append-line! path (pr-str entry)))
    entry))

(defn journal
  "An EdnlJournal over `path`."
  [path]
  (->EdnlJournal path))

(defn parse-lines
  "Pure: the journal text `text` as {:entries [..] :skipped n}. Entries are
   deduped on [:awaiter-id :attempt], the first one kept, in file order;
   unreadable lines (a torn tail) are skipped and counted."
  [text]
  (let [parsed (for [l (str/split-lines (or text ""))
                     :when (not (str/blank? l))]
                 (try (let [v (edn/read-string l)] (when (map? v) v))
                      (catch Exception _ nil)))
        good   (remove nil? parsed)]
    {:entries (second (reduce (fn [[seen acc] e]
                                (let [k [(:awaiter-id e) (:attempt e)]]
                                  (if (contains? seen k) [seen acc] [(conj seen k) (conj acc e)])))
                              [#{} []] good))
     :skipped (count (filter nil? parsed))}))

(m/=> parse-lines [:=> [:cat [:maybe :string]] [:map [:entries [:vector :map]] [:skipped :int]]])

(defn read-journal
  "The journal at `path`, read by the contract above. {:entries [] :skipped 0}
   when there is no file yet."
  [path]
  (let [f (io/file path)]
    (if (.exists f) (parse-lines (slurp f)) {:entries [] :skipped 0})))

(m/=> read-journal [:=> [:cat :string] [:map [:entries [:vector :map]] [:skipped :int]]])
