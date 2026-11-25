(ns hive-mcp.await.adapters.ednl-journal
  "The default ports/IWakeJournal adapter: one EDN map per line, appended
   with an fsync before append-wake! answers.

   Reader contract (see `read-journal`):
   - The registry claims and persists an attempt number BEFORE the attempt
     runs, so each [:awaiter-id :attempt] has one outcome. A pair can still
     appear twice when a crash falls between the journal fsync and the
     registry write that follows it: that second line is a REPEAT of the
     same outcome. A reader collapses a pair to one entry: the last line
     wins, except that an :ok? true entry is never replaced by a failed one.
   - A crash mid-append can leave a torn line. append-line! writes a newline
     before appending after one, so a torn fragment always sits alone on its
     line; a reader skips it (and any other unreadable line), counts it, and
     never fails on it."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [hive-mcp.await.ports :as ports]
            [malli.core :as m])
  (:import (java.io File FileOutputStream RandomAccessFile)))

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

(defn- ends-without-newline?
  "True when the file `f` is non-empty and its last byte is not a newline:
   the tail a crash mid-append leaves."
  [^File f]
  (and (.exists f) (pos? (.length f))
       (with-open [raf (RandomAccessFile. f "r")]
         (.seek raf (dec (.length raf)))
         (not= (int \newline) (.read raf)))))

(defn append-line!
  "Append `line` and a newline to `path`, then fsync, so the bytes are on
   the device before this answers. Creates the parent directory.

   When the file does not end with a newline (a torn line left by a crash
   mid-append), a newline is written FIRST, so the torn fragment stays alone
   on its line and the reader skips only it, never the entry appended after
   it (review N2)."
  [path ^String line]
  (let [f (io/file path)]
    (.mkdirs (.getParentFile (.getAbsoluteFile f)))
    (let [sep (if (ends-without-newline? f) "\n" "")]
      (with-open [out (FileOutputStream. ^File f true)]
        (.write out (.getBytes (str sep line "\n") "UTF-8"))
        (.flush out)
        (.sync (.getFD out))))
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

(m/=> journal [:=> [:cat :string] [:fn #(satisfies? ports/IWakeJournal %)]])

(defn parse-lines
  "Pure: the journal text `text` as {:entries [..] :skipped n}. One entry per
   [:awaiter-id :attempt], in order of first appearance; for a repeated pair
   the last line wins, except that an :ok? true entry is never replaced by a
   failed one. Unreadable lines (a torn fragment) are skipped and counted."
  [text]
  (let [parsed (for [l (str/split-lines (or text ""))
                     :when (not (str/blank? l))]
                 (try (let [v (edn/read-string l)] (when (map? v) v))
                      (catch Exception _ nil)))
        good   (remove nil? parsed)
        [order by-k] (reduce (fn [[order by-k] e]
                               (let [k    [(:awaiter-id e) (:attempt e)]
                                     prev (get by-k k)]
                                 [(if prev order (conj order k))
                                  (assoc by-k k (if (and (:ok? prev) (not (:ok? e))) prev e))]))
                             [[] {}] good)]
    {:entries (mapv by-k order)
     :skipped (count (filter nil? parsed))}))

(m/=> parse-lines [:=> [:cat [:maybe :string]] [:map [:entries [:vector :map]] [:skipped :int]]])

(defn read-journal
  "The journal at `path`, read by the contract above. {:entries [] :skipped 0}
   when there is no file yet."
  [path]
  (let [f (io/file path)]
    (if (.exists f) (parse-lines (slurp f)) {:entries [] :skipped 0})))

(m/=> read-journal [:=> [:cat :string] [:map [:entries [:vector :map]] [:skipped :int]]])
