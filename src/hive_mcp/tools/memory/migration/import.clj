(ns hive-mcp.tools.memory.migration.import
  "JSON import: migrate legacy Emacs-backed memory JSON into the current store."
  (:require [hive-mcp.tools.memory.core :refer [with-store]]
            [hive-mcp.tools.memory.scope :as scope]
            [hive-mcp.tools.core :refer [mcp-json]]
            [hive-mcp.protocols.memory :as mem-proto]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]
            [hive-mcp.vectordb.resilience :refer [with-resilience]]
            [clojure.java.io :as io]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn import-entry!
  "Import a single entry to memory store with content-hash deduplication."
  [entry project-id]
  (let [store (mem-proto/get-store)
        entry-hash (or (:content-hash entry)
                       (mem-proto/content-hash (:content entry)))
        entry-type (or (:type entry) "note")]
    (cond
      (with-resilience
        (mem-proto/find-duplicate store entry-type entry-hash {:project-id project-id}))
      :skipped-hash

      (with-resilience
        (mem-proto/get-entry store (:id entry)))
      :skipped-id

      :else
      (do
        (with-resilience
          (mem-proto/add-entry! store
                                {:id (:id entry)
                                 :type entry-type
                                 :content (:content entry)
                                 :tags (if (vector? (:tags entry))
                                         (vec (:tags entry))
                                         (:tags entry))
                                 :content-hash entry-hash
                                 :created (:created entry)
                                 :updated (:updated entry)
                                 :duration (or (:duration entry) "long")
                                 :expires (or (:expires entry) "")
                                 :access-count (or (:access-count entry) 0)
                                 :helpful-count (or (:helpful-count entry) 0)
                                 :unhelpful-count (or (:unhelpful-count entry) 0)
                                 :project-id project-id}))
        :imported))))

(def ^:private legacy-types
  "Legacy storage file stem -> the by-type key the import reports it under."
  [["note" :notes] ["snippet" :snippets] ["convention" :conventions] ["decision" :decisions]])

(defn default-legacy-dir
  "Root of the legacy Emacs JSON storage: HIVE_MCP_LEGACY_MEMORY_DIR when set,
   else the first existing of ~/.emacs.d/hive-mcp and ~/.config/emacs/hive-mcp
   (the old `hive-mcp-memory-storage-directory` default), else the first."
  []
  (let [home (System/getProperty "user.home")
        candidates [(str home "/.emacs.d/hive-mcp") (str home "/.config/emacs/hive-mcp")]]
    (or (not-empty (System/getenv "HIVE_MCP_LEGACY_MEMORY_DIR"))
        (first (filter #(.isDirectory (io/file %)) candidates))
        (first candidates))))

(defn legacy-project-dir
  "Directory holding PROJECT-ID's legacy files under ROOT, mirroring the old
   elisp `hive-mcp-memory-storage-project-dir`: global/ or projects/<id>/."
  [root project-id]
  (if (= "global" project-id)
    (io/file root "global")
    (io/file root "projects" project-id)))

(defn read-legacy-export
  "Read PROJECT-ID's legacy JSON files under ROOT straight from disk.
   Returns {:success true :result {:notes [..] :snippets [..] ...}} (a missing
   type file reads as no entries) or {:success false :error msg} when the
   project directory is absent or a file does not parse."
  [root project-id]
  (let [dir (legacy-project-dir root project-id)]
    (if-not (.isDirectory dir)
      {:success false :error (str "no legacy memory directory at " (.getPath dir))}
      (try
        {:success true
         :result (into {}
                       (for [[stem k] legacy-types
                             :let [f (io/file dir (str stem ".json"))]]
                         [k (if (and (.isFile f) (pos? (.length f)))
                              (vec (json/read-str (slurp f) :key-fn keyword))
                              [])]))}
        (catch Exception e
          {:success false :error (str "unreadable legacy JSON in " (.getPath dir) ": " (ex-message e))})))))

(defn handle-import-json
  "Import memory entries from legacy Emacs JSON storage.
   Reads the JSON files directly (see `read-legacy-export`); the former Emacs
   round-trip through `hive-mcp-memory-query` always failed, so the import no
   longer depends on the editor at all. LEGACY-DIR overrides the storage root."
  [{:keys [project-id dry-run legacy-dir]}]
  (log/info "mcp-memory-import-json:" project-id "dry-run:" dry-run)
  (with-store
    (let [pid (or project-id (scope/get-current-project-id))
          {:keys [success result error]} (read-legacy-export (or legacy-dir (default-legacy-dir)) pid)]
      (if-not success
        (mcp-json {:error (str "Failed to read JSON: " error)})
        (let [data result
              all-entries (concat (:notes data) (:snippets data)
                                  (:conventions data) (:decisions data))]
          (if dry-run
            (mcp-json {:dry-run true
                       :would-import (count all-entries)
                       :by-type {:notes (count (:notes data))
                                 :snippets (count (:snippets data))
                                 :conventions (count (:conventions data))
                                 :decisions (count (:decisions data))}})
            (let [results (mapv #(import-entry! % pid) all-entries)
                  imported (count (filter #(= :imported %) results))
                  skipped-hash (count (filter #(= :skipped-hash %) results))
                  skipped-id (count (filter #(= :skipped-id %) results))]
              (mcp-json {:imported imported
                         :skipped {:by-hash skipped-hash
                                   :by-id skipped-id
                                   :total (+ skipped-hash skipped-id)}
                         :project-id pid}))))))))
