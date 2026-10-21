(ns hive-mcp.migration.core-facets-test
  "Facets for the backup core: trifectas on the pure name parser and the
   scope guard, and a golden over the backup file round trip against a temp
   directory (write, list with every filter, latest, read, validate)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.migration.core :as mc]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden-fn]]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private BackupName
  ;; Half well-formed names, half near misses (bad stamp, bad extension),
  ;; so both the parse and the nil arm are reached.
  [:string {:gen/schema [:tuple [:enum "kg" "memory" "full"] [:enum "datalevin" "chroma"]
                         [:enum "20260203T164530" "20261020T000000" "2026-02-03" "20260203T1645"]
                         [:enum ".edn" ".json"]]
            :gen/fmap (fn [[s b t ext]] (str s "-" b "-" t ext))}])

(deftrifecta-from-schema parse-backup-name-reads-the-pattern
  #'mc/parse-backup-name
  {:in [:cat BackupName]
   :out [:maybe [:map [:scope :keyword] [:backend :keyword] [:timestamp :string] [:filename :string]]]
   :rel (fn [[filename] out]
          (if out
            (= filename (str (name (:scope out)) "-" (name (:backend out)) "-" (:timestamp out) ".edn"))
            (not (and (str/ends-with? filename ".edn")
                      (some #(str/includes? filename %) ["T164530" "T000000"])))))
   :classify (fn [_ out] (if out :parsed :rejected))
   :classify-domain #{:parsed :rejected}
   :golden-path "test/golden/migration/core-parse-backup-name.edn"
   :num-tests 300})

(def ^:private Pid [:enum "global" "hive-mcp" "hive-mcp:agora" "other"])

(def ^:private BackupAndTarget
  ;; Half the cases pin two different named projects, so the cross-project
  ;; refusal is reached as often as the compatible arms.
  [:cat {:gen/schema [:tuple [:map [:backup/project-id {:optional true} Pid]] [:maybe Pid] :boolean]
         :gen/fmap (fn [[b t cross?]]
                     (if cross? [{:backup/project-id "other"} "hive-mcp"] [b t]))}
   [:map [:backup/project-id {:optional true} Pid]] [:maybe Pid]])

(deftrifecta-from-schema check-scope-compatibility-guards-cross-project
  #'mc/check-scope-compatibility
  {:in BackupAndTarget
   :out [:map [:compatible? :boolean] [:backup-project-id :string]
         [:target-project-id [:maybe :string]] [:reason :string]]
   :rel (fn [[backup target] out]
          (let [bpid (or (:backup/project-id backup) "global")]
            (and (= bpid (:backup-project-id out))
                 (= target (:target-project-id out))
                 (= (:compatible? out) (or (nil? target) (= bpid target) (= bpid "global"))))))
   :classify (fn [_ out] (if (:compatible? out) :compatible :cross-project))
   :classify-domain #{:compatible :cross-project}
   :golden-path "test/golden/migration/core-check-scope-compatibility.edn"
   :num-tests 300})

(defn- temp-dir []
  (.toFile (java.nio.file.Files/createTempDirectory
            "migration-core" (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- rm-rf [^java.io.File f]
  (when (.isDirectory f) (run! rm-rf (.listFiles f)))
  (.delete f))

(defn- listed [backups] (mapv :filename backups))

(def ^:private host-keys [:backup/created-at :backup/created-by :backup/hostname])

(defn- scenario []
  (let [root (temp-dir)
        dir  (str (.getPath root) "/backups")
        md   (fn [scope pid] (apply dissoc (mc/backup-metadata scope :datalevin {:n 1} pid) host-keys))]
    (try
      (mc/ensure-backup-dir! dir)
      (mc/write-backup! (str dir "/kg-datalevin-20260101T000000.edn") (md :kg nil) {:kg/edges [1]})
      (mc/write-backup! (str dir "/kg-datalevin-20260201T000000.edn") (md :kg "hive-mcp") {:kg/edges [2]})
      (mc/write-backup! (str dir "/memory-chroma-20260301T000000.edn") (md :memory "other") {:entries []})
      (mc/write-backup! (str dir "/kg-datalevin-20260401T000000.edn") (md :kg "other") {:kg/edges [3]})
      (spit (str dir "/notes.edn") "{}")
      (spit (str dir "/kg-datalevin-20260501T000000.edn") "{:broken")
      (spit (str dir "/memory-chroma-20260601T000000.edn") "{:backup/scope :memory}")
      {:metadata     (md :kg nil)
       :all          (listed (mc/list-backups {:dir dir}))
       :kg-only      (listed (mc/list-backups {:dir dir :scope :kg}))
       :chroma-only  (listed (mc/list-backups {:dir dir :backend :chroma}))
       :agora-sees   (listed (mc/list-backups {:dir dir :project-id "hive-mcp:agora"}))
       :limit-2      (listed (mc/list-backups {:dir dir :limit 2}))
       :missing-dir  (mc/list-backups {:dir (str dir "/nope")})
       :latest-kg    (:filename (mc/latest-backup :kg {:dir dir}))
       :latest-none  (mc/latest-backup :full {:dir dir})
       :read         (mc/read-backup (str dir "/kg-datalevin-20260201T000000.edn"))
       :valid        (mc/validate-backup (str dir "/kg-datalevin-20260201T000000.edn"))
       :missing-keys (mc/validate-backup (str dir "/memory-chroma-20260601T000000.edn"))
       :unreadable   (update (mc/validate-backup (str dir "/kg-datalevin-20260501T000000.edn"))
                             :errors #(mapv (fn [e] (str/starts-with? e "Failed to read backup")) %))}
      (finally (rm-rf root)))))

(deftest-golden-fn backup-round-trip-golden
  "test/golden/migration/core-backup-round-trip.edn"
  scenario)

(deftest generated-names-parse-back
  (let [n (mc/generate-backup-name :kg :datahike)]
    (is (= {:scope :kg :backend :datahike} (select-keys (mc/parse-backup-name n) [:scope :backend])))))

(deftest metadata-carries-host-facts
  (let [m (mc/backup-metadata :kg :datalevin {} nil)]
    (is (= "global" (:backup/project-id m)))
    (is (inst? (:backup/created-at m)))
    (is (= mc/backup-format-version (:backup/version m)))))
