(ns hive-mcp.kondo.config-export-test
  "DETECTOR for KONDO-MCP-DIVERGE: the inline .clj-kondo/config.edn and the
   repo's own clj-kondo export must not disagree.

   hive-mcp self-exports a clj-kondo config under
   resources/clj-kondo.exports/io.github.hive-agi/hive-mcp/config.edn, which
   every CONSUMER receives, while this repo itself lints through the inline
   .clj-kondo/config.edn. clj-kondo MERGES configs and both declare the same
   :hooks/:analyze-call keys, so when they disagree the merge order decides
   which hook runs — silently. That is exactly how the repo came to lint
   hive-mcp.dns.result/let-ok through hooks.hive-dsl.result while consumers
   received hive-mcp.hooks.result, two separate implementations of one hook
   that nobody compared.

   These tests are the comparison. They read the two files as EDN and assert
   they agree on every hooked call, that each hook target namespace exists as
   a file clj-kondo can load, and that no hooked call is modelled two
   different ways (hook in one config, :lint-as in the other)."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]))

(def ^:private repo-root
  (System/getProperty "user.dir"))

(def ^:private inline-config-path
  ".clj-kondo/config.edn")

(def ^:private export-config-path
  "resources/clj-kondo.exports/io.github.hive-agi/hive-mcp/config.edn")

(defn- read-config
  "Parse a repo-relative clj-kondo config, or nil when it is absent."
  [path]
  (let [f (io/file repo-root path)]
    (when (.isFile f)
      (edn/read-string (slurp f)))))

(defn- hooked-calls
  "call-symbol -> hook-symbol, the :hooks/:analyze-call map of a config."
  [config]
  (or (get-in config [:hooks :analyze-call]) {}))

(defn- lint-as
  "call-symbol -> core-symbol, the :lint-as map of a config."
  [config]
  (or (:lint-as config) {}))

(defn- clj-files-under
  "Every .clj file at or below `dir`."
  [^java.io.File dir]
  (when (.isDirectory dir)
    (filter #(and (.isFile ^java.io.File %)
                  (str/ends-with? (.getName ^java.io.File %) ".clj"))
            (file-seq dir))))

(defn- declared-hook-namespaces
  "The namespaces every hook file shipped with this repo declares.

   Resolved by reading each file's `ns` form rather than by guessing the path
   kondo maps a namespace onto: hooks legitimately live under .clj-kondo/hooks,
   under a bare .clj-kondo/<ns-path>, and inside each imports/<org>/<dep>
   config dir, and the namespace is what kondo actually resolves."
  []
  (let [root (io/file repo-root ".clj-kondo")]
    (into #{}
          (keep (fn [^java.io.File f]
                  (let [ns-name (->> (slurp f)
                                     (re-find #"(?m)^\(ns\s+([^\s\(\)\[\]\{\}]+)")
                                     second)]
                    (some-> ns-name symbol))))
          (clj-files-under root))))

(defn- resolves?
  "True when some hook file shipped with this repo declares the namespace."
  [hook-sym]
  (contains? (declared-hook-namespaces) (some-> hook-sym namespace symbol)))

(deftest both-configs-exist
  (testing "the repo both lints and exports a clj-kondo config"
    (is (some? (read-config inline-config-path))
        (str inline-config-path " must exist: it is what CI lints through"))
    (is (some? (read-config export-config-path))
        (str export-config-path " must exist: it is what every consumer receives"))))

(deftest inline-and-export-agree-on-hooked-calls
  (testing "no call is hooked differently by the inline config than by the export"
    (let [inline (hooked-calls (read-config inline-config-path))
          export (hooked-calls (read-config export-config-path))
          shared (set/intersection (set (keys inline)) (set (keys export)))
          divergent (into (sorted-map)
                          (for [call shared
                                :when (not= (get inline call) (get export call))]
                            [call {:inline (get inline call) :export (get export call)}]))]
      (is (empty? divergent)
          (str "the same call is mapped to two different hooks — the repo lints "
               "through one while consumers get the other: " (pr-str divergent))))))

(deftest every-exported-hook-is-hooked-inline
  (testing "the export may not hook a call the repo's own lint leaves unmodelled"
    (let [inline (hooked-calls (read-config inline-config-path))
          export (hooked-calls (read-config export-config-path))
          missing (->> (keys export) (remove #(contains? inline %)) sort vec)]
      (is (empty? missing)
          (str "hooked by the export but not inline, so the repo lints these "
               "trees without the hook consumers rely on: " (pr-str missing))))))

(deftest no-call-is-modelled-twice
  (testing "a call is a hook or a :lint-as, never both"
    (doseq [path [inline-config-path export-config-path]
            :let [config (read-config path)
                  both (->> (keys (hooked-calls config))
                            (filter #(contains? (lint-as config) %))
                            sort
                            vec)]
            :when config]
      (is (empty? both)
          (str path " maps these calls as BOTH a hook and :lint-as, so which "
               "one kondo honours is an implementation detail: " (pr-str both))))))

(deftest exported-hook-targets-resolve
  (testing "every hook the export names is a namespace that ships with hive-mcp"
    (let [export (hooked-calls (read-config export-config-path))
          targets (->> (vals export) distinct sort)
          unresolved (vec (remove resolves? targets))]
      (is (empty? unresolved)
          (str "the export names hook namespaces no shipped file provides, so a "
               "consumer gets an unresolvable hook: " (pr-str unresolved))))))

(deftest inline-hook-targets-resolve
  (testing "every hook the repo lints through is loadable"
    (let [inline (hooked-calls (read-config inline-config-path))
          targets (->> (vals inline) distinct sort)
          unresolved (vec (remove resolves? targets))]
      (is (empty? unresolved)
          (str "the inline config names hook namespaces no file provides: "
               (pr-str unresolved))))))

(deftest let-ok-has-one-definition
  (testing "the hook for a re-exported macro points at the namespace that defines it"
    ;; hive-mcp.dns.result is a pure re-export shim of hive-dsl.result, so the
    ;; hook must be hive-dsl's own — not a second implementation kept here.
    (doseq [path [inline-config-path export-config-path]
            :let [call 'hive-mcp.dns.result/let-ok
                  target (get (hooked-calls (read-config path)) call)]]
      (is (= 'hive-dsl.hooks.result/let-ok target)
          (str path " must hook " call " at hive-dsl.hooks.result/let-ok, the "
               "single definition hive-dsl ships; got " (pr-str target))))))
