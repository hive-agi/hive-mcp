(ns hive-mcp.tools.catchup.caller-persona-test
  "Per-caller persona resolution in catchup.

   A persona lens is registered under the bare agent id; the `:_caller_id` a
   CLI ling stamps is `<agent-id>:<session-id>`. These tests pin the lookup
   order (raw id, then the numeric-suffix prefix) at the pure helper and
   through `handle-native-catchup` driven against an injected in-memory store
   and stub per-caller extensions."
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.tools.catchup :as catchup]
            [hive-mcp.tools.catchup.caller :as caller]))

;; =============================================================================
;; Pure helper
;; =============================================================================

(deftest caller-id-candidates-order
  (testing "numeric session suffix adds the slave-id prefix after the raw id"
    (is (= ["ling-x:12345" "ling-x"] (caller/caller-id-candidates "ling-x:12345")))
    (is (= ["coordinator:12345" "coordinator"] (caller/caller-id-candidates "coordinator:12345"))))
  (testing "only the LAST ':' splits"
    (is (= ["swarm:ling-x:777" "swarm:ling-x"] (caller/caller-id-candidates "swarm:ling-x:777"))))
  (testing "an id without ':' is unchanged"
    (is (= ["ling-x"] (caller/caller-id-candidates "ling-x"))))
  (testing "nil resolves to coordinator"
    (is (= ["coordinator"] (caller/caller-id-candidates nil))))
  (testing "non-numeric, empty suffix or empty prefix yields the raw id only"
    (is (= ["ling-x:ab12cd34"] (caller/caller-id-candidates "ling-x:ab12cd34")))
    (is (= ["ling-x:"] (caller/caller-id-candidates "ling-x:")))
    (is (= [":12345"] (caller/caller-id-candidates ":12345")))))

(deftest resolve-for-caller-first-hit-wins
  (let [calls (atom [])
        f     (fn [lenses] (fn [id pid] (swap! calls conj [id pid]) (get lenses id)))]
    (testing "raw id wins over the prefix"
      (reset! calls [])
      (is (= :raw (caller/resolve-for-caller (f {"ling-x:1" :raw "ling-x" :prefix}) "ling-x:1" "p")))
      (is (= [["ling-x:1" "p"]] @calls)))
    (testing "prefix is consulted when the raw id misses"
      (reset! calls [])
      (is (= :prefix (caller/resolve-for-caller (f {"ling-x" :prefix}) "ling-x:1" "p")))
      (is (= [["ling-x:1" "p"] ["ling-x" "p"]] @calls)))
    (testing "no hit is nil"
      (is (nil? (caller/resolve-for-caller (f {}) "ling-x:1" "p"))))))

;; =============================================================================
;; handle-native-catchup against injected ports
;; =============================================================================

(def ^:private per-caller-keys [:catchup/persona-lens :catchup/bundle-profile])

(defn- isolate-ports
  "Snapshot the memory-store registry and the per-caller extension slots, run
   `t`, then restore both."
  [t]
  (let [stores (mem-proto/registered-stores)
        exts   (into {} (map (juxt identity ext/get-extension)) per-caller-keys)]
    (try
      (mem-proto/reset-registry!)
      (run! ext/deregister! per-caller-keys)
      (t)
      (finally
        (mem-proto/reset-registry!)
        (doseq [[k store] stores] (mem-proto/register-store! k store))
        (doseq [[k f] exts]
          (if f (ext/register! k f) (ext/deregister! k)))))))

(use-fixtures :each isolate-ports)

(defn- entries-for
  [project-id]
  (let [scope-tag (str "scope:project:" project-id)
        mk        (fn [type n]
                    (for [i (range n)]
                      {:id         (str project-id "-" type "-" i)
                       :type       type
                       :content    (str type " " i)
                       :tags       [scope-tag]
                       :project-id project-id
                       :created    (str "2026-09-13T10:0" i ":00Z")}))]
    (vec (concat (mk "axiom" 3) (mk "decision" 5) (mk "convention" 4)))))

(defn- entry-match?
  [{:keys [type project-id project-ids tags]} e]
  (and (or (nil? type) (= type (:type e)))
       (or (nil? project-id) (= project-id (:project-id e)))
       (or (nil? project-ids) (some #{(:project-id e)} project-ids))
       (or (empty? tags) (every? (set (:tags e)) tags))))

(defn- memory-store
  [entries]
  (reify mem-proto/IMemoryStore
    (connect! [_ _] nil)
    (disconnect! [_] nil)
    (connected? [_] true)
    (health-check [_] {:healthy? true})
    (get-entry [_ id] (first (filter #(= id (:id %)) entries)))
    (query-entries [_ opts]
      (vec (take (or (:limit opts) 100) (filter #(entry-match? opts %) entries))))
    (search-similar [_ _ _] [])
    (supports-semantic-search? [_] false)
    (entries-expiring-soon [_ _ _] [])
    (store-status [_] {:backend "caller-persona-test"})))

(defn- lens-registry-provider
  "Stub of the persona-lens provider contract: exact caller-id match only."
  [registry]
  (fn [caller-id _project-id] (get registry caller-id)))

(defn- project-dir!
  [project-id]
  (let [dir (doto (io/file (System/getProperty "java.io.tmpdir")
                           (str "caller-persona-test-" project-id))
              (.mkdirs))]
    (spit (io/file dir ".hive-project.edn") (pr-str {:project-id project-id}))
    (.getAbsolutePath dir)))

(defn- catchup-counts
  "Run catchup for `caller-id` with `lenses` ({caller-id lens}) registered on
   both per-caller seams; return the header block's :counts."
  [caller-id lenses]
  (let [project-id (str "cpt-" (random-uuid))
        dir        (project-dir! project-id)]
    (mem-proto/register-store! :default (memory-store (entries-for project-id)))
    (ext/register! :catchup/bundle-profile
                   (let [p (lens-registry-provider lenses)]
                     (fn [id pid] (some-> (p id pid) (select-keys [:caps])))))
    (ext/register! :catchup/persona-lens (lens-registry-provider lenses))
    (try
      (let [result (catchup/handle-native-catchup {:directory  dir
                                                   :_caller_id caller-id})
            header (->> (if (map? result) [result] result)
                        (map #(json/read-str (:text %) :key-fn keyword))
                        (filter #(= "header" (:_block %)))
                        first)]
        (:counts header))
      (finally
        (run! io/delete-file (reverse (file-seq (io/file dir))))))))

(def ^:private ling-lens {:caps {:decisions 2 :conventions 1}})

(deftest session-suffixed-caller-finds-bare-agent-lens
  (let [counts (catchup-counts "ling-x:12345" {"ling-x" ling-lens})]
    (is (= 2 (:decisions counts)) "decision bucket capped by the ling-x lens")
    (is (= 1 (:conventions counts)) "convention bucket capped by the ling-x lens")
    (is (= 3 (:axioms counts)) "axioms are not capped")))

(deftest raw-caller-id-lens-wins-over-prefix
  (let [counts (catchup-counts "ling-x:12345" {"ling-x:12345" {:caps {:decisions 4 :conventions 3}}
                                               "ling-x"       ling-lens})]
    (is (= 4 (:decisions counts)))
    (is (= 3 (:conventions counts)))
    (is (= 3 (:axioms counts)))))

(deftest coordinator-session-without-lens-is-uncapped
  (let [counts (catchup-counts "coordinator:12345" {})]
    (is (= 5 (:decisions counts)))
    (is (= 4 (:conventions counts)))
    (is (= 3 (:axioms counts)))))

(deftest bare-caller-id-is-unchanged
  (testing "exact bare id still resolves"
    (let [counts (catchup-counts "ling-x" {"ling-x" ling-lens})]
      (is (= 2 (:decisions counts)))
      (is (= 1 (:conventions counts)))))
  (testing "a bare id never falls back to a shorter key"
    (let [counts (catchup-counts "ling" {"ling-x" ling-lens})]
      (is (= 5 (:decisions counts)))
      (is (= 4 (:conventions counts))))))
