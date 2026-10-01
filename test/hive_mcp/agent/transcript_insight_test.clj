(ns hive-mcp.agent.transcript-insight-test
  "Pure folds behind transcript list/find/digest.

   Unit:     time parsing, globbing, needle compilation, parsers
   Property: listing selection is a newest-first, capped subset; every find
             hit's snippet contains a match; digest counts agree with entries
   Golden:   the digest of a fixed ling run"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.clojure-test :refer [defspec]]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-test.golden :as golden]
            [hive-mcp.agent.transcript-insight :as ti]))

;; =============================================================================
;; Fixtures
;; =============================================================================

(defn- e [turn ts role content & [calls]]
  (cond-> {:transcript/agent-id "ling" :transcript/turn turn :transcript/timestamp ts
           :transcript/role role :transcript/content content}
    calls (assoc :transcript/tool-calls calls)))

(defn- tc [name args result]
  {:tool-call/name name :tool-call/arguments args :tool-call/result result})

(def run
  [(e 0 1000 :user "fix the parser")
   (e 1 2000 :assistant "reading"
      [(tc "read_file" "{\"path\":\"src/a.clj\"}" "(ns a)")
       (tc "bash" "{\"command\":\"cd /w && git status\"}" "On branch feat/x")])
   (e 2 5000 :assistant ""
      [(tc "file_write" "{\"file_path\":\"src/a.clj\",\"content\":\"(ns a)\"}" "{\"written\":true}")
       (tc "edit" "{\"path\":\"test/a_test.clj\"}" "ok")])
   (e 3 65000 :assistant ""
      [(tc "bash" "{\"command\":\"java -cp x clojure.main -e \\\"(clojure.test/run-tests 'a)\\\"\"}"
           "Testing a\n\nFAIL in (t)\nRan 3 tests containing 7 assertions.\n1 failures, 0 errors.")])
   (e 4 70000 :assistant ""
      [(tc "bash" "{\"command\":\"clojure -T:build jar\"}" "Execution error at build/jar")])
   (e 5 80000 :assistant ""
      [(tc "bash" "{\"command\":\"git commit -m 'fix(a): parse'\"}"
           "[feat/x 1a2b3c4d] fix(a): parse\n 1 file changed")])
   (e 6 90000 :assistant "Done: parser fixed, 3 tests, commit 1a2b3c4d.")])

;; =============================================================================
;; Unit
;; =============================================================================

(deftest parse-since-reads-relative-iso-and-rejects-junk
  (let [now 10000000]
    (is (nil? (ti/parse-since nil now)))
    (is (= (- now 7200000) (ti/parse-since "2h" now)))
    (is (= (- now 1800000) (ti/parse-since "30m" now)))
    (is (= 0 (ti/parse-since "1970-01-01T00:00:00Z" now)))
    (is (= 86400000 (ti/parse-since "1970-01-02" now)))
    (is (= 1790000000000 (ti/parse-since "1790000000000" now)))
    (is (:error (ti/parse-since "yesterday-ish" now)))))

(deftest glob-is-prefix-unless-it-has-wildcards
  (is ((ti/glob->pred "ss-") "ss-transcript"))
  (is (not ((ti/glob->pred "ss-") "x-ss-")))
  (is ((ti/glob->pred "*insight*") "mcp-insight-1"))
  (is (not ((ti/glob->pred "e3-?") "e3-core")))
  (is ((ti/glob->pred "a.b*") "a.bc"))
  (is (not ((ti/glob->pred "a.b*") "axbc")) "glob metachars other than * ? are literal")
  (is ((ti/glob->pred nil) "anything")))

(deftest needles
  (is (:error (ti/compile-needle "")))
  (is (:error (ti/compile-needle "/[/")))
  (is (= :regex (:kind (ti/compile-needle "/ran \\d+/i"))))
  (is (= :substring (:kind (ti/compile-needle "a.b(")))))

(deftest commands-are-classified
  (is (= "test" (ti/classify-command "clojure -M:test -n x")))
  (is (= "git" (ti/classify-command "cd /w && git push -u origin x")))
  (is (= "build" (ti/classify-command "clojure -T:build jar")))
  (is (= "other" (ti/classify-command "ls -la"))))

(deftest parsers
  (is (= [{:branch "main" :sha "abc1234" :subject "init"}]
         (ti/parse-commits "[main (root-commit) abc1234] init\n")))
  (is (= [{:tests 2 :assertions 5 :failures 0 :errors 1}]
         (ti/parse-test-runs "Ran 2 tests containing 5 assertions.\n0 failures, 1 errors."))))

(deftest find-hits-in-content-args-and-results
  (let [hits (ti/find-hits {:agent "ling" :run "p"} run (ti/compile-needle "src/a.clj") {})]
    (is (= #{"assistant"} (set (map :role hits))))
    (is (= #{"read_file" "file_write"} (set (map :tool hits)))))
  (testing "role and tool filters"
    (is (= [5] (map :turn (ti/find-hits {} run (ti/compile-needle "/1a2b3c4d\\]/") {:role "tool"}))))
    (is (empty? (ti/find-hits {} run (ti/compile-needle "parser") {:tool "bash"})))
    (is (= [0 6] (map :turn (ti/find-hits {} run (ti/compile-needle "PARSER") {}))))))

(deftest snippet-is-bounded
  (let [text (str (apply str (repeat 500 "a")) "NEEDLE" (apply str (repeat 500 "b")))
        s    (ti/snippet text 500 506)]
    (is (str/includes? s "NEEDLE"))
    (is (<= (count s) (+ 6 (* 2 ti/snippet-radius) 2)))))

;; =============================================================================
;; Golden
;; =============================================================================

(deftest digest-golden
  (golden/assert-golden "test/golden/transcript-insight-digest.edn"
                        {:digest (ti/digest "ling" run)
                         :row    (ti/digest-row (ti/digest "ling" run))}))

;; =============================================================================
;; Properties
;; =============================================================================

(def gen-row
  (gen/hash-map :agent-id (gen/fmap #(str (rand-nth ["ss-" "e3-" "mcp-"]) %) gen/string-alphanumeric)
                :project-id (gen/elements ["hive-mcp" "dirge" "hive"])
                :modified gen/nat))

(defspec select-listing-is-a-newest-first-capped-subset 200
  (prop/for-all [rows (gen/vector gen-row 0 30)
                 limit (gen/choose 1 10)
                 project (gen/elements [nil "hive-mcp"])
                 since (gen/elements [nil 0 50])]
    (let [out (ti/select-listing rows {:project project :since-ms since :limit limit})]
      (and (<= (count out) limit)
           (every? (set rows) out)
           (= out (sort-by :modified > out))
           (every? #(or (nil? project) (= project (:project-id %))) out)
           (every? #(or (nil? since) (>= (:modified %) since)) out)))))

(def gen-entry
  (gen/fmap (fn [[turn role content]] (e turn turn role content))
            (gen/tuple gen/nat (gen/elements [:user :assistant :tool]) gen/string-ascii)))

(defspec every-find-hit-snippet-contains-a-match 200
  (prop/for-all [entries (gen/vector gen-entry 0 20)
                 q (gen/elements ["a" "b" "x1" "!"])]
    (let [needle (ti/compile-needle q)]
      (every? #(re-find (:pattern needle) (:snippet %))
              (ti/find-hits {} entries needle {})))))

(defspec digest-agrees-with-its-entries 200
  (prop/for-all [entries (gen/vector gen-entry 0 20)]
    (let [d (ti/digest "x" entries)]
      (and (= (count entries) (:entries d))
           (= (ti/max-turn entries) (:turns d))
           (or (empty? entries) (= (:wall-ms d) (- (apply max (map :transcript/timestamp entries))
                                                   (apply min (map :transcript/timestamp entries)))))))))
