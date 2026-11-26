(ns hive-mcp.launch.deps-test
  "hive-mcp.launch.deps: the -Sdeps overlay, the local.deps.edn round trip,
   and the :starter preset resource.

   Six gates:
     1. the ROUND-TRIP LAW as a test.check property over generated dep maps
        (test.check's clojure-test integration is broken under babashka, so the
        property drives quick-check directly, as knobs-test does);
     2. the same law on a FIXTURE shaped like a real personal overlay: 83
        :local/root siblings, 6 :mvn pins, alias :heap, :extra-paths [\"test\"];
     3. preset merge order: the user's coordinate wins on the same lib;
     4. workspace resolution: relative roots join, absolute and no-workspace stay;
     5. empty sections omitted, and the answer is a valid -Sdeps map;
     6. the :starter resource parses equal to the repo's starter.deps.edn.

   Runs under `bb test:launch` and under the JVM suite."
  (:require [clojure.string :as str]
            ;; The starter.deps.edn comparison is a JVM/babashka gate: cljs has
            ;; neither clojure.edn nor a classpath file stream.
            #?@(:cljs []
                :default [[clojure.edn :as edn]
                          [clojure.java.io :as io]])
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check :as tc]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [hive-mcp.launch.deps :as deps]
            [hive-mcp.launch.schema :as schema]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Generators: dependency maps of the three coordinate shapes
;; =============================================================================

(def lib-names
  "Lib symbols in the shape local.deps.edn uses, drawn from a small pool so
   generated maps collide on the same lib often enough for the merge rule and
   the round trip to be exercised."
  '[io.github.hive-agi/hive-carto
    io.github.hive-agi/hive-emacs
    io.github.hive-agi/hive-ds
    hive/ds
    org.clojure/clojure
    metosin/malli
    com.github.typedclojure/typed.clj
    a/b])

(def lib-gen
  "A lib symbol drawn from lib-names."
  (gen/elements lib-names))

(def rel-root-gen
  "A relative :local/root: sibling-directory spellings, some carrying a
   dot-dot segment, which must survive the join uncanonicalized."
  (gen/fmap (fn [[n nm]] (str "../" nm "-" n))
            (gen/tuple (gen/choose 1 99)
                       (gen/elements ["hive" "acme" "ref"]))))

(def abs-root-gen
  "An absolute :local/root."
  (gen/fmap (fn [[d n]] (str "/" d "/" n))
            (gen/tuple (gen/elements ["opt" "srv" "home"])
                       (gen/elements ["hive-carto" "acme-rs"]))))

(def version-gen
  "An :mvn/version pin."
  (gen/elements ["0.1.0" "0.2.2" "1.12.6"]))

(def sha-gen
  "A :git/sha."
  (gen/fmap (fn [n] (str "dfb30dd" n)) (gen/choose 0 9)))

(def coord-gen
  "A coordinate of one of the three shapes the round trip must carry: a local
   root (relative or absolute), an :mvn/version pin, or a :git/sha."
  (gen/one-of [(gen/fmap (fn [r] {:local/root r}) rel-root-gen)
               (gen/fmap (fn [r] {:local/root r}) abs-root-gen)
               (gen/fmap (fn [v] {:mvn/version v}) version-gen)
               (gen/fmap (fn [s] {:git/url "https://example.invalid/x.git"
                                  :git/sha s})
                         sha-gen)]))

(def deps-map-gen
  "A map of lib symbol to coordinate, as a local.deps.edn :deps holds."
  (gen/map lib-gen coord-gen {:max-elements 6}))

(def alias-gen
  "An alias map with deps.edn-shaped values."
  (gen/map (gen/elements [:heap :dev :test :mcp])
           (gen/one-of [(gen/return {:jvm-opts ["-Xmx4g"]})
                        (gen/return {:extra-paths ["test"]})
                        (gen/return {:main-opts ["-m" "hive-mcp.server.core"]})])
           {:max-elements 3}))

(def local-deps-gen
  "A parsed local.deps.edn with the three overlay sections, plus a :paths and a
   key the importer must drop."
  (gen/fmap (fn [[d a extra]]
              (cond-> {:paths ["src"]
                       :mystery-key :dropped}
                (seq d) (assoc :deps d)
                (seq a) (assoc :aliases a)
                (seq extra) (assoc :extra-paths extra)))
            (gen/tuple deps-map-gen
                       alias-gen
                       (gen/vector (gen/elements ["test" "dev" "resources"])
                                   0 3))))

(def canonical
  "M's three overlay sections, each kept only when non-empty.

   The round-trip law is stated against this normalization: an explicitly EMPTY
   :deps, :aliases or :extra-paths is dropped by the importer and by the
   overlay alike, so it is absent on both sides rather than present as {}."
  (fn [m]
    (cond-> {}
      (seq (:deps m)) (assoc :deps (:deps m))
      (seq (:aliases m)) (assoc :aliases (:aliases m))
      (seq (:extra-paths m)) (assoc :extra-paths (vec (:extra-paths m))))))

;; =============================================================================
;; 1. The round-trip law, as a property
;; =============================================================================

(deftest round-trip-law-is-a-property
  (testing "for any dep map, overlay(import(m)) is the three sections of m"
    (let [result
          (tc/quick-check
           200
           (prop/for-all [m local-deps-gen]
             (= (deps/overlay (deps/import-local-deps m) [])
                (select-keys m [:deps :aliases :extra-paths]))))]
      (is (:pass? result) (pr-str result))
      (is (= 200 (:num-tests result))
          "the property runs its full 200 trials")))

  (testing "and it holds on any m once an empty section is normalized away"
    (let [result
          (tc/quick-check
           200
           (prop/for-all [[d a extra] (gen/tuple deps-map-gen
                                                 alias-gen
                                                 (gen/vector
                                                  (gen/elements ["test" "dev"])
                                                 0 3))
                          empty-deps? gen/boolean
                          empty-aliases? gen/boolean]
             (let [m (cond-> {:paths ["src"]}
                       (or (not empty-deps?) (seq d))
                       (assoc :deps (if empty-deps? {} d))

                       (or (not empty-aliases?) (seq a))
                       (assoc :aliases (if empty-aliases? {} a))

                       true
                       (assoc :extra-paths extra))]
               (= (deps/overlay (deps/import-local-deps m) [])
                  (canonical m)))))]
      (is (:pass? result) (pr-str result)))))

(deftest round-trip-law-never-leaks-foreign-keys
  (testing "a local.deps.edn :paths and any unknown key never reach the overlay"
    (let [m {:deps {'a/b {:mvn/version "1"}}
             :aliases {:heap {:jvm-opts ["-Xmx4g"]}}
             :extra-paths ["test"]
             :paths ["src"]
             :mystery-key :dropped}]
      (is (= {:deps {'a/b {:mvn/version "1"}}
              :aliases {:heap {:jvm-opts ["-Xmx4g"]}}
              :extra-paths ["test"]}
             (deps/overlay (deps/import-local-deps m) [])))
      (is (not (contains? (deps/import-local-deps m) :paths)))
      (is (not (contains? (deps/import-local-deps m) :mystery-key))))))

(deftest import-is-total
  (testing "a non-map answers {}, and never throws"
    (doseq [x [nil 1 "str" [] :kw #{1}]]
      (is (= {} (deps/import-local-deps x)) (pr-str x))))

  (testing "empty sections are omitted, so the fragment validates"
    (doseq [m [{:deps {}} {:aliases {}} {:extra-paths []}
               {:deps {} :aliases {} :extra-paths []}]]
      (is (= {} (deps/import-local-deps m)) (pr-str m))))

  (testing "the fragment it answers is always a valid :launch section"
    (let [result
          (tc/quick-check
           100
           (prop/for-all [m local-deps-gen]
             (m/validate schema/LaunchConfig (deps/import-local-deps m))))]
      (is (:pass? result) (pr-str result)))))

;; =============================================================================
;; 2. The fixture shaped like a real personal overlay
;; =============================================================================

(def sibling-names
  "83 sibling repositories, the count and shape the live local.deps.edn carries."
  ["acme-dart" "acme-go" "acme-rs" "acme-smells" "acme-zig"
   "agent-interop-contracts" "agentic-protocols" "autonomous"
   "basic-tools-mcp" "bb-build" "bb-db-backup" "bb-depsolve" "bb-mcp"
   "case-studies" "clj-ddd-example" "clj-kondo-mcp" "clj-qdrant"
   "cloffeine" "clojure-chroma-client" "clojure-lsp" "clojure-mcp-light"
   "claude-agent-sdk-python" "claude-mem" "cljs-ajax" "conjure-ref"
   "datahike" "docs" "fp-patterns-lab" "hasch" "haystack" "helix"
   "hicada" "hive-addon" "hive-addon-wasm" "hive-agent" "hive-ansatz"
   "hive-artcraft" "hive-assay" "hive-bambu" "hive-blender" "hive-browser"
   "hive-build" "hive-cache" "hive-candidate" "hive-carto" "hive-chat-backend"
   "hive-chroma" "hive-chroma-client" "hive-claw" "hive-cljs"
   "hive-cloak" "hive-compose" "hive-connectors" "hive-contracts"
   "hive-creator" "hive-datahike" "hive-datalevin" "hive-datascript"
   "hive-deepseek" "hive-desk" "hive-di" "hive-dirge" "hive-domain"
   "hive-dsl" "hive-dvergr" "hive-eca" "hive-elm" "hive-emacs"
   "hive-event-protocols" "hive-events" "hive-fosfora" "hive-fressian"
   "hive-gimp" "hive-git" "hive-guard" "hive-guard-shapes" "hive-heap"
   "hive-html" "hive-ingest-kit" "hive-ingestor" "hive-kdenlive" "hive-keg"
   "hive-knowledge"])

(def mvn-pins
  "6 :mvn pins, the same set the live local.deps.edn carries."
  {'org.bytedeco/javacv {:mvn/version "1.5.9"}
   'org.bytedeco/javacpp {:mvn/version "1.5.9"}
   'org.bytedeco/ffmpeg {:mvn/version "6.0-1.5.9"}
   'org.typedclojure/typed.clj {:mvn/version "1.0.24"}
   'io.github.hive-agi/konserve {:mvn/version "0.6.5"}
   'org.babashka/sci {:mvn/version "0.15.59"}})

(defn- sibling-deps
  "One lib symbol per sibling name, each a relative :local/root beside the
   checkout."
  []
  (into {}
        (map (fn [n]
               [(symbol (str "io.github.hive-agi/" n))
                {:local/root (str "../" n)}]))
        sibling-names))

(def personal-fixture
  "A personal overlay shaped like the real local.deps.edn: 83 :local/root
   siblings, 6 :mvn pins, alias :heap, :extra-paths [\"test\"], and a :paths
   that must not cross into the overlay.

   Written inline on purpose: a test that read the real file would fail on
   every machine but this one, and the file is gitignored."
  {:deps (merge (sibling-deps) mvn-pins)
   :aliases {:heap {:jvm-opts ["-Xmx8g" "-XX:+UseSerialGC"]}}
   :extra-paths ["test"]
   :paths ["src"]})

(deftest fixture-has-the-measured-shape
  (testing "83 local roots, 6 mvn pins, one alias, :extra-paths [\"test\"]"
    (let [d (:deps personal-fixture)
          local-roots (filter (fn [entry] (contains? (val entry) :local/root)) d)
          mvn (filter (fn [entry] (contains? (val entry) :mvn/version)) d)]
      (is (= 83 (count local-roots)) "83 siblings, as measured on the live file")
      (is (= 6 (count mvn)) "6 :mvn pins")
      (is (= 89 (count d)) "83 + 6, so no fixture entry is both")
      (is (= {:heap {:jvm-opts ["-Xmx8g" "-XX:+UseSerialGC"]}}
             (:aliases personal-fixture)))
      (is (= ["test"] (:extra-paths personal-fixture)))
      (is (= (count sibling-names) (count (set sibling-names)))
          "the sibling list holds no duplicate, or the 83 above is a lie"))))

(deftest fixture-round-trips-through-the-importer
  (testing "the law holds on the measured-shape fixture, with no workspace set"
    (is (= (select-keys personal-fixture [:deps :aliases :extra-paths])
           (deps/overlay (deps/import-local-deps personal-fixture) []))
        "every relative root is kept as written when :workspace is absent"))

  (testing "and :paths never reaches the overlay"
    (is (not (contains? (deps/overlay (deps/import-local-deps personal-fixture) [])
                        :paths))))

  (testing "the fragment validates against LaunchConfig"
    (is (m/validate schema/LaunchConfig
                    (deps/import-local-deps personal-fixture)))))

(deftest fixture-local-roots-resolve-against-a-workspace
  (testing "with :workspace set, every relative sibling joins onto it"
    (let [cfg (assoc (deps/import-local-deps personal-fixture)
                     :workspace "/home/someone/PP/hive")
          out (deps/overlay cfg [])
          roots (keep (fn [entry] (:local/root (val entry))) (:deps out))]
      (is (= 83 (count roots)) "all 83 siblings carry a root; the 6 pins do not")
      (is (every? (fn [r] (str/starts-with? r "/home/someone/PP/hive/")) roots))
      (is (some #{"/home/someone/PP/hive/../hive-carto"} roots)
          "the .. segment survives the join, uncanonicalized")
      (is (some #{"/home/someone/PP/hive/../acme-rs"} roots))))

  (testing "the mvn pins are untouched"
    (let [out (deps/overlay (assoc (deps/import-local-deps personal-fixture)
                                   :workspace "/ws")
                            [])]
      (is (= mvn-pins (select-keys (:deps out) (keys mvn-pins)))))))

;; =============================================================================
;; 3. Preset merge order: the user wins on the same lib
;; =============================================================================

(deftest preset-merge-order-user-wins
  (testing "a preset lib the user does not touch survives"
    (is (= {:deps {'a/preset {:mvn/version "1"}
                   'a/mine {:mvn/version "1"}}}
           (deps/overlay {:deps {'a/mine {:mvn/version "1"}}}
                         [{:deps {'a/preset {:mvn/version "1"}}}]))))

  (testing "the user's coordinate wins on the same lib"
    (is (= {:deps {'a/shared {:mvn/version "2"}}}
           (deps/overlay {:deps {'a/shared {:mvn/version "2"}}}
                         [{:deps {'a/shared {:mvn/version "1"}}}]))))

  (testing "among presets, the LATER one wins"
    (is (= {:deps {'a/shared {:mvn/version "2"}}}
           (deps/overlay {}
                         [{:deps {'a/shared {:mvn/version "1"}}}
                          {:deps {'a/shared {:mvn/version "2"}}}]))))

  (testing "deps, aliases and extra-paths each follow the order"
    (let [out (deps/overlay {:deps {'x/y {:mvn/version "9"}}
                             :aliases {:heap {:jvm-opts ["-user"]}}
                             :extra-paths ["user-path"]}
                            [{:deps {'x/y {:mvn/version "1"}}
                              :aliases {:heap {:jvm-opts ["-preset"]}
                                        :mcp {:main-opts ["-m" "x"]}}
                              :extra-paths ["preset-path"]}])]
      (is (= {:mvn/version "9"} (get-in out [:deps 'x/y])))
      (is (= {:jvm-opts ["-user"]} (get-in out [:aliases :heap]))
          "aliases merge per alias KEY, so the user alias replaces the preset's")
      (is (= {:main-opts ["-m" "x"]} (get-in out [:aliases :mcp])))
      (is (= ["preset-path" "user-path"] (:extra-paths out))
          "extra-paths concatenate presets-then-user"))))

(deftest preset-three-way-merge
  (testing "starter, then a personal preset, then the user: last writer per lib"
    (is (= {:deps {'s/one {:mvn/version "1"}
                   'p/two {:mvn/version "1"}
                   'u/three {:mvn/version "1"}
                   'shared/lib {:mvn/version "user"}}}
           (deps/overlay {:deps {'shared/lib {:mvn/version "user"}
                                 'u/three {:mvn/version "1"}}}
                         [{:deps {'s/one {:mvn/version "1"}
                                  'shared/lib {:mvn/version "starter"}}}
                          {:deps {'p/two {:mvn/version "1"}
                                  'shared/lib {:mvn/version "personal"}}}])))))

(deftest starter-preset-is-a-real-overlay-input
  (testing "the :starter resource merges under a user :deps"
    (let [starter (deps/load-preset :starter)]
      (is (map? starter))
      (is (contains? starter :deps))
      (let [out (deps/overlay {:deps {'a/mine {:mvn/version "1"}}}
                              [starter])]
        (is (contains? (:deps out) 'a/mine))
        (is (= (inc (count (:deps starter))) (count (:deps out))))
        (is (every? (fn [entry] (or (contains? (val entry) :mvn/version)
                                    (contains? (val entry) :local/root)))
                    (:deps out)))))))

;; =============================================================================
;; 4. Workspace resolution
;; =============================================================================

(deftest workspace-resolution
  (testing "a relative root joins onto the workspace, by plain string join"
    (is (= {:local/root "/ws/../hive-carto"}
           (deps/resolve-local-root "/ws" {:local/root "../hive-carto"})))
    (is (= {:local/root "/ws/sub/dir"}
           (deps/resolve-local-root "/ws" {:local/root "sub/dir"}))))

  (testing "a trailing slash on the workspace does not double up"
    (is (= {:local/root "/ws/../hive-carto"}
           (deps/resolve-local-root "/ws/" {:local/root "../hive-carto"}))))

  (testing "an absolute root is kept exactly as written"
    (is (= {:local/root "/opt/hive-carto"}
           (deps/resolve-local-root "/ws" {:local/root "/opt/hive-carto"}))))

  (testing "no workspace (nil, absent, blank) keeps the root as written"
    (doseq [ws [nil "" "   "]]
      (is (= {:local/root "../hive-carto"}
             (deps/resolve-local-root ws {:local/root "../hive-carto"}))
          (pr-str ws))))

  (testing "a non-string root is untouched"
    (is (= {:local/root :weird}
           (deps/resolve-local-root "/ws" {:local/root :weird})))
    (is (= {:local/root 42}
           (deps/resolve-local-root "/ws" {:local/root 42}))))

  (testing "a coordinate with no :local/root is untouched"
    (is (= {:mvn/version "1"}
           (deps/resolve-local-root "/ws" {:mvn/version "1"})))
    (is (= {:git/url "u" :git/sha "s"}
           (deps/resolve-local-root "/ws" {:git/url "u" :git/sha "s"}))))

  (testing "a tilde is not absolute here: the shell expands those, not us"
    (is (= {:local/root "/ws/~/x"}
           (deps/resolve-local-root "/ws" {:local/root "~/x"})))))

(deftest workspace-through-the-overlay
  (testing "only relative roots move; the overlay is otherwise identical"
    (let [cfg {:workspace "/ws"
               :deps {'rel/lib {:local/root "../lib"}
                      'abs/lib {:local/root "/srv/lib"}
                      'mvn/lib {:mvn/version "1"}}}]
      (is (= {'rel/lib {:local/root "/ws/../lib"}
              'abs/lib {:local/root "/srv/lib"}
              'mvn/lib {:mvn/version "1"}}
             (:deps (deps/overlay cfg []))))))

  (testing "a preset's relative root resolves against the user's :workspace too"
    (let [out (deps/overlay {:workspace "/ws"}
                            [{:deps {'p/lib {:local/root "../p"}}}])]
      (is (= {:local/root "/ws/../p"} (get-in out [:deps 'p/lib])))))

  (testing "the workspace key itself never reaches the overlay"
    (is (not (contains? (deps/overlay {:workspace "/ws"
                                       :deps {'a/b {:mvn/version "1"}}}
                                      [])
                        :workspace)))))

(deftest absolute-path-predicate
  ;; Private by design: hive-system.path takes path arithmetic over, so this is
  ;; reached through the var object. The assertions are the ones it always had.
  (let [absolute-path? @#'deps/absolute-path?]
    (is (absolute-path? "/a"))
    (is (not (absolute-path? "a")))
    (is (not (absolute-path? "../a")))
    (is (not (absolute-path? "~/a")))
    (is (not (absolute-path? nil)))
    (is (not (absolute-path? "")))))

;; =============================================================================
;; 5. Empty sections omitted, and the answer is a valid -Sdeps map
;; =============================================================================

(deftest empty-sections-are-omitted
  (testing "nothing in, nothing out"
    (is (= {} (deps/overlay {} [])))
    (is (= {} (deps/overlay nil [])))
    (is (= {} (deps/overlay {} [{}])))
    (is (= {} (deps/overlay {:deps {} :aliases {} :extra-paths []} []))))

  (testing "only the non-empty section appears"
    (is (= {:deps {'a/b {:mvn/version "1"}}}
           (deps/overlay {:deps {'a/b {:mvn/version "1"}}
                          :aliases {}
                          :extra-paths []}
                         [])))
    (is (= {:extra-paths ["test"]}
           (deps/overlay {:extra-paths ["test"]} [])))
    (is (= {:aliases {:heap {:jvm-opts ["-X"]}}}
           (deps/overlay {:aliases {:heap {:jvm-opts ["-X"]}}} []))))

  (testing "a nil preset in the list is dropped"
    (is (= {:deps {'a/b {:mvn/version "1"}}}
           (deps/overlay {:deps {'a/b {:mvn/version "1"}}} [nil nil])))))

(deftest overlay-answers-a-valid-sdeps-map
  (testing "only the three sections, each of the shape tools.deps reads"
    (let [out (deps/overlay personal-fixture [])]
      (is (every? #{:deps :aliases :extra-paths} (keys out)))
      (is (map? (:deps out)))
      (is (every? symbol? (keys (:deps out))))
      (is (every? map? (vals (:deps out))))
      (is (every? string? (:extra-paths out))))))

;; =============================================================================
;; 6. Preset resources on the classpath
;; =============================================================================

#?(:cljs nil
   :default
   (do
     (def starter-deps-file
       "The repo's starter.deps.edn, beside deps.edn at the project root.

        Resolved from the source file of THIS namespace rather than from the
        working directory: starter.deps.edn is a project file, not a resource,
        and a test that depended on the cwd would fail from any other
        directory. Path arithmetic only: no regex, no environment variable."
       (-> (io/resource "hive_mcp/launch/deps_test.cljc")
           (io/file)
           (.getParentFile)          ; launch
           (.getParentFile)          ; hive_mcp
           (.getParentFile)          ; test
           (.getParentFile)          ; project root
           (io/file "starter.deps.edn")))

     (deftest starter-preset-equals-starter-deps-edn
       (testing "resources/hive_mcp/launch/presets/starter.edn holds the SAME
                 map as the repo's starter.deps.edn, which bin/hive-mcp-foss
                 still reads"
         (let [preset (deps/load-preset :starter)
               file (io/file starter-deps-file)]
           (is (some? preset)
               "the :starter preset resource resolves on the classpath")
           (is (.isFile file)
               (str "starter.deps.edn is in the project root: " file))
           (when (and preset (.isFile file))
             (is (= (edn/read-string {:default tagged-literal} (slurp file))
                    preset)
                 "the two parse to equal maps; edit them together")))))))

(deftest load-preset-is-name-tolerant
  (testing "a keyword and a string name resolve the same resource"
    (is (= (deps/load-preset :starter) (deps/load-preset "starter"))))

  (testing "an absent preset answers nil rather than throwing"
    (is (nil? (deps/load-preset :nonesuch)))
    (is (nil? (deps/load-preset "nonesuch"))))

  (testing "load-presets drops the absent ones and keeps order"
    (is (= [(deps/load-preset :starter)]
           (deps/load-presets [:nonesuch :starter])))))

(deftest starter-preset-carries-the-measured-addons
  (testing "the 8 coordinates the 2026-09-02 boot measurement confirmed"
    (let [d (:deps (deps/load-preset :starter))]
      (is (= 8 (count d)))
      (is (= #{"0.5.4" "0.1.1" "0.1.19" "0.1.2" "0.6.5" "0.2.5" "0.1.7" "0.3.14"}
             (set (map :mvn/version (vals d)))))
      (is (every? (fn [entry] (and (symbol? (key entry))
                                   (contains? (val entry) :mvn/version)))
                  d)))))
