(ns hive-mcp.tools.composite-discoverability-test
  "The addon discoverability contract (kanban 20260927234748-2b9b53f8): every
   contributed command carries a purpose, every contributed verb map answers
   `help`, and every routed `<command> <verb>` is advertised. The census arm
   draws its universe from the MOUNTED contributions, never from the schema
   it checks."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-addon.registry.commands :as acmds]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.composite :as composite]
            [hive-schemas.test :refer [deftrifecta-from-schema]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftrifecta-from-schema handler-verb-names composite/handler-verb-names
  {:in  [:maybe [:map-of [:enum :help :land :status :_handler :scan] :int]]
   :out [:maybe [:vector :string]]
   :rel (fn [in out]
          (= out (not-empty (vec (sort (map name (remove #{:_handler} (keys in))))))))
   :num-tests 200})

(def ^:private verb-map-var {:land (fn [_] :l) :help (fn [_] :h)})

(deftest verb-names-read-through-a-var-and-skip-the-default-route
  (is (= ["help" "land"] (composite/handler-verb-names #'verb-map-var)))
  (is (= ["drain"] (composite/handler-verb-names {:_handler (fn [_] :d) :drain (fn [_] :d)})))
  (is (nil? (composite/handler-verb-names (fn [_] :opaque))))
  (is (nil? (composite/handler-verb-names {:_handler (fn [_] :d)}))))

(deftest command-names-never-advertise-the-default-route
  (let [names (composite/contributed-command-names
               {"ss"   {:handler {:_handler (fn [_] :s) :help (fn [_] :h)}}
                "ship" {:handler #'verb-map-var}})]
    (is (= ["ship" "ship help" "ship land" "ss" "ss help"] names))
    (is (not-any? #(str/includes? % "_handler") names))))

(def ^:private tool "discoverability-test-root")

(defn- core-def [t]
  {:name t :consolidated true :description "Core."
   :inputSchema {:type "object"
                 :properties {"command" {:type "string" :enum ["status"]}}}})

(defn- with-contributions [cmds f]
  (ext/contribute-commands! tool :discoverability-test-addon cmds)
  (try (f) (finally (ext/retract-commands! tool :discoverability-test-addon))))

(deftest a-conforming-contribution-has-no-gaps
  (with-contributions
    {"ship" {:handler #'verb-map-var :summary "land branches"}
     "leaf" {:handler (fn [_] :x) :description "Leaf purpose.\nmore"}
     "old"  {:handler (fn [_] :o) :deprecated "ship"}}
    #(is (= [] (composite/discoverability-gaps (core-def tool))))))

(deftest every-gap-kind-is-named-with-addon-and-command
  (with-contributions
    {"mute"  {:handler {:drain (fn [_] :d)}}
     "quiet" {:handler (fn [_] :q)}}
    (fn []
      (let [found (composite/discoverability-gaps (core-def tool))
            gaps  (set (map (juxt :command :gap) found))]
        (is (= #{["mute" :no-summary] ["mute" :no-help] ["quiet" :no-summary]} gaps))
        (is (every? #(= :discoverability-test-addon (:addon %)) found))))))

(deftest a-var-registered-verb-map-reaches-the-enum
  (testing "before handler-verb-names a var-held verb map advertised only its root"
    (with-contributions
      {"ship" {:handler #'verb-map-var :summary "land branches"}}
      (fn []
        (let [enum (set (get-in (composite/build-merged-tool (core-def tool))
                                [:inputSchema :properties "command" :enum]))]
          (is (contains? enum "ship land"))
          (is (contains? enum "ship help")))))))

(deftest census-over-every-mounted-contribution
  (testing "universe: hive-addon's command registry as mounted in this JVM;
            the host-side guarantees (enum + description) hold for every one"
    (with-contributions
      {"census-probe" {:handler {:help (fn [_] :h) :go (fn [_] :g)} :summary "probe"}}
      (fn []
        (let [tools (acmds/contributed-tool-names)]
          (is (seq tools) "non-vacuous universe")
          (doseq [t tools
                  :let [host-gaps (filter (comp #{:unadvertised-verb :not-in-description} :gap)
                                          (composite/discoverability-gaps (core-def t)))]]
            (is (empty? host-gaps) (str t ": " (pr-str host-gaps)))))))))
