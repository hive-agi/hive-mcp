(ns hive-mcp.tools.registry-contributed-schema-test
  "The surface external loaders read (`get-advertised-tools`, what bb-mcp
   serves as tools/list) carries what addons contribute to a consolidated
   root: their params and their `<cmd> <verb>` commands.

   Measured 2026-10-01: hive-git contributed `ship`/`belt` with :params to the
   `git` root. The server's own tool table advertised them (make-tool folds
   composite/build-merged-tool), but get-advertised-tools only merged the
   schema-ext registry, so every bb-mcp client got the bare core `command`
   enum and no contributed params."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-mcp.tools.registry :as reg]
            [hive-mcp.extensions.registry :as ext]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- enum-root
  "A consolidated root whose `command` declares an enum, from the real registry."
  []
  (first (filter #(seq (get-in % [:inputSchema :properties "command" :enum]))
                 (reg/get-consolidated-tools))))

(defn- advertised [tool-name]
  (first (filter #(= tool-name (:name %)) (reg/get-advertised-tools))))

(deftest advertised-surface-folds-addon-contributions
  (let [root (:name (enum-root))]
    (is (some? root) "the registry has a consolidated root with a command enum")
    (ext/contribute-commands! root :contributed-schema-test
                              {"zship" {:handler {:flow (fn [_] nil) :watch (fn [_] nil)}
                                        :params  {"zfleet" {:type "boolean" :description "every repo"}
                                                  "zlimit" {:type "integer" :description "rows"}}}})
    (try
      (let [t     (advertised root)
            props (get-in t [:inputSchema :properties])
            enum  (set (get-in props ["command" :enum]))]
        (testing "contributed params are declared with their types"
          (is (= "boolean" (get-in props ["zfleet" :type])))
          (is (= "integer" (get-in props ["zlimit" :type]))))
        (testing "the command enum names the contributed subdomain and each verb"
          (is (every? enum ["zship" "zship flow" "zship watch"])))
        (testing "core commands stay"
          (is (every? enum (get-in (enum-root) [:inputSchema :properties "command" :enum])))))
      (testing "a retraction leaves the surface"
        (ext/retract-commands! root :contributed-schema-test)
        (let [props (get-in (advertised root) [:inputSchema :properties])]
          (is (not (contains? props "zfleet")))
          (is (not-any? #{"zship flow"} (get-in props ["command" :enum])))))
      (finally (ext/retract-commands! root :contributed-schema-test)))))

(deftest compact-projection-keeps-contributed-verbs
  ;; bb-mcp's compact mode copies property VALUES from this surface, so the
  ;; enum it advertises is the one here.
  (let [root (:name (enum-root))]
    (ext/contribute-commands! root :contributed-schema-test
                              {"zship" {:handler {:flow (fn [_] nil)}}})
    (try
      (let [t (first (filter #(= root (:name %))
                             (reg/get-advertised-tools {:compact-schema? true})))]
        (is (some #{"zship flow"} (get-in t [:inputSchema :properties "command" :enum]))))
      (finally (ext/retract-commands! root :contributed-schema-test)))))
