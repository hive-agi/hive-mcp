(ns hive-mcp.extensions.registry-delegation-test
  "An addon depends on hive-di / hive-contracts / hive-addon, never on hive-mcp
   core, so hive-addon.registry.commands is the seam that owns command
   contributions and this host namespace is only a facade over it.

   While the host kept its own atom the two were separate implementations of one
   concept, and the failure that hid it was silent: an addon that migrated to the
   correct seam still mounted and still reported :status :ok, then vanished from
   the tool surface because nothing on the host side ever read hive-addon's
   store. These tests pin that ONE store backs both names, in both directions."
  (:require [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.registry.commands :as addon-cmds]
            [hive-mcp.extensions.registry :as registry]
            [hive-mcp.extensions.lifecycle :as lifecycle]
            [hive-mcp.tools.composite :as composite]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private tool "analysis")

(defn- reset-world! []
  (addon-cmds/clear!)
  (registry/clear-all!))

(use-fixtures :each (fn [t] (reset-world!) (t) (reset-world!)))

(deftest a-host-contribution-lands-in-the-addon-store
  (registry/contribute-commands! tool :kondo {"lint" {:handler identity :description "d"}})
  (is (= ["lint"] (keys (addon-cmds/get-commands tool)))
      "the host must not keep a second store the addon seam cannot see")
  (is (= :kondo (:addon (get (addon-cmds/get-commands tool) "lint")))
      "the contributing addon is stamped, so shutdown can retract exactly its own"))

(deftest an-addon-contribution-reaches-the-hosts-own-readers
  ;; Asserting through addon-cmds on BOTH sides would only prove hive-addon
  ;; agrees with itself. The question this test exists to answer is whether
  ;; the HOST sees a contribution placed at the addon seam, so it reads back
  ;; through a host consumer -- composite/build-composite-handlers, which is
  ;; what the advertised tool surface is actually built from.
  (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
  (let [handlers (composite/build-composite-handlers tool)]
    (is (contains? handlers :build)
        "this is the migration that used to mount green and then vanish")
    (is (contains? handlers :help)
        "and the host's own :help is folded in beside it, which is why this
         reads through the host rather than back through hive-addon")))

(deftest both-seams-accumulate-into-one-tree
  (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
  (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
  (let [handlers (composite/build-composite-handlers tool)]
    (is (every? (partial contains? handlers) [:build :lint])
        "a half-migrated fleet must see every command, whichever seam placed it,
         and the host reader is where that has to be true")))

(deftest retraction-reaches-across-the-facade
  (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
  (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
  (testing "the host retracts only the named addon's commands"
    (registry/retract-commands! tool :kondo)
    (is (= ["build"] (keys (addon-cmds/get-commands tool)))))
  (testing "retract-all-by-addon! clears the rest"
    (registry/retract-all-by-addon! :cljs)
    (is (empty? (addon-cmds/get-commands tool)))))

(deftest listeners-still-fire-around-the-delegated-store
  (let [events (atom [])]
    (registry/add-contribution-listener! ::probe (fn [e] (swap! events conj e)))
    (try
      (registry/contribute-commands! tool :kondo {"lint" {:handler identity}})
      (registry/retract-all-by-addon! :kondo)
      (is (= [:contribute :retract] (mapv :type @events))
          "notification is the host's own contribution on top of the seam;
           hive-addon does not own it and must not silently drop it")
      (is (= [tool tool] (mapv :tool-name @events))
          "retract-all-by-addon! reads the touched set BEFORE retracting, or it
           has nothing left to attribute the notification to")
      (finally (registry/remove-contribution-listener! ::probe)))))

(deftest clear-all-empties-the-single-store
  (addon-cmds/contribute! tool :cljs {"build" {:handler identity}})
  (registry/clear-all!)
  (is (empty? (addon-cmds/get-commands tool))
      "clearing the host must not leave contributions stranded in the seam"))
