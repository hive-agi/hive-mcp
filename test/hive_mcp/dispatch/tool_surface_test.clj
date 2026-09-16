(ns hive-mcp.dispatch.tool-surface-test
  "The ratchet: a tool handler registered BY VALUE cannot pick up a reload of
   its own namespace, so the tool surface must not acquire new ones.

   This is the guard on the conversion. Without it the fleet drifts back one
   tool at a time, because the value form is what every existing example in
   the tree teaches and nothing complains."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.dispatch.handler :as dispatch]
            [hive-mcp.tools.registry :as registry]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private constructed-handlers
  "Tools whose :handler is CONSTRUCTED by a call rather than referenced by
   name, today `(composite/build-merged-handler \"x\" canonical-handlers)`.

   An exemption with a reason, not a skip list. `#'` cannot be written in
   front of a call: there is no var to quote, because the handler is a fresh
   closure built at load time over the `canonical-handlers` map. Making these
   reloadable is a DIFFERENT change from var-quoting a reference, since the
   closure has to resolve the handler map at call time instead. Until that
   lands, these are known-stale on reload and are listed here so the number is
   visible rather than implied.

   `hivemind` is deliberately NOT listed, even though its handler is still a
   value. It is a plain symbol reference and belongs in the converted set; it
   is unconverted only because another session holds that file as in-flight
   WIP, and editing a file someone else is mid-change on trades a reload for a
   merge conflict. Listing it here would be the rot this namespace's last test
   exists to catch, since the reason is temporary and has nothing to do with
   the handler being constructed."
  #{"code" "fs" "git" "hot" "swarm" "web"})

(defn- tool-defs []
  (registry/get-all-tools :include-deprecated? false))

(deftest the-registry-is-populated
  (testing "guard against a vacuous pass: every other test here quantifies over this"
    (is (pos? (count (tool-defs)))
        "an empty registry would make the ratchet below assert nothing at all")))

(deftest every-referenced-handler-is-registered-by-var
  (testing "a handler named by symbol must be var-quoted, or a reload cannot reach it"
    (doseq [{:keys [name handler]} (tool-defs)
            :when (not (contains? constructed-handlers name))]
      (is (var? handler)
          (str "tool " name " registers its handler by VALUE. "
               "Write :handler #'the-fn so the seam resolves through the var "
               "at call time (20260817195749-0d407e9c). If its handler is "
               "genuinely constructed rather than referenced, add it to "
               "constructed-handlers WITH the reason.")))))

(deftest every-handler-passes-the-dispatch-gate
  (testing "converted or not, every handler must survive the six gates"
    (doseq [{:keys [name handler]} (tool-defs)]
      (is (dispatch/handler? handler)
          (str "tool " name " has a handler the dispatch gates would refuse")))))

(deftest the-exemption-list-does-not-rot
  (testing "an exemption for a tool that no longer exists is a lie about the surface"
    (let [live (into #{} (map :name) (tool-defs))
          stale (remove live constructed-handlers)]
      (is (empty? stale)
          (str "these tools are exempted but not in the registry: "
               (str/join ", " stale)
               ". Remove them from constructed-handlers.")))))
