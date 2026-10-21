(ns hive-mcp.tools.catchup.hcr-fixture
  "Fixture world for the catchup HCR-scope tests: memory entries and kanban
   cards spread over `hive-mcp.test.stub.hierarchy/fixture`
   (ws -> {a -> {a1}, b}; far alone), each id naming the visibility case it
   pins, plus `with-world`, which installs the hierarchy and a seeded stub
   memory store.

   Entries carry timestamps relative to now, so the 45-day freshness window of
   the session and wrap branches never ages a fixture out."
  (:require [hive-mcp.protocols.memory :as mem-proto]
            [hive-mcp.test.stub.hierarchy :as hier]
            [hive-mcp.test.stub.memory-store :as stub]
            [hive-mcp.tools.catchup.axiom-cache :as axc]
            [hive-mcp.tools.catchup.bundle-cache :as bc]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- ago [minutes] (str (.minusSeconds (java.time.Instant/now) (* 60 minutes))))
(defn- in-days [days] (str (.plusSeconds (java.time.Instant/now) (* 86400 days))))
(defn- sp [pid] (str "scope:project:" pid))

(def ^:private entry-specs
  "[id type project-id tags extra] — see the ns doc."
  [["ax-global-far"         "axiom"           "far"    ["scope:global"]]
   ["ax-far"                "axiom"           "far"    [(sp "far")]]
   ["ax-ws"                 "axiom"           "ws"     [(sp "ws")]]
   ["ax-b"                  "axiom"           "b"      [(sp "b")]]
   ["ax-global-pid-far-tag" "axiom"           "global" [(sp "far")]]
   ["ax-untagged-global"    "axiom"           "global" []]
   ["ax-untagged-far"       "axiom"           "far"    []]
   ["ax-mixed"              "axiom"           "far"    ["scope:global" (sp "far")]]
   ["cand-far"              "axiom-candidate" "far"    [(sp "far") "axiom-pending"]]
   ["cand-a1"               "axiom-candidate" "a1"     [(sp "a1") "axiom-pending"]]
   ["pr-far"                "principle"       "far"    [(sp "far")]]
   ["pr-global"             "principle"       "far"    ["scope:global"]]
   ["pr-a"                  "principle"       "a"      [(sp "a")]]
   ["prp-far"               "principle"       "far"    ["catchup-priority" (sp "far")]]
   ["prp-ws"                "principle"       "ws"     ["catchup-priority" (sp "ws")]]
   ["dec-a"                 "decision"        "a"      [(sp "a")]]
   ["dec-b-misfiled"        "decision"        "a"      [(sp "b")]]
   ["dec-untagged-a1"       "decision"        "a1"     []]
   ["dec-untagged-b"        "decision"        "b"      []]
   ["conv-prio-far"         "convention"      "global" ["catchup-priority" (sp "far")]]
   ["conv-prio-global"      "convention"      "global" ["catchup-priority" "scope:global"]]
   ["legacy-ax-far"         "convention"      "far"    ["axiom" (sp "far")]]
   ["legacy-ax-ws"          "convention"      "ws"     ["axiom" (sp "ws")]]
   ["ss-far"                "note"            "far"    ["session-summary" (sp "far")]]
   ["ss-a"                  "note"            "a"      ["session-summary" (sp "a")]]
   ["wrap-b"                "note"            "b"      ["wrap-generated" (sp "b")]]
   ["wrap-a1"               "note"            "a1"     ["wrap-generated" (sp "a1")]]
   ["exp-a"                 "note"            "a"      [(sp "a")] :expiring]
   ["exp-far-pierce"        "convention"      "global" ["catchup-priority" (sp "far")] :expiring]
   ["exp-global-pierce"     "convention"      "global" ["catchup-priority" "scope:global"] :expiring]
   ["conv-prio-global-from-far" "convention"  "far"    ["catchup-priority" "scope:global"]]
   ["dec-global-from-b"     "decision"        "b"      ["scope:global"]]
   ["ss-global-from-far"    "note"            "far"    ["session-summary" "scope:global"]]
   ["exp-global-from-far"   "convention"      "far"    ["catchup-priority" "scope:global"] :expiring]])

(defn memory-entries
  "The fixture memory entries, timestamped relative to now."
  []
  (vec (map-indexed
        (fn [i [id type pid tags extra]]
          (cond-> {:id id :type type :project-id pid :tags tags
                   :content (str "content of " id) :created (ago (+ 10 i))}
            (= extra :expiring) (assoc :expires (in-days 3))))
        entry-specs)))

(defn- card [id pid status tags updated]
  {:id id :type "note" :project-id pid
   :tags (into ["kanban" status] tags)
   :content {:task-type "kanban" :title (str "title " id) :status status}
   :updated updated})

(def kanban-cards
  "Fixture kanban cards; :updated orders the todos."
  [(card "k-a-todo-1"         "a"   "todo"   [(sp "a")]                 "2026-10-01T10:00:01Z")
   (card "k-a-todo-2"         "a"   "todo"   [(sp "a")]                 "2026-10-01T10:00:02Z")
   (card "k-a-doing"          "a"   "doing"  [(sp "a")]                 "2026-10-01T10:00:03Z")
   (card "k-a-review"         "a"   "review" [(sp "a")]                 "2026-10-01T10:00:04Z")
   (card "k-a-done"           "a"   "done"   [(sp "a")]                 "2026-10-01T10:00:05Z")
   (card "k-a1-todo"          "a1"  "todo"   [(sp "a1")]                "2026-10-01T10:00:06Z")
   (card "k-a1-done"          "a1"  "done"   [(sp "a1")]                "2026-10-01T10:00:07Z")
   (card "k-ws-todo"          "ws"  "todo"   [(sp "ws")]                "2026-10-01T10:00:08Z")
   (card "k-ws-untagged-todo" "ws"  "todo"   []                         "2026-10-01T10:00:09Z")
   (card "k-b-todo"           "b"   "todo"   [(sp "b")]                 "2026-10-01T10:00:10Z")
   (card "k-a1-misfiled-b"    "a1"  "todo"   [(sp "b")]                 "2026-10-01T10:00:11Z")
   (card "k-far-todo"         "far" "todo"   [(sp "far")]               "2026-10-01T10:00:12Z")
   (card "k-a-session-todo"   "a"   "todo"   [(sp "a") "session-todo"]  "2026-10-01T10:00:13Z")
   (assoc (card "k-a-not-kanban" "a" "todo" [(sp "a")] "2026-10-01T10:00:14Z")
          :tags ["todo" (sp "a")])
   (card "k-a1-tagged-ws"     "a1"  "todo"   [(sp "ws")]                "2026-10-01T10:00:15Z")
   (card "k-global-todo"      "global" "todo" [(sp "global")]           "2026-10-01T10:00:16Z")
   (card "k-global-far-tag"   "global" "todo" [(sp "far")]              "2026-10-01T10:00:17Z")
   (card "k-global-untagged-done" "global" "done" []                    "2026-10-01T10:00:18Z")])

(defn- await-refreshes!
  "Block, up to 5 s, until neither the axiom cache nor the bundle cache has a
   background refresh in flight."
  []
  (let [busy?    #(or (seq @@#'axc/axioms-refreshing) (seq @@#'bc/refreshing))
        deadline (+ (System/currentTimeMillis) 5000)]
    (while (and (busy?) (< (System/currentTimeMillis) deadline))
      (Thread/sleep 10))))

(defn reset-caches!
  "Wait out in-flight cache refreshes, then drop the catchup bundle, content
   and axiom caches."
  []
  (await-refreshes!)
  (bc/reset-cache!)
  (axc/invalidate-axioms-cache!))

(defn with-store
  "Run F with a fresh stub memory store seeded with ENTRIES installed as the
   :default store, caches reset around it."
  [entries f]
  (stub/with-stub-store
    (fn []
      (stub/seed! (mem-proto/get-store) entries)
      (reset-caches!)
      (try (f) (finally (reset-caches!))))))

(defn with-world
  "Run F inside the fixture world: PARENTS (default `hier/fixture`) as the
   project hierarchy and ENTRIES (default `memory-entries`) in a stub store."
  ([f] (with-world hier/fixture (memory-entries) f))
  ([parents entries f]
   (hier/with-hierarchy parents #(with-store entries f))))

(defn with-kanban-store
  "Run F with CARDS seeded into one fresh stub memory store registered as
   both :default and :kanban, the stores FacadeBoardSource reads through
   kanban-facade."
  [cards f]
  (stub/with-stub-store
    (fn []
      (let [s (mem-proto/get-store)]
        (mem-proto/register-store! :kanban s)
        (stub/seed! s cards)
        (f)))))
