(ns hive-mcp.dispatch.verbs
  "Verb-level contribution seam for consolidated tool roots.

   A consolidated root dispatches on a static handler map. Before this seam an
   addon could only add whole subdomains beside that map (a shallow merge), so
   moving ONE verb out of core meant leaving a delegate shim behind in core.
   Here an addon contributes a single verb, with its params, under a root, and
   the dispatcher consults the contribution BEFORE the static map.

   A static map opts in through its metadata:

     ::root         the root name contributions are filed under (a string)
     ::provided-by  {verb owner}: verbs this root expects an addon to supply.
                    A static entry for such a verb is a shim that yields to
                    that owner; with no entry and no contribution the verb
                    answers an error naming the owner.

   Resolution rule (`resolve-verb`, pure):
     - a contribution is ADMITTED when the verb is declared for its owner, or
       when the verb is undeclared and the static map has no entry for it
     - an admitted contribution always wins; it never falls through to static
     - a contribution over a core entry it does not own is REFUSED: the core
       entry keeps dispatching and the refusal is reported (and logged once
       at the boundary), never a silent shadowing
     - otherwise static, else :missing (declared, nobody contributed), else
       :unknown

   Stratified: the top half is pure (data in, data out); the registry and its
   listeners are the boundary, at the bottom. Names no domain: any root can
   carry the metadata."
  (:require [hive-mcp.dispatch.handler :as handler]
            [taoensso.timbre :as log]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Pure: resolution
;; =============================================================================

(defn root-of
  "The root name STATIC files contributions under, or nil when it opted out."
  [static]
  (::root (meta static)))

(defn provided-by
  "STATIC's {verb owner} declarations; {} when it declares none."
  [static]
  (or (::provided-by (meta static)) {}))

(defn verb-key
  "VERB as the keyword a handler map is keyed by."
  [verb]
  (if (keyword? verb) verb (keyword (str verb))))

(defn resolve-verb
  "Which handler VERB dispatches to, given the STATIC handler map and the
   CONTRIBUTIONS made under its root ({verb {:handler h :owner o ...}}).

   Returns a map with :source, one of
     :contributed  {:handler :owner}   the contribution, admitted
     :refused      {:handler :owner :refused-owner}  a contribution over a core
                   entry it does not own; :handler is the static entry (nil
                   when there is none) and :owner who keeps the verb
     :static       {:handler}          the static entry
     :missing      {:owner}            declared for an addon, none came
     :unknown      {}                  nobody routes VERB"
  [static contributions verb]
  (let [v        (verb-key verb)
        c        (get contributions v)
        s        (get static v)
        declared (get (provided-by static) v)]
    (cond
      (and c (if declared (= declared (:owner c)) (nil? s)))
      {:source :contributed :verb v :handler (:handler c) :owner (:owner c)}

      c
      {:source :refused :verb v :handler s :owner (or declared :core)
       :refused-owner (:owner c)}

      (some? s) {:source :static :verb v :handler s}
      declared  {:source :missing :verb v :owner declared}
      :else     {:source :unknown :verb v})))

(defn missing-verb-error
  "The MCP error a verb answers when its owner has not contributed it."
  [root verb owner]
  {:type    "text"
   :isError true
   :text    (str "'" root " " (name verb) "' is provided by the addon " owner
                 ", which has not contributed it. Mount or reload " owner
                 " to enable it.")})

(defn- missing-handler
  [root verb owner]
  (fn [_params] (missing-verb-error root verb owner)))

(defn fold-verbs
  "STATIC with CONTRIBUTIONS folded in verb by verb through `resolve-verb`.
   Keeps STATIC's metadata and records every refusal under ::refusals in the
   result's metadata (the boundary logs them). A :missing verb gets a handler
   answering `missing-verb-error`."
  [static contributions]
  (let [root  (root-of static)
        verbs (distinct (concat (keys static) (keys contributions)
                                (keys (provided-by static))))
        rs    (map #(resolve-verb static contributions %) verbs)
        m     (reduce (fn [m {:keys [source verb handler owner]}]
                        (case source
                          (:contributed :static) (assoc m verb handler)
                          :refused (if handler (assoc m verb handler) m)
                          :missing (assoc m verb (missing-handler root verb owner))
                          m))
                      {} rs)]
    (with-meta m (assoc (meta static)
                        ::refusals (filterv #(= :refused (:source %)) rs)))))

;; =============================================================================
;; Boundary: the registry
;; =============================================================================

;; {root {verb {:handler h :owner o :params {...} :description s}}}
(defonce ^:private registry (atom {}))

;; {listener-id f}, called with {:type :contribute-verb|:retract-verb
;;                               :tool-name root :addon-id owner :verb v}
(defonce ^:private listeners (atom {}))

;; #{[root verb refused-owner]} already logged
(defonce ^:private logged-refusals (atom #{}))

(defn add-listener!
  "Call F after every contribution or retraction. Idempotent by id. A listener
   that throws is ignored."
  [listener-id f]
  (swap! listeners assoc listener-id f)
  listener-id)

(defn remove-listener!
  "Drop a listener. Returns the id."
  [listener-id]
  (swap! listeners dissoc listener-id)
  listener-id)

(defn- notify! [event]
  (doseq [[_ f] @listeners]
    (try (f event) (catch Throwable _ nil))))

(defn contributions
  "The verbs contributed under ROOT: {verb {:handler :owner :params}}."
  [root]
  (get @registry root {}))

(defn contribute-verb!
  "Contribute VERB under ROOT. SPEC: {:handler h :owner o :params {...}
   :description s}. Refused, with nothing written, when the handler is not
   invocable, the owner is blank, or another owner already contributed VERB.
   Re-contributing by the same owner replaces (hot reload).
   Returns {:ok? true ...} or {:ok? false :reason kw :message s}."
  [root verb {:keys [handler owner] :as spec}]
  (let [v        (verb-key verb)
        existing (get-in @registry [root v])]
    (cond
      (not (handler/handler? handler))
      {:ok? false :reason :verb/not-invocable
       :message (str root " " (name v) ": handler is not invocable")}

      (not (and (string? owner) (seq owner)))
      {:ok? false :reason :verb/no-owner
       :message (str root " " (name v) ": owner is required")}

      (and existing (not= owner (:owner existing)))
      (do (log/warn "verb contribution refused: already owned"
                    {:root root :verb v :owner (:owner existing) :refused owner})
          {:ok? false :reason :verb/owned-by :owner (:owner existing)
           :message (str root " " (name v) " is already contributed by " (:owner existing))})

      :else
      (do (swap! registry assoc-in [root v]
                 (select-keys spec [:handler :owner :params :description]))
          (notify! {:type :contribute-verb :tool-name root :addon-id owner :verb v})
          {:ok? true :root root :verb v :owner owner}))))

(defn retract-verb!
  "Withdraw VERB under ROOT when OWNER contributed it. Returns true when it did."
  [root verb owner]
  (let [v (verb-key verb)]
    (if (= owner (get-in @registry [root v :owner]))
      (do (swap! registry update root dissoc v)
          (notify! {:type :retract-verb :tool-name root :addon-id owner :verb v})
          true)
      false)))

(defn retract-owner!
  "Withdraw every verb OWNER contributed, under every root. Returns the
   [root verb] pairs withdrawn."
  [owner]
  (let [pairs (vec (for [[root vs] @registry
                         [v spec] vs
                         :when (= owner (:owner spec))]
                     [root v]))]
    (doseq [[root v] pairs] (retract-verb! root v owner))
    pairs))

(defn clear!
  "Drop every contribution. For tests."
  []
  (reset! registry {})
  (reset! logged-refusals #{})
  nil)

(defn- log-refusals! [root refusals]
  (doseq [{:keys [verb owner refused-owner]} refusals
          :let [k [root verb refused-owner]]
          :when (not (contains? @logged-refusals k))]
    (swap! logged-refusals conj k)
    (log/warn "verb contribution refused: core entry not owned by contributor"
              {:root root :verb verb :keeps owner :refused refused-owner})))

(defn effective
  "STATIC with the verbs contributed under its root folded in (`fold-verbs`),
   refusals logged once each. STATIC unchanged when it names no root."
  [static]
  (if-let [root (and (map? static) (root-of static))]
    (let [folded (fold-verbs static (contributions root))]
      (log-refusals! root (::refusals (meta folded)))
      folded)
    static))

(defn effective-tree
  "HANDLERS, a consolidated handler tree, with `effective` applied to the root
   and to each direct subtree that names a root (a domain root folding a
   subdomain: `<root> <subdomain> <verb>`). Subtrees behind vars are read
   through `handler/current`. Metadata of HANDLERS is kept."
  [handlers]
  (let [h (effective handlers)]
    (if (map? h)
      (reduce-kv (fn [m k node]
                   (let [cur (handler/current node)]
                     (if (and (map? cur) (root-of cur))
                       (assoc m k (effective cur))
                       m)))
                 h h)
      h)))

(defn contributed-params
  "Every param the verbs contributed under ROOT declare, merged."
  [root]
  (apply merge (keep :params (vals (contributions root)))))

(defn contributed-verb-names
  "The verbs contributed under ROOT, as sorted strings."
  [root]
  (vec (sort (map name (keys (contributions root))))))
