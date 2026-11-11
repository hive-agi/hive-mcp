(ns hive-mcp.nats.sense-republish
  "Sixth-sense republish: a ling's terminal and attention senses (completed,
   ask, blocked, error, context-death, truncated, never-started) are also
   published into its PARENT coordinator's HIVE_INBOX inbox, so a parent with
   an armed inbox hears them without polling `ss drain`.

   The inbox a parent reads is its STABLE inbox id (decision
   20261102194055-2dba76dd, Q8), never derived from the session id. A
   coordinator binds its caller id to its inbox with `swarm inbox arm`
   (`bind-inbox!`); a parent that is not a coordinator lane and whose id is
   itself a valid inbox id (a ling parent) reads the inbox of that name.

   Idempotent: every message carries msg id `sense-<sense id>`, so a resend
   inside the stream's duplicate window is stored once, and a republisher
   also remembers the ids it already sent. With no bus (NATS down, no
   connection) a sense is skipped, logged ONCE until the bus comes back.

   Strata (CPPB):
     Value objects  Sense, Message, Outcome, Stats, Status
     Pure           republish-classes, republishable?, msg-id, sense->wire,
                    binding-key, inbox-for, ->message, remember
     Ports          ISenseFeed (where senses come from),
                    IInboxPublisher (where messages go)
     Boundary       bind-inbox!/unbind-inbox!/bindings (the binding table),
                    republisher, republish!, listener, install!, uninstall!,
                    status. Adapters: hive-mcp.nats.sense-adapters."
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [hive-mcp.channel.audience :as audience]
            [hive-mcp.nats.inbox :as inbox]
            [taoensso.timbre :as log]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Value objects
;; =============================================================================

(def Sense
  "A sixth-sense Sense as hive-agent.sixth-sense.schema stores it. Open: the
   republisher reads only these keys."
  [:map
   [:sense/id :string]
   [:sense/class :keyword]
   [:sense/agent {:optional true} [:maybe :string]]
   [:sense/project {:optional true} [:maybe :string]]
   [:sense/parent {:optional true} [:maybe :string]]
   [:sense/text {:optional true} [:maybe :string]]
   [:sense/at {:optional true} [:maybe :int]]
   [:sense/detail {:optional true} [:maybe :map]]])

(def Message
  "What a publisher is handed: hive-mcp.nats.inbox/send! input."
  [:map
   [:id :string]
   [:to inbox/InboxId]
   [:from :string]
   [:body [:map-of :string :any]]
   [:meta [:map-of :keyword :any]]])

(def Outcome
  [:map
   [:outcome [:enum :sent :duplicate :skipped :degraded :failed]]
   [:reason {:optional true} :keyword]
   [:to {:optional true} :string]
   [:id {:optional true} :string]])

(def Stats
  [:map
   [:sent :int] [:duplicate :int] [:skipped :int] [:degraded :int]
   [:failed :int] [:dropped :int]
   [:degraded? :boolean]
   [:last-error {:optional true} [:maybe :string]]])

(def Status
  [:map
   [:installed? :boolean]
   [:feed {:optional true} [:maybe :map]]
   [:bindings [:map-of :string :string]]
   [:stats {:optional true} [:maybe Stats]]])

;; =============================================================================
;; Pure
;; =============================================================================

(def republish-classes
  "The sense classes a parent's inbox receives: what ends a run or asks for
   the parent's attention. Progress, rate-limit and commit senses stay in
   `ss drain` only."
  #{:sense/completed :sense/ask :sense/blocked :sense/error
    :sense/context-death :sense/truncated :sense/never-started})

(defn republishable?
  "Is `sense` one of the `republish-classes`?"
  [sense]
  (and (map? sense) (contains? republish-classes (:sense/class sense))))

(m/=> republishable? [:=> [:cat :any] :boolean])

(defn msg-id
  "The idempotency id of `sense`'s inbox message (Nats-Msg-Id)."
  [sense]
  (str "sense-" (:sense/id sense)))

(m/=> msg-id [:=> [:cat Sense] :string])

(defn- kw-name [k] (subs (str k) 1))

(defn- json-safe
  "`v` with keywords as their full name and anything that is not plain JSON
   data as its string."
  [v]
  (walk/postwalk
   (fn [x]
     (cond (keyword? x) (kw-name x)
           (or (nil? x) (string? x) (number? x) (boolean? x)
               (map? x) (sequential? x) (set? x) (map-entry? x)) x
           :else (str x)))
   v))

(defn sense->wire
  "`sense` as JSON data: the keys of the sixth-sense receptor contract (id,
   class, agent, project, parent, text, at, and detail when present)."
  [sense]
  (cond-> {"id"      (:sense/id sense)
           "class"   (name (:sense/class sense))
           "agent"   (:sense/agent sense)
           "project" (:sense/project sense)
           "parent"  (:sense/parent sense)
           "text"    (:sense/text sense)
           "at"      (:sense/at sense)}
    (seq (:sense/detail sense))
    (assoc "detail" (json-safe (update-keys (:sense/detail sense) kw-name)))))

(m/=> sense->wire [:=> [:cat Sense] [:map-of :string :any]])

(defn- blank->nil [s] (when-not (str/blank? (str s)) (str s)))

(defn binding-key
  "The key a parent id is bound under. A coordinator lane that names a
   session reads as `coordinator:<session>`, so `coordinator:7-hive` and
   `coordinator:7` are one parent; every other id is itself."
  [id]
  (let [s (str id)]
    (if-let [session (audience/coordinator-session s)]
      (str "coordinator:" session)
      s)))

(m/=> binding-key [:=> [:cat :any] :string])

(defn inbox-for
  "The inbox `parent` reads, or nil. A binding wins. Otherwise a parent that
   is no coordinator lane and whose id is a valid inbox id reads the inbox of
   that name; a coordinator that never armed has none."
  [bindings parent]
  (when-let [p (blank->nil parent)]
    (or (get bindings (binding-key p))
        (when (and (not (audience/coordinator-reader? p))
                   (inbox/valid-inbox-id? p))
          p))))

(m/=> inbox-for [:=> [:cat [:maybe [:map-of :string :string]] :any] [:maybe :string]])

(defn ->message
  "The inbox message for `sense` sent `to` an inbox."
  [sense to]
  {:id   (msg-id sense)
   :to   to
   :from "sixth-sense"
   :body (sense->wire sense)
   :meta {:kind   "sense"
          :class  (name (:sense/class sense))
          :agent  (:sense/agent sense)
          :parent (:sense/parent sense)}})

(m/=> ->message [:=> [:cat Sense inbox/InboxId] Message])

(def ^:private remember-cap 1024)

(defn remember
  "`seen` ({:order [id ...] :ids #{id ...}}) after sending `id`, the oldest
   forgotten past `cap` (default 1024)."
  ([seen id] (remember seen id remember-cap))
  ([{:keys [order ids] :or {order [] ids #{}}} id cap]
   (let [order (conj order id)
         ids   (conj ids id)]
     (if (> (count order) cap)
       (let [drop-n (- (count order) cap)]
         {:order (subvec order drop-n)
          :ids   (reduce disj ids (subvec order 0 drop-n))})
       {:order order :ids ids}))))

(m/=> remember [:function
                [:=> [:cat [:maybe :map] :string] :map]
                [:=> [:cat [:maybe :map] :string pos-int?] :map]])

;; =============================================================================
;; Ports
;; =============================================================================

(defprotocol ISenseFeed
  "Where senses come from (hive-agent's sixth-sense, or a test feed)."
  (feed-id [this] "A keyword naming the feed.")
  (subscribe! [this key f]
    "Call (f sense) for every sense stored from now on, under `key` (same
     key replaces). Returns true when attached, false when there is no feed.")
  (unsubscribe! [this key] "Stop calling the fn under `key`. Idempotent."))

(defprotocol IInboxPublisher
  "Where inbox messages go (the hive NATS client, or a test publisher)."
  (publisher-available? [this] "True when a publish can be attempted now.")
  (publish-message! [this message]
    "Publish one Message. Returns {:duplicate? bool ...}; throws on failure."))

;; =============================================================================
;; Boundary: the binding table
;; =============================================================================

(defonce ^:private bindings* (atom {}))

(defn bindings
  "{binding-key inbox-id} bound now."
  []
  @bindings*)

(m/=> bindings [:=> [:cat] [:map-of :string :string]])

(defn bind-inbox!
  "Bind parent `parent-id` (a caller id, e.g. `coordinator:<session>`) to
   inbox `inbox-id`: its lings' senses go there. Returns the binding key."
  [parent-id inbox-id]
  (when-not (inbox/valid-inbox-id? inbox-id)
    (throw (ex-info (str "invalid inbox id " (pr-str inbox-id))
                    {:error :inbox/invalid-id :inbox inbox-id})))
  (let [k (binding-key parent-id)]
    (swap! bindings* assoc k inbox-id)
    k))

(m/=> bind-inbox! [:=> [:cat :string :string] :string])

(defn unbind-inbox!
  "Remove the binding of `parent-id`. Returns nil."
  [parent-id]
  (swap! bindings* dissoc (binding-key parent-id))
  nil)

(m/=> unbind-inbox! [:=> [:cat :any] :nil])

;; =============================================================================
;; Boundary: the republisher
;; =============================================================================

(def Republisher
  [:map
   [:feed [:fn #(satisfies? ISenseFeed %)]]
   [:publisher [:fn #(satisfies? IInboxPublisher %)]]
   [:resolve-inbox ifn?]
   [:execute ifn?]
   [:state* [:fn #(instance? clojure.lang.IAtom %)]]])

(def ^:private zero-stats
  {:sent 0 :duplicate 0 :skipped 0 :degraded 0 :failed 0 :dropped 0
   :degraded? false :last-error nil})

(defn republisher
  "A republisher over `feed` and `publisher`. Options:
     :resolve-inbox  (fn [parent] inbox-or-nil); default reads `bindings`
                     through `inbox-for`
     :execute        (fn [thunk]) -> truthy when accepted; default runs the
                     thunk on the caller's thread. A host passes a bounded
                     executor so the sense collector never waits on NATS."
  ([feed publisher] (republisher feed publisher nil))
  ([feed publisher {:keys [resolve-inbox execute]}]
   {:feed          feed
    :publisher     publisher
    :resolve-inbox (or resolve-inbox #(inbox-for (bindings) %))
    :execute       (or execute (fn [thunk] (thunk) true))
    :state*        (atom {:stats zero-stats :seen nil})}))

(m/=> republisher [:function
                   [:=> [:cat :any :any] Republisher]
                   [:=> [:cat :any :any [:maybe :map]] Republisher]])

(defn- count! [{:keys [state*]} k]
  (swap! state* update-in [:stats k] inc))

(defn- degrade!
  "Mark the bus unavailable; log only on the transition."
  [{:keys [state*] :as r} why]
  (count! r :degraded)
  (let [[old] (swap-vals! state* #(-> % (assoc-in [:stats :degraded?] true)
                                      (assoc-in [:stats :last-error] why)))]
    (when-not (get-in old [:stats :degraded?])
      (log/warn "[sense-republish] parent inboxes unavailable, senses stay in `ss drain`"
                "until the bus is back:" why))))

(defn- recover! [{:keys [state*]}]
  (let [[old] (swap-vals! state* assoc-in [:stats :degraded?] false)]
    (when (get-in old [:stats :degraded?])
      (log/info "[sense-republish] parent inboxes reachable again"))))

(defn republish!
  "Publish `sense` into its parent's inbox through `r`. Never throws.
   -> Outcome: :sent | :duplicate (already sent, here or inside the broker's
   duplicate window) | :skipped (:class, :no-inbox) | :degraded (no bus) |
   :failed (the publish threw)."
  [{:keys [publisher resolve-inbox state*] :as r} sense]
  (let [ok? (republishable? sense)
        to  (when ok?
              (try (resolve-inbox (:sense/parent sense)) (catch Throwable _ nil)))]
    (cond
      (not ok?)
      (do (count! r :skipped) {:outcome :skipped :reason :class})

      (nil? to)
      (do (count! r :skipped) {:outcome :skipped :reason :no-inbox})

      (contains? (get-in @state* [:seen :ids]) (msg-id sense))
      (do (count! r :duplicate) {:outcome :duplicate :to to :id (msg-id sense)})

      (not (try (publisher-available? publisher) (catch Throwable _ false)))
      (do (degrade! r "no connected event bus") {:outcome :degraded :to to})

      :else
      (let [msg (->message sense to)]
        (try
          (let [receipt (publish-message! publisher msg)]
            (recover! r)
            (swap! state* update :seen remember (:id msg))
            (if (:duplicate? receipt)
              (do (count! r :duplicate) {:outcome :duplicate :to to :id (:id msg)})
              (do (count! r :sent) {:outcome :sent :to to :id (:id msg)})))
          (catch Throwable t
            (count! r :failed)
            (degrade! r (or (.getMessage t) (str (class t))))
            {:outcome :failed :to to :id (:id msg)}))))))

(m/=> republish! [:=> [:cat Republisher :any] Outcome])

(defn listener
  "The fn `r` subscribes to its feed: hands each sense to `r`'s executor, so
   the feed's thread only enqueues. A rejected hand-off is counted :dropped."
  [{:keys [execute] :as r}]
  (fn [sense]
    (when-not (try (execute #(republish! r sense)) (catch Throwable _ false))
      (count! r :dropped))
    nil))

(m/=> listener [:=> [:cat Republisher] ifn?])

;; =============================================================================
;; Boundary: the process republisher
;; =============================================================================

(def listener-key ::parent-inboxes)

(defonce ^:private current* (atom nil))

(defn install!
  "Subscribe `r` to its feed as THE process republisher, replacing any
   earlier one. Returns {:installed? bool :feed kw}: false when the feed is
   absent (no sixth-sense in this JVM); idempotent, so call it again later."
  [{:keys [feed] :as r}]
  (let [attached? (boolean (try (subscribe! feed listener-key (listener r))
                                (catch Throwable t
                                  (log/debug t "[sense-republish] feed refused")
                                  false)))
        [old]     (reset-vals! current* (when attached? r))]
    (when (and old (not attached?))
      (try (unsubscribe! (:feed old) listener-key) (catch Throwable _ nil)))
    {:installed? attached? :feed (feed-id feed)}))

(m/=> install! [:=> [:cat Republisher] [:map [:installed? :boolean] [:feed :any]]])

(defn installed?
  "Is a process republisher subscribed now?"
  []
  (some? @current*))

(m/=> installed? [:=> [:cat] :boolean])

(defn uninstall!
  "Unsubscribe the process republisher. Idempotent. Returns nil."
  []
  (when-let [{:keys [feed]} (first (reset-vals! current* nil))]
    (try (unsubscribe! feed listener-key) (catch Throwable _ nil)))
  nil)

(m/=> uninstall! [:=> [:cat] :nil])

(defn status
  "Is a republisher installed, through which feed, the binding table, and
   what it has done."
  []
  (let [r @current*]
    (cond-> {:installed? (some? r) :bindings (bindings)}
      r (assoc :feed {:id (feed-id (:feed r)) :attached? true}
               :stats (:stats @(:state* r))))))

(m/=> status [:=> [:cat] Status])
