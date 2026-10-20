(ns hive-mcp.hot.claims
  "Owner-scoped hive-hot dir claims for mounted addons.

   hive-hot's remove-dirs! releases a dir only when no OTHER owner still
   claims it, and never releases a dir the initial init! declared (core). So
   how a host first hands hive-hot its addon dirs decides whether an addon can
   ever be plugged out cleanly:

     - init! with addon dirs makes them CORE: no unmount ever releases them;
     - extend-init! without :owner claims them for an anonymous owner, which
       then keeps every dir an addon releases under its own id.

   The rule here: the baseline (interlock, :since) is established WITHOUT
   addon dirs, and every addon claims its own dirs under its :addon/id, the
   same owner hive-addon's inject!/eject! use.

   Strata:
     pure     — base-opts, owner-claims, claims-report
     boundary — claim! (over an injected extend fn; the caller owns the effect)"
  (:require [hive-mcp.dns.result :as r]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; Pure
;; =============================================================================

;; =============================================================================
;; Value schemas
;; =============================================================================

(def ^:private AddonId [:or {:gen/elements [:hive.a :hive.b "hive.c"]} :keyword :string])

(def ExtendRequest
  "One owner's claim: its dirs, its id, the interlock."
  [:map [:dirs [:vector :string]] [:owner AddonId] [:no-reload [:set :any]]])

(def HotPlan
  "The slice of a hive-addon.hot/plan this namespace reads."
  [:map
   [:hot/no-reload {:optional true} [:maybe [:or [:set :symbol] [:sequential :symbol]]]]
   [:hot/registered {:optional true}
    [:maybe [:sequential [:map
                          [:addon/id AddonId]
                          [:hot/source {:optional true}
                           [:maybe [:map [:hot/source-dir {:optional true}
                                          [:maybe [:string {:gen/elements ["/a/src" "/b/src"]}]]]]]]]]]]])

(def ExtendAnswer
  "What one extend answered: a map carrying :added dirs, or an err Result."
  [:or [:map [:added {:optional true} [:maybe [:sequential :string]]]]
   [:map [:error :keyword] [:message {:optional true} :string]]])

(def ClaimsReport
  [:map {:closed true}
   [:claims [:map-of :string [:vector :string]]]
   [:added [:vector :string]]
   [:errors [:vector :string]]])

(def Extend
  "An extend fn: hive-hot extend-init!, or a stub."
  [:fn {:error/message "an invocable extend fn"
        :gen/elements [(fn [req] {:added (:dirs req)})
                       (fn [_] {:added []})
                       (fn [req] (throw (ex-info (str "extend refused " (:owner req)) {})))]}
   ifn?])

(defn base-opts
  "hive-hot ensure-init! options that establish the baseline and the protocol
   interlock WITHOUT any addon dir (so none becomes core). SINCE, epoch ms, may
   be nil."
  [no-reload since]
  (cond-> {:dirs [] :no-reload (set no-reload)}
    since (assoc :since (long since))))

(m/=> base-opts
      [:=> [:cat [:maybe [:or [:set :any] [:sequential :any]]] [:maybe :int]]
       [:map {:closed true} [:dirs [:= []]] [:no-reload [:set :any]] [:since {:optional true} :int]]])

(defn owner-claims
  "The extend requests for a hive-addon.hot/plan PLAN: one
   {:dirs [dir] :owner addon-id :no-reload set} per registered (reloadable)
   addon, its dir read off the row's :hot/source, in id order. Rows without a
   source dir claim nothing."
  [plan]
  (let [no-reload (set (:hot/no-reload plan))]
    (into []
          (keep (fn [row]
                  (when-let [d (get-in row [:hot/source :hot/source-dir])]
                    {:dirs [(str d)] :owner (:addon/id row) :no-reload no-reload})))
          (sort-by (comp str :addon/id) (:hot/registered plan)))))

(m/=> owner-claims [:=> [:cat HotPlan] [:vector ExtendRequest]])

(defn claims-report
  "Pure. {:claims {owner [dir]} :added [dir] :errors [string]} from the
   requests and what each extend answered (a map, or an err Result)."
  [reqs answers]
  (reduce (fn [acc [{:keys [owner dirs]} ans]]
            (if (r/err? ans)
              (update acc :errors conj (str owner ": " (:message ans)))
              (-> acc
                  (assoc-in [:claims (str owner)] dirs)
                  (update :added into (:added ans)))))
          {:claims (sorted-map) :added [] :errors []}
          (map vector reqs answers)))

(m/=> claims-report
      [:=> [:cat [:sequential [:map [:owner AddonId] [:dirs [:sequential :string]]]] [:sequential ExtendAnswer]]
       ClaimsReport])

;; =============================================================================
;; Boundary
;; =============================================================================

(defn claim!
  "Run EXTEND! (hive-hot extend-init!, or a stub) once per request of
   owner-claims. A throw is folded into that owner's error; never throws."
  [extend! reqs]
  (claims-report reqs (mapv (fn [req]
                              (let [res (r/try-effect (extend! req))]
                                (if (r/err? res) res (:ok res))))
                            reqs)))

(m/=> claim! [:=> [:cat Extend [:sequential [:map [:owner AddonId] [:dirs [:sequential :string]]]]] ClaimsReport])
