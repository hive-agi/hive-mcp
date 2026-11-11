(ns hive-mcp.await.ports
  "Wake bounded context, ports: where events come from and who resumes.

   Two open multimethods (OCP): a new source or a new owner kind is one
   defmethod in its own namespace, never an edit here.

   open-source  dispatches on the subscription :type. It starts delivering
                events for one awaiter by calling (deliver! event) for each
                event and answers a zero-arg close fn (or nil).
   open-resumer dispatches on the owner :kind. It answers a one-arg fn the
                registry calls once with {:awaiter :payload :prompt} when
                the awaiter wakes; its return value is recorded.

   The defaults refuse plainly, so an unknown :type or :kind is a loud
   error at arm time, never a silent wait that nothing can ever wake."
  (:require [malli.core :as m]
            [hive-mcp.await.domain :as domain]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defmulti open-source
  "Start the source for `subscription` of `awaiter`. `deliver!` takes one
   event map {:type kw :data any}. Answer a zero-arg close fn, or nil."
  (fn [subscription _awaiter _deliver!] (:type subscription)))

(defmethod open-source :default
  [subscription _awaiter _deliver!]
  (throw (ex-info (str "await: no source registered for type "
                       (pr-str (:type subscription)))
                  {:error :await/unknown-source
                   :type  (:type subscription)
                   :known (vec (remove #{:default} (keys (methods open-source))))})))

(defmulti open-resumer
  "The resume fn for `owner`: (fn [{:keys [awaiter payload prompt]}] ...)."
  (fn [owner] (:kind owner)))

(defmethod open-resumer :default
  [owner]
  (throw (ex-info (str "await: no resumer registered for owner kind "
                       (pr-str (:kind owner)))
                  {:error :await/unknown-resumer
                   :kind  (:kind owner)
                   :known (vec (remove #{:default} (keys (methods open-resumer))))})))

(defn known-sources
  "Every registered source :type."
  []
  (vec (sort (remove #{:default} (keys (methods open-source))))))

(m/=> known-sources [:=> [:cat] [:vector :keyword]])

(defn known-resumers
  "Every registered owner :kind."
  []
  (vec (sort (remove #{:default} (keys (methods open-resumer))))))

(m/=> known-resumers [:=> [:cat] [:vector :keyword]])

(defn require-ports!
  "Throw the same refusal the defaults throw when `awaiter`'s source :type
   or owner :kind has no registered method, and let the resumer refuse an
   owner it cannot resume (open-resumer is called once and its fn dropped);
   answer `awaiter` otherwise. Opens no source, so it is safe before
   anything is recorded."
  [awaiter]
  (let [t (get-in awaiter [:subscription :type])
        k (get-in awaiter [:owner :kind])]
    (when-not (contains? (methods open-source) t)
      ((get-method open-source :default) (:subscription awaiter) awaiter nil))
    (when-not (contains? (methods open-resumer) k)
      ((get-method open-resumer :default) (:owner awaiter)))
    (open-resumer (:owner awaiter))
    awaiter))

(m/=> require-ports! [:=> [:cat domain/Awaiter] domain/Awaiter])
