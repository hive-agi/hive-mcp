(ns hive-mcp.events.wrap-stats-trifecta-test
  "Golden, generative and mutation coverage for the wrap notification delta."
  (:require [clojure.test.check.generators :as gen]
            [hive-mcp.events.handlers.crystal :as handler]
            [hive-mcp.tools.crystal :as tools]
            [hive-test.trifecta :refer [deftrifecta]]))

(defn notification [input]
  (tools/wrap-notify-data "ling" (:harvested input) (:result input)
                          "hive-mcp" (:stats input)))

(defn notify-effects [input]
  (handler/handle-crystal-wrap-notify {} [:crystal/wrap-notify input]))

(def ^:private gen-notification
  (gen/let [ids (gen/vector (gen/choose 1 1000) 0 8)
            sub-ids (gen/vector (gen/choose 1001 2000) 0 3)]
    {:harvested {:memory-ids-created (mapv (fn [n] {:id (str "m-" n)}) ids)}
     :result {:session "s" :sub-summaries (mapv (fn [n] {:summary-id (str "s-" n)}) sub-ids)}
     :stats {:created-count (count ids)}}))

(deftrifecta notification-created-ids
  hive-mcp.events.wrap-stats-trifecta-test/notification
  {:golden-path "test/golden/hive-mcp/wrap-notification.edn"
   :cases {:single {:harvested {:memory-ids-created [{:id "m-1"} {:id "m-2"}]}
                    :result {:session "s" :summary-id "s-1"}
                    :stats {:created-count 2}}
           :multiple {:harvested {:memory-ids-created [{:id "m-1"} {:id "m-1"}]}
                      :result {:session "s" :summary-id "s-2"
                               :sub-summaries [{:summary-id "s-1"} {:summary-id "s-2"}]}
                      :stats {}}
           :failed-store {:harvested {:memory-ids-created []}
                          :result {:session "s" :summary-id nil}
                          :stats {}}}
   :gen gen-notification
   :pred (fn [output]
           (and (= (:wrapped (:stats output)) (count (:created-ids output)))
                (= (count (:created-ids output)) (count (distinct (:created-ids output))))
                (every? string? (:created-ids output))))
   :num-tests 100
   :mutations [["summary-only" (fn [input]
                                  {:created-ids (some-> (get-in input [:result :summary-id]) vector)
                                   :stats (:stats input)})]]})

(def ^:private gen-handler-input
  (gen/let [ids (gen/vector (gen/choose 1 1000) 0 8)]
    {:agent-id "ling" :session-id "s" :project-id "hive-mcp"
     :created-ids (mapv #(str "m-" %) ids) :stats {:created-count (count ids)}}))

(deftrifecta notify-projects-wrapped-disposition
  hive-mcp.events.wrap-stats-trifecta-test/notify-effects
  {:golden-path "test/golden/hive-mcp/wrap-notify-effects.edn"
   :cases {:two {:agent-id "ling" :session-id "s" :project-id "hive-mcp"
                 :created-ids ["m-1" "m-2"] :stats {:created-count 2}}
           :empty {:agent-id "ling" :session-id "s" :project-id "hive-mcp"
                   :created-ids [] :stats nil}}
   :xf (fn [effects] {:wrapped (get-in effects [:shout :data :stats :wrapped])
                      :message (get-in effects [:shout :data :message])})
   :gen gen-handler-input
   :pred (fn [effects]
           (let [ids (get-in effects [:wrap-notify :created-ids])
                 n (count (distinct ids))]
             (and (= n (get-in effects [:shout :data :stats :wrapped]))
                  (= n (get-in effects [:wrap-notify :stats :wrapped]))
                  (.contains (get-in effects [:shout :data :message])
                             (str "[wrote " n " · merged 0 · skipped 0]")))))
   :num-tests 100
   :mutations [["forgets-wrapped" (fn [input]
                                     {:wrap-notify {:created-ids (:created-ids input) :stats {}}
                                      :shout {:data {:stats {} :message "Session wrapped"}}})]]})
