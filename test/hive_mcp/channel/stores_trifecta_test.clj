(ns hive-mcp.channel.stores-trifecta-test
  "The channel's held stores against their contracts: the conversation inbox,
   the context store, the event bus and server state of `channel.core`, the
   activation seam and the block renderer. Each store is reset around every
   test; extension seams are driven through the real registry."
  (:require [clojure.core.async :as async]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.channel.activation :as activation]
            [hive-mcp.channel.blocks :as blocks]
            [hive-mcp.channel.context-store :as cs]
            [hive-mcp.channel.conversation-inbox :as inbox]
            [hive-mcp.channel.core :as core]
            [hive-mcp.extensions.registry :as ext]
            [hive-schemas.test :as hst]
            [malli.core :as m]
            [malli.generator :as mg]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(use-fixtures :each (fn [f] (inbox/reset-all!) (try (f) (finally (inbox/reset-all!)))))

(defn- out-schema [nsym fsym]
  (-> (m/function-schemas) (get-in [nsym fsym :schema]) m/children second))

;;; ---------------------------------------------------------------------------
;;; conversation inbox
;;; ---------------------------------------------------------------------------

(def ^:private Envelope
  [:map [:event-type [:enum :conversation/tell :conversation/ask :conversation/respond :other]]
   [:from [:enum "a"]]])

(deftest inbox-contract-test
  (testing "a push lands in its section, an unknown event-type in :tell"
    (doseq [env (mg/sample Envelope {:size 8 :seed 7})]
      (is (true? (inbox/push! "ling-a" env))))
    (let [sections (inbox/peek* "ling-a")]
      (is (m/validate inbox/Sections sections))
      (is (= 8 (inbox/pending-count "ling-a")))
      (is (true? (inbox/pending? "ling-a")))
      (is (= 1 (inbox/agent-count)))
      (is (= sections (inbox/drain! "ling-a")) "drain answers what peek saw")
      (is (nil? (inbox/drain! "ling-a")) "and clears it")
      (is (false? (inbox/pending? "ling-a")))))
  (testing "clear answers whether anything was removed"
    (inbox/push! "ling-b" {:event-type :conversation/ask})
    (is (true? (inbox/clear! "ling-b")))
    (is (false? (inbox/clear! "ling-b"))))
  (testing "the per-section cap drops the oldest"
    (let [before (inbox/set-section-cap! 2)]
      (try
        (is (= 2 before))
        (doseq [i (range 5)] (inbox/push! "ling-c" {:event-type :conversation/tell :i i}))
        (is (= [3 4] (mapv :i (:tell (inbox/peek* "ling-c")))))
        (finally (inbox/set-section-cap! inbox/default-section-cap)))))
  (is (= {} (inbox/reset-all!))))

;;; ---------------------------------------------------------------------------
;;; context store
;;; ---------------------------------------------------------------------------

(deftest context-store-contract-test
  (let [id  (cs/context-put! {:k 1} :tags #{"zz-tag" "zz-other"} :ttl-ms 60000)
        ids (cs/context-put-batch! {:a {:data [1 2] :tags #{"zz-tag"}}
                                    :skip {:data nil}
                                    :b {:data "text" :ttl-ms 60000}})]
    (try
      (is (m/validate cs/CtxId id))
      (is (= #{:a :b} (set (keys ids))) "an entry with no data is skipped")
      (let [e (cs/context-get id)]
        (is (m/validate cs/Entry e))
        (is (= 1 (:access-count e)))
        (is (= 2 (:access-count (cs/context-get id))) "each read counts"))
      (is (nil? (cs/context-get "ctx-0-00000000")))
      (is (m/validate [:vector cs/Entry] (cs/context-query :tags #{"zz-tag"})))
      (is (= #{id (:a ids)} (set (map :id (cs/context-query :tags #{"zz-tag"} :limit 10)))))
      (is (m/validate (out-schema 'hive-mcp.channel.context-store 'context-stats) (cs/context-stats)))
      (is (true? (cs/context-evict! (:b ids))))
      (is (false? (cs/context-evict! (:b ids))))
      (is (= 2 (cs/evict-by-tags! #{"zz-tag"})))
      (is (nat-int? (cs/reap-expired!)))
      (finally
        (cs/context-evict! id)
        (doseq [i (vals ids)] (cs/context-evict! i))))))

(deftest reaper-contract-test
  (let [was-running? (some? @cs/reaper-executor)]
    (is (nil? (cs/start-reaper!)))
    (is (nil? (cs/start-reaper!)) "idempotent")
    (when-not was-running? (is (nil? (cs/stop-reaper!))))
    (is (nil? (cs/stop-reaper!)) "idempotent")))

;;; ---------------------------------------------------------------------------
;;; channel core: the bus, and the server state with no server running
;;; ---------------------------------------------------------------------------

(deftest event-bus-contract-test
  (let [ch (core/subscribe! :zz/probe)]
    (is (m/validate core/Chan ch))
    (is (true? (core/publish! {:type :zz/probe :n 1})))
    (is (= {:type :zz/probe :n 1} (first (async/alts!! [ch (async/timeout 2000)])))
        "a fresh subscription sees the next event")
    (is (nil? (core/unsubscribe! :zz/probe ch))))
  (testing "an event without :type is refused by the body, not by the contract"
    (is (thrown? clojure.lang.ExceptionInfo (core/publish! {:n 1})))))

(deftest server-state-contract-test
  (when-not (core/client-count)
    (is (nil? (core/stop-server!)))
    (is (nil? (core/force-stop-server!)))
    (is (nil? (core/broadcast! {:type :zz})))
    (is (nil? (core/server-connected?)))
    (is (nil? (core/client-count)))
    (testing "emit-event! publishes with the string envelope and a keyword :type"
      (let [ch (core/subscribe! :zz/emit)]
        (is (nil? (core/emit-event! :zz/emit {:k "v"})))
        (let [[ev] (async/alts!! [ch (async/timeout 2000)])]
          (is (= "v" (get ev "k")))
          (is (= "emit" (get ev (:type core/envelope-keys))))
          (is (= :zz/emit (:type ev))))
        (core/unsubscribe! :zz/emit ch)))))

;;; ---------------------------------------------------------------------------
;;; activation and blocks, through the real extension registry
;;; ---------------------------------------------------------------------------

(defn- with-extension [k f body]
  (let [before (ext/get-extension k)]
    (try
      (ext/register! k f)
      (body)
      (finally
        (if before (ext/register! k before) (ext/deregister! k))))))

(hst/deftrifecta-from-schema drain-ctx-without-provider-contract
  hive-mcp.channel.activation/drain-ctx
  {:in        [:map [:tool-name {:optional true} [:enum "memory"]]
               [:cues {:optional true} [:set [:enum "kg" "axiom"]]]]
   :out       [:map [:tokens [:set :string]]]
   ;; with no provider registered the ctx is exactly {:tokens cues}
   :rel       (fn [in out]
                (or (some? (activation/provider))
                    (= out {:tokens (or (:cues in) #{})})))})

(deftest drain-ctx-with-provider-test
  (with-extension activation/extension-key
    (fn [_] {:pins ["p" 7] :tokens #{"extra" :kw} :floor-cap 3})
    (fn []
      (is (= {:tokens #{"kg" "extra"} :pins #{"p"} :floor-cap 3}
             (activation/drain-ctx {:cues #{"kg"}})) "a malformed member is dropped")))
  (with-extension activation/extension-key
    (fn [_] (throw (ex-info "boom" {})))
    (fn [] (is (= {:tokens #{"kg"}} (activation/drain-ctx {:cues #{"kg"}})) "a throwing provider degrades"))))

(deftest blocks-contract-test
  (is (= "FRONTIER" (blocks/tag-of :block/frontier)))
  (is (= "X" (blocks/tag-of "x")))
  (with-extension :block/zz-probe
    (fn [{:keys [tool-name]}] (str "seen " tool-name))
    (fn []
      (is (some #{:block/zz-probe} (blocks/emitter-keys)))
      (let [out (blocks/render {:tool-name "memory" :cues #{"seen"}})]
        (is (m/validate blocks/Blocks out))
        (is (some #(= ["ZZ-PROBE" "seen memory"] %) out)))
      (is (= [] (blocks/render {:caller-id "c"} 0)) "no budget, no blocks")
      (is (m/validate blocks/Blocks (blocks/render {:caller-id "c"} 10 true))))))

(deftest stores-schemas-generate-test
  (doseq [s [inbox/Sections cs/CtxId cs/Entry core/Chan core/ServerState blocks/BlockCtx blocks/Blocks]]
    (is (seq (mg/sample s {:size 3})) (pr-str (m/form s)))))
