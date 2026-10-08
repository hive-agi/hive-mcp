(ns hive-mcp.extensions.dispatch-wrap-test
  "hive-mcp's adapter over hive-addon.hot.drain/dispatch-handler: what it hands
   the library (its registered extension, its MCP error shape) and what a host
   call sees. The gate itself is hive-addon's and tested there; here each test
   binds *dispatcher* over a gate of its own."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-addon.hot.drain :as drain]
            [hive-addon.hot.port :as hport]
            [hive-mcp.extensions.dispatch-wrap :as dw]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.tools.consolidated.hot :as hot]
            [hive-test.trifecta :as tri]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Captured before any mutation rebinds the var root.
(def ^:private real-plug-out-opts hot/plug-out-opts)

(defn- dispatcher-over [gate]
  (fn [] (fn [id h opts] (drain/dispatch-handler id h (assoc opts :gate gate)))))

(defmacro ^:private with-extension [k v & body]
  `(let [prev# (ext/get-extension ~k)]
     (ext/register! ~k ~v)
     (try ~@body
          (finally (if prev# (ext/register! ~k prev#) (ext/deregister! ~k))))))

(deftest a-call-to-an-unmounting-addon-answers-an-mcp-error
  (let [gate (drain/atom-drain-gate)
        ran  (atom 0)]
    (binding [dw/*dispatcher* (dispatcher-over gate)]
      (let [h (dw/wrap-addon-handler "probe.a" (fn [_] (swap! ran inc)))]
        (is (= 1 (h {})))
        (hport/-close! gate "probe.a")
        (let [res (h {})]
          (is (true? (:isError res)))
          (is (str/includes? (:text res) "probe.a is being unmounted")))
        (is (= 1 @ran) "the refused call never ran")))))

(deftest the-registered-extension-is-the-inner-wrap
  (let [gate  (drain/atom-drain-gate)
        seen  (atom nil)]
    (binding [dw/*dispatcher* (dispatcher-over gate)]
      (with-extension :addon/wrap-handler
        (fn [id h] (fn [& a] (reset! seen [id (hport/-in-flight gate id)]) (apply h a)))
        ((dw/wrap-addon-handler "probe.a" identity) {})))
    (is (= ["probe.a" 1] @seen) "the extension runs inside a counted call")))

(deftest without-a-drain-gate-in-hive-addon-only-the-extension-applies
  (binding [dw/*dispatcher* (constantly nil)]
    (let [h (fn [p] p)]
      (is (identical? h (dw/wrap-addon-handler "probe.a" h)))
      (with-extension :addon/wrap-handler (fn [_ h] (fn [p] (h (assoc p :wrapped true))))
        (is (= {:wrapped true} ((dw/wrap-addon-handler "probe.a" h) {})))))))

(tri/deftrifecta plug-out-opts-from-params
  hive-mcp.tools.consolidated.hot/plug-out-opts
  {:golden-path "test/golden/hot/plug-out-opts.edn"
   :cases {:none        {}
           :cascade     {:cascade true}
           :force       {:force true}
           :drain       {:drain_ms 500}
           :all         {:cascade true :force true :drain_ms 2000}
           :bad-drain   {:drain_ms 0}
           :truthy-only {:cascade "yes" :force 1}}
   :mutations
   [;; Drops force, so a stuck call can never be overridden from the tool.
    ["force-dropped" (fn [p] (dissoc (real-plug-out-opts p) :force?))]
    ;; Passes a zero bound through, which would refuse every unmount at once.
    ["any-drain" (fn [{:keys [drain_ms] :as p}]
                   (cond-> (dissoc (real-plug-out-opts p) :drain-ms)
                     (some? drain_ms) (assoc :drain-ms drain_ms)))]
    ;; Truthy non-booleans read as true.
    ["truthy" (fn [{:keys [cascade force]}] {:cascade? (boolean cascade) :force? (boolean force)})]]})
