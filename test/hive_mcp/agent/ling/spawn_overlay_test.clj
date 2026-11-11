;; SPDX-License-Identifier: AGPL-3.0-or-later
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>

(ns hive-mcp.agent.ling.spawn-overlay-test
  "The spawn-opts overlay seam takes several contributors: the function under
   :spawn/opts-overlay, then every function under a key in the
   `spawn.opts-overlay` namespace, in key-name order. Each sees the opts the
   one before it returned, and a contributor that fails changes nothing."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agent.ling.spawn :as spawn]
            [hive-mcp.extensions.registry :as ext]))

(def ^:private apply-overlay @#'spawn/apply-spawn-overlay)

(def ^:private test-keys
  [:spawn/opts-overlay :spawn.opts-overlay/a-first :spawn.opts-overlay/b-second])

(defn- with-contributors*
  "Register `k->f` for the duration of `body-fn`, restoring what was there."
  [k->f body-fn]
  (let [before (into {} (keep (fn [k] (when-let [f (ext/get-extension k)] [k f])))
                     test-keys)]
    (try
      (doseq [k test-keys] (ext/deregister! k))
      (doseq [[k f] k->f] (ext/register! k f))
      (body-fn)
      (finally
        (doseq [k test-keys] (ext/deregister! k))
        (doseq [[k f] before] (ext/register! k f))))))

(def ^:private ling {:cwd "/tmp/x" :project-id "p"})

(deftest with-no-contributor-only-the-request-key-is-stripped
  (with-contributors* {}
    (fn []
      (is (= {:model "m" :task "t"}
             (apply-overlay {:model "m" :task "t" :spawn/request {:tier "cheap"}}
                            ling))))))

(deftest the-single-key-contributor-still-applies
  (with-contributors*
    {:spawn/opts-overlay (fn [opts ctx] (assoc opts :seen-request (:request ctx)))}
    (fn []
      (is (= {:model "m" :seen-request {:role "reviewer"}}
             (apply-overlay {:model "m" :spawn/request {:role "reviewer"}} ling))))))

(deftest contributors-apply-in-order-and-see-earlier-results
  (with-contributors*
    {:spawn.opts-overlay/b-second (fn [opts _] (update opts :trail conj :b))
     :spawn/opts-overlay          (fn [opts _] (assoc opts :trail [:single]))
     :spawn.opts-overlay/a-first  (fn [opts _] (update opts :trail conj :a))}
    (fn []
      (is (= [:single :a :b]
             (:trail (apply-overlay {:model "m"} ling)))
          "single key first, then the namespaced keys by name"))))

(deftest every-contributor-gets-the-same-request-and-ling-facts
  (let [seen (atom [])]
    (with-contributors*
      {:spawn/opts-overlay         (fn [opts ctx] (swap! seen conj ctx) opts)
       :spawn.opts-overlay/a-first (fn [opts ctx] (swap! seen conj ctx) opts)}
      (fn []
        (apply-overlay {:task "t" :kanban-task-id "k" :spawn/request {:tier "cheap"}}
                       ling)
        (is (= 2 (count @seen)))
        (is (apply = @seen))
        (is (= {:request {:tier "cheap"} :cwd "/tmp/x" :project-id "p"
                :task "t" :kanban-task-id "k"}
               (first @seen)))))))

(deftest a-failing-contributor-changes-nothing-and-the-rest-still-run
  (with-contributors*
    {:spawn/opts-overlay          (fn [_ _] (throw (ex-info "boom" {})))
     :spawn.opts-overlay/a-first  (fn [_ _] nil)
     :spawn.opts-overlay/b-second (fn [opts _] (assoc opts :model "routed"))}
    (fn []
      (is (= {:model "routed" :task "t"}
             (apply-overlay {:model "m" :task "t"} ling))))))
