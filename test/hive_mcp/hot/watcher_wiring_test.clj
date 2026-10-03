(ns hive-mcp.hot.watcher-wiring-test
  "Does the watcher actually RECEIVE the protocol interlock?

   hive-mcp.hot.self-test proves the set is derived correctly. That is a
   different claim from the set reaching hive-hot, and the gap between those
   two claims is exactly where this feature was broken for its whole life:
   `init-with-watcher!` accepted `:no-reload` all along and nobody passed it.
   A test of the derivation alone would have stayed green throughout.

   Contained on purpose. It captures the options rather than starting a real
   watcher, so hive-hot's global registry and clj-reload's baseline are left
   alone and this namespace cannot perturb another suite in the same JVM."
  (:require [clojure.test :refer [deftest is testing]]
            [hive-hot.core :as hot]
            [hive-mcp.protocols.dispatch]
            [hive-mcp.server.init :as init]
            [hive-mcp.hot.core :as hot-core]
            [clojure.string :as str]
            [clojure.java.io :as io]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- captured-watcher-opts
  "Run the real `init-hot-reload-watcher!` with the boundary stubbed, and
   return whatever it handed to `init-with-watcher!`.

   Returns ::never-called when the boundary was not reached at all, which is a
   state the caller MUST check: the function under test wraps its whole body
   in `result/rescue`, so any exception on the way would swallow itself and
   leave a nil-punning assertion looking green."
  [project-config]
  (let [captured (atom ::never-called)]
    (with-redefs [hot/init-with-watcher! (fn [opts] (reset! captured opts) nil)]
      (init/init-hot-reload-watcher! project-config))
    @captured))

(deftest the-watch-dirs-are-absolute-and-anchored-on-core
  (testing "nothing configured: core's classpath roots, as given"
    (is (= ["/r/src"] (init/watch-dirs nil ["/r/src"])))
    (is (= ["/r/src"] (init/watch-dirs [] ["/r/src"]))))
  (testing "a relative configured dir resolves against core's project dir, not the cwd"
    (is (= ["/r/src" "/r/dev"] (init/watch-dirs ["src" "dev"] ["/r/src"]))))
  (testing "an absolute configured dir is kept"
    (is (= ["/elsewhere/src"] (init/watch-dirs ["/elsewhere/src"] ["/r/src"]))))
  (testing "a jar-backed core falls back to ./src made absolute, never a relative \"src\""
    (let [[d & more] (init/watch-dirs nil [])]
      (is (nil? more))
      (is (.isAbsolute (java.io.File. ^String d)))
      (is (.endsWith ^String d "src"))))
  (testing "in this JVM the stock \"src\" names core's own source root"
    (let [roots (hot-core/core-roots)]
      (is (= (mapv str roots) (init/watch-dirs ["src"] roots)))
      (is (.exists (java.io.File. ^String (first (init/watch-dirs ["src"] roots))
                                  "hive_mcp/hot/core.clj"))))))

(deftest the-watcher-is-handed-the-protocol-interlock
  (let [opts (captured-watcher-opts {:hot-reload true})]
    (testing "vacuity guard: the body is rescue-wrapped, so silence is possible"
      (is (not= ::never-called opts)
          "init-with-watcher! was never reached, so nothing below was actually checked"))
    (let [{:keys [dirs no-reload]} opts]
      (testing "it still watches a source tree"
        (is (seq dirs)))
      (testing "and it now carries the interlock, which is the whole fix"
        (is (some? no-reload)
            "no :no-reload means a reload of a protocol namespace orphans every instance built against it")
        (is (pos? (count no-reload))
            "an empty set is the same as no interlock, and passes a some? check")
        (is (contains? no-reload 'hive-mcp.protocols.dispatch)
            "a real core protocol namespace must be in the protected set")))))

(deftest a-disabled-watcher-starts-nothing
  (testing "the off switch still works, interlock or not"
    (is (= ::never-called (captured-watcher-opts {:hot-reload false}))
        "hot-reload false must not reach init-with-watcher! at all")))

(deftest every-hot-namespace-init-names-exists
  (testing "init.clj names no hive-mcp.hot.* namespace that is missing from the classpath"
    ;; hive-mcp.hot.state and hive-mcp.hot.silence were required at boot long
    ;; after both were deleted; a result/rescue around each require hid the
    ;; drift. Pure source scan: nothing is required or invoked.
    (let [src      (slurp (io/resource "hive_mcp/server/init.clj"))
          named    (set (re-seq #"hive-mcp\.hot\.[a-z][a-z0-9-]*(?:\.[a-z][a-z0-9-]*)*" src))
          ns->path (fn [n] (-> n (str/replace "-" "_") (str/replace "." "/")))
          missing  (remove (fn [n] (some #(io/resource (str (ns->path n) %)) [".clj" ".cljc"]))
                           named)]
      (is (seq named) "vacuity guard: init.clj must still name hot namespaces")
      (is (empty? missing) (str "init.clj names absent namespaces: " (vec missing))))))
