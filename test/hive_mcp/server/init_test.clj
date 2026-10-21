(ns hive-mcp.server.init-test
  "Facets for hive-mcp.server.init (cov-facets f07).

   watch-dirs is pure and is driven from its contract. register-hot-reload-
   listener! takes its registration port as an argument, so it is pinned with
   a recording add-listener!. on-hot-reload-event! is pinned on the event
   types it ignores (only :reload-success acts).

   The other boot steps (init-nats!, wire-memory-store!, start-*-scheduler!,
   load-extensions!, ...) reach config, the store registry, schedulers and
   addons through concretions, not injected ports; they are exercised only by
   the boot integration suites and are recorded as missing seams."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.server.init :as init]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Dir [:enum "src" "dev" "/abs/src" "/abs/test" "lib/x"])

(def ^:private Roots
  ;; always anchored: the jar-backed (no-roots) arm resolves against the JVM
  ;; cwd, which differs per machine, so it is checked by a plain deftest below.
  [:vector {:min 1 :gen/max 2} [:enum "/proj/src" "/proj/dev" "/other/src"]])

(deftrifecta-from-schema watch-dirs-contract init/watch-dirs
  {:in  [:cat [:maybe [:vector {:gen/max 3} Dir]] Roots]
   :out [:vector [:string {:min 1}]]
   :rel (fn [[configured roots] out]
          (let [base (some-> (first roots) java.io.File. .getParent)
                want (if (seq configured) configured roots)]
            (and (= (count want) (count out))
                 (every? true?
                         (map (fn [d o]
                                (if (str/starts-with? d "/")
                                  (= o d)
                                  (= o (str base "/" d))))
                              want out)))))
   :num-tests 200
   :golden-path "test/golden/hive-mcp/f07-init-watch-dirs.edn"})

(deftest watch-dirs-without-roots-uses-the-working-directory
  (let [cwd (System/getProperty "user.dir")]
    (is (= [(str cwd "/src")] (init/watch-dirs nil nil)))
    (is (= ["/abs" (str cwd "/dev")] (init/watch-dirs ["/abs" "dev"] [])))))

(deftest-golden hot-reload-listener-golden
  "test/golden/hive-mcp/f07-init-hot-reload-listener.edn"
  (let [calls (atom [])
        ret   (init/register-hot-reload-listener!
               (fn [id f] (swap! calls conj [id (ifn? f)])))]
    {:returned ret
     :registered @calls
     :ignored-events [(init/on-hot-reload-event! nil)
                      (init/on-hot-reload-event! {:type :reload-failure})
                      (init/on-hot-reload-event! {:type :reload-start :loaded []})]}))

(deftest server-context-atom-is-nil-or-an-atom
  (let [a (init/server-context-atom)]
    (is (or (nil? a) (instance? clojure.lang.IAtom a)))))
