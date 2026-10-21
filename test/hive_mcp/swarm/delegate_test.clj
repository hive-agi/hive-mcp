(ns hive-mcp.swarm.delegate-test
  "Facets for hive-mcp.swarm.delegate, the resolution every swarm compat shim
   goes through.

     trifecta  addon-missing / addon-outdated from their contracts: the ex-data
               names the library (the first ns segment), the ns and the var
     golden    resolve-var / resolve-required / available? over a present
               namespace, a present namespace without the var, and an absent
               addon (deref nil, call throws addon-missing)"
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.swarm.delegate :as delegate]
            [hive-schemas.test :as hst]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private NsStr
  "Dotted namespace names of one to three segments, so the library segment
   is a real prefix of a multi-segment name."
  [:string {:gen/schema [:vector {:min 1 :max 3} [:enum "hive-agent" "swarm" "x" "a_b" "Q9"]]
            :gen/fmap #(str/join "." %)
            :min 1}])

(def ^:private Sym [:or :symbol :keyword [:string {:min 1}]])

(defn- names-the-addon?
  [[ns-str sym] e]
  (let [d (ex-data e)]
    (and (= ns-str (:ns d))
         (= (name sym) (:var d))
         (= (first (.split ^String ns-str "\\.")) (:addon d)))))

(hst/deftrifecta-from-schema addon-missing-names-the-addon
  hive-mcp.swarm.delegate/addon-missing
  {:in  [:cat NsStr Sym]
   :out delegate/AddonMissing
   :rel names-the-addon?
   :num-tests 200})

(hst/deftrifecta-from-schema addon-outdated-names-the-addon
  hive-mcp.swarm.delegate/addon-outdated
  {:in  [:cat NsStr Sym]
   :out delegate/AddonOutdated
   :rel names-the-addon?
   :num-tests 200})

(defn- call-answer
  "What calling F with no args does: its value, or the ex-data it throws."
  [f]
  (try [:value (f)]
       (catch clojure.lang.ExceptionInfo e [:threw (ex-message e) (ex-data e)])))

(defn- resolution-cases []
  (let [absent (delegate/resolve-var "f00-absent-addon.swarm.thing" 'make)]
    {:available-present  (delegate/available? "hive-mcp.swarm.delegate")
     :available-absent   (delegate/available? "f00-absent-addon.swarm.thing")
     :present-var        (delegate/resolve-var "clojure.string" 'join)
     :present-no-var     (delegate/resolve-var "clojure.string" 'f00-no-such-var)
     :absent-deref       @absent
     :absent-call        (call-answer absent)
     :required-present   (delegate/resolve-required "clojure.string" "join")
     :required-outdated  (call-answer #(delegate/resolve-required "clojure.string" 'f00-no-such-var))}))

(deftest-golden resolution-golden
  "test/golden/hive-mcp/swarm/delegate-resolution.edn"
  (update-vals (resolution-cases) #(if (var? %) (symbol %) %)))

(deftest absent-stand-in-is-callable-and-derefable-test
  (let [v (delegate/resolve-var "f00-absent-addon.x" "y")]
    (is (instance? clojure.lang.IDeref v))
    (is (thrown? clojure.lang.ExceptionInfo (v)))))
