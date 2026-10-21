(ns hive-mcp.config.source-test
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.config.source :as source]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.generator :as mg]
            [clojure.test.check.generators :as gen]))

;; The subject is the contracted constructor; the relation observes the port.
(deftrifecta-from-schema static-source-values hive-mcp.config.source/static-source
  {:in [:map-of :keyword :int]
   :out [:fn {:gen/gen (gen/return (source/empty-source))}
         #(satisfies? source/IConfigSource %)]
   :rel (fn [config src] (= config (source/-config src)))
   :num-tests 500
   ;; No schema-derived mutant can invalidate an opaque port while preserving
   ;; its interface; the direct golden below snapshots the observable port value.
   :mutation false})

(deftest source-port-behavior
  (let [a (atom nil) s (source/atom-source a {:default 1})]
    (is (= {:default 1} (source/-config s)))
    (reset! a {:user 2})
    (is (= {:user 2} (source/-config s)))
    (is (identical? a (source/-config-atom s))))
  (is (= {} (source/-config (source/empty-source))))
  (is (satisfies? source/IConfigSource (mg/generate
    [:fn {:gen/gen (clojure.test.check.generators/return (source/empty-source))}
     #(satisfies? source/IConfigSource %)])))
  (golden/assert-golden
   (golden/anchor 'hive-mcp.config.source-test "test/golden/config/source-port.edn")
   (source/-config (source/static-source {:check :isolated}))))
