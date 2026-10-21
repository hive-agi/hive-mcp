(ns hive-mcp.embeddings.cache-test
  "The embedding LRU+TTL cache. cache-key is pure and carries a contract, so
   it gets a schema-driven trifecta; the stateful store/lookup/stats cycle
   gets a golden that owns and restores the cache configuration."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.embeddings.cache :as cache]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private hex-chars (set "0123456789abcdef"))

(defn- key-shape?
  "OUT is PREFIX followed by a 64-char lowercase hex digest."
  [prefix out]
  (and (str/starts-with? out prefix)
       (let [h (subs out (count prefix))]
         (and (= 64 (count h)) (every? hex-chars h)))))

(deftrifecta-from-schema cache-key-scoped-by-space
  #'cache/cache-key
  {:in [:cat [:maybe :string] [:maybe :string] :string]
   :out :string
   ;; with a space id the key is coll:space:hash, without it the legacy coll:hash
   :rel (fn [[c s t] out]
          (and (key-shape? (str c ":" (when s (str s ":"))) out)
               (= out (cache/cache-key c s t))
               (if s
                 (not= out (cache/cache-key c t))
                 (= out (cache/cache-key c t)))))
   :classify (fn [[_ s _] _] (if s :spaced :legacy))
   :classify-domain #{:spaced :legacy}
   :golden-path "test/golden/hive-mcp/embeddings/cache-key.edn"
   :num-tests 200})

(defn- own-the-cache [f]
  (try (f) (finally (cache/configure! {}))))

(use-fixtures :each own-the-cache)

(deftest-golden store-lookup-stats-golden
  "test/golden/hive-mcp/embeddings/cache-cycle.edn"
  (let [fresh (cache/configure! {:cap 2 :ttl-ms 60000})
        _     (cache/store! "c" "a" [1.0])
        _     (cache/store! "c" "space-1" "a" [2.0])
        hits  [(cache/lookup "c" "a") (cache/lookup "c" "space-1" "a")
               (cache/lookup "c" "space-2" "a") (cache/lookup "other" "a")]
        _     (cache/store! "c" "b" [3.0])       ; cap 2: evicts the LRU entry
        lru   [(cache/lookup "c" "space-1" "a") (cache/lookup "c" "a") (cache/lookup "c" "b")]
        mid   (cache/stats)
        _     (cache/configure! {:cap 4 :ttl-ms -1}) ; every entry already expired
        _     (cache/store! "c" "z" [9.0])
        stale (cache/lookup "c" "z")
        end   (cache/stats)
        clear (cache/clear!)]
    {:fresh (dissoc fresh :map) :hits hits :lru lru :mid mid
     :stale stale :end end :cleared (dissoc clear :map) :after (cache/stats)}))

(deftest configure-defaults
  (let [s (cache/configure! {})]
    (is (= 2048 (:cap s)))
    (is (= 300000 (:ttl-ms s)))))
