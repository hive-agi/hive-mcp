(ns hive-mcp.server.log-guard-facet-test
  "Facets for hive-mcp.server.log-guard: credential redaction (pure), and the
   close lifecycle of a guarded writer and a sink, driven through an injected
   LineWriter (no native writer, no with-redefs)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.server.log-guard :as guard]
            [hive-mcp.server.log-sink :as sink]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; =============================================================================
;; redact (pure)
;; =============================================================================

(def ^:private Word
  "Plain words, credential assignments, bare API keys and bearer prefixes, so
   both the kept and the redacted branch are generated."
  [:or
   [:enum "hello" "embedding" "failed" "after" "9800" "ms" "token" "key:" "Bearer" "Authorization:"]
   [:enum "api_key=hunter2" "token=abc" "PASSWORD:pw" "sk-abcdefghij" "secret=s3"]
   [:string {:min 1 :max 6 :gen/elements ["a" "bb" "x1" "Q"]}]])

(def ^:private Line
  "A line of such words joined by runs of spaces, tabs or newlines."
  [:vector {:gen/max 8} [:tuple Word [:enum " " "  " "\t" "\n "]]])

(defn- line->str [pairs] (apply str (mapcat identity pairs)))

(defn- words [s] (remove str/blank? (str/split (str s) #"\s+")))

(deftrifecta-from-schema redact hive-mcp.server.log-guard/redact
  {:in [:and {:gen/fmap line->str :gen/schema Line} :string]
   :out :string
   :rel (fn [in out]
          (let [ws (words in) os (str/split out #" ")]
            (if (empty? ws)
              (= "" out)
              (and (= (count ws) (count os))
                   (every? true? (map #(or (= %1 %2) (= "[redacted]" %2)) ws os))
                   (not (str/includes? out "hunter2"))
                   (not (str/includes? out "sk-abcdefghij"))))))
   :classify (fn [_ out] (if (str/includes? out "[redacted]") :redacted :clean))
   :classify-domain #{:redacted :clean}
   :num-tests 200})

(golden/deftest-golden redact-golden
  "test/golden/hive-mcp/server/log-guard-redact.edn"
  (into (sorted-map)
        (for [s ["plain message, nothing secret"
                 "Authorization: Bearer sk-abc123xyz"
                 "api_key=hunter2 and token: kept-because-separate"
                 "token=abc password=  sk-short"
                 "  many\t\twhitespace \n runs  "
                 ""]]
          [s (guard/redact s)])))

;; =============================================================================
;; Close lifecycle (LineWriter port injected)
;; =============================================================================

(defrecord ListWriter [lines]
  sink/LineWriter
  (write-line! [_ line] (swap! lines conj line)))

(defn- writer-trace
  "What reaches the delegate and what close answers, across one lifecycle."
  []
  (let [lines (atom [])
        w (guard/guarded-writer (->ListWriter lines))
        before (guard/closed? w)]
    (sink/write-line! w "one")
    (let [first-close (guard/close-writer! w 100)]
      (sink/write-line! w "two")
      {:closed-before before
       :closed-after (guard/closed? w)
       :first-close first-close
       :second-close (guard/close-writer! w 100)
       :unguarded-close (guard/close-writer! (->ListWriter (atom [])) 100)
       :unguarded-closed? (guard/closed? (->ListWriter (atom [])))
       :delegate @lines})))

(golden/deftest-golden guarded-writer-lifecycle-golden
  "test/golden/hive-mcp/server/log-guard-lifecycle.edn"
  (writer-trace))

(deftest close-sinks-fn-closes-every-sink-and-survives-a-bad-one
  (let [lines (atom [])
        good (sink/make-sink (guard/guarded-writer (->ListWriter lines)) {:capacity 4 :start? false})
        hook (guard/close-sinks-fn [{:not "a sink"} good] 100)]
    (is (nil? (hook)) "a sink that throws on close does not stop the hook")
    (is (guard/closed? (:writer good)))
    (is (= :dropped (sink/offer! good "late")))))

(deftest appender-keeps-only-warnings-of-the-named-prefixes
  (let [s (sink/make-sink (->ListWriter (atom [])) {:capacity 8 :start? false})
        append! (guard/ns-warn-appender-fn s ["hive-mcp.embeddings."])]
    (doseq [[level ns-str] [[:warn "hive-mcp.embeddings.a"] [:error "hive-mcp.embeddings.b"]
                            [:fatal "hive-mcp.embeddings.c"] [:info "hive-mcp.embeddings.d"]
                            [:warn "hive-mcp.server.core"] [:warn nil]]]
      (append! {:level level :?ns-str ns-str :vargs ["m"] :instant (java.util.Date. 0)}))
    (is (= ["1970-01-01T00:00:00Z WARN hive-mcp.embeddings.a m"
            "1970-01-01T00:00:00Z ERROR hive-mcp.embeddings.b m"
            "1970-01-01T00:00:00Z FATAL hive-mcp.embeddings.c m"]
           (vec (:queue s))))))
