(ns hive-mcp.server.log-guard-facet-test
  "Facets for hive-mcp.server.log-guard: credential redaction (pure), and the
   close lifecycle of a guarded writer and a sink, driven through an injected
   LineWriter (no native writer, no with-redefs)."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.server.log-guard :as guard]
            [hive-mcp.server.log-sink :as sink]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

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

(def ^:private credential-words
  "The Word vocabulary entries that are themselves a credential assignment or
   a bare API key."
  #{"api_key=hunter2" "token=abc" "PASSWORD:pw" "sk-abcdefghij" "secret=s3"})

(def ^:private credential-prefixes
  "The Word vocabulary entries whose NEXT word is a credential."
  #{"Bearer" "Authorization:"})

(defn- expected-redaction
  "Independent oracle over the closed Word vocabulary: exactly the credential
   words, and every word after a credential prefix (judged on the original
   previous word), become [redacted]; every other word is kept verbatim."
  [ws]
  (mapv (fn [prev w]
          (if (or (credential-words w) (credential-prefixes prev)) "[redacted]" w))
        (cons nil ws) ws))

(deftrifecta-from-schema redact hive-mcp.server.log-guard/redact
  {:in [:and {:gen/fmap line->str :gen/schema Line} :string]
   :out :string
   :rel (fn [in out]
          (let [ws (words in) os (str/split out #" ")]
            (if (empty? ws)
              (= "" out)
              (= (expected-redaction ws) os))))
   :classify (fn [_ out] (if (str/includes? out "[redacted]") :redacted :clean))
   :classify-domain #{:redacted :clean}
   :num-tests 200})

;; BUG (recorded, not fixed; src/hive_mcp/server/log_guard.clj secret-word?):
;; a credential key word that ENDS in ':' or '=' and is followed by whitespace
;; is not redacted, and neither is the secret word after it, because only
;; bearer/authorization prefixes redact the NEXT word. Tracked by bug card
;; 20261022150130-5f4d0e4f. The KNOWN-BUG golden below pins that leaking
;; behaviour on purpose, so a fix shows up as a deliberate golden change; its
;; expected output is NOT the desired behaviour.
(golden/deftest-golden redact-golden
  "test/golden/hive-mcp/server/log-guard-redact.edn"
  (into (sorted-map)
        (for [[label s] {"plain"         "plain message, nothing secret"
                         "bearer-header" "Authorization: Bearer sk-abc123xyz"
                         "assignment"    "api_key=hunter2 and other words"
                         "whitespace"    "  many\t\twhitespace \n runs  "
                         "empty"         ""}]
          [label {:in s :out (guard/redact s)}])))

(golden/deftest-golden redact-KNOWN-BUG-leaks-secret-after-spaced-key-golden
  "test/golden/hive-mcp/server/log-guard-redact-known-bug-leak.edn"
  (into (sorted-map)
        (for [[label s] {"colon-space"  "api_key=hunter2 and token: kept-because-separate"
                         "equals-space" "token=abc password=  sk-short"}]
          [label {:in s :out (guard/redact s)}])))

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

(defn- refusing-sink
  "A Sink that satisfies sink/Sink but throws on close: its :running atom has a
   validator that refuses false, so log-sink/stop! raises."
  []
  (assoc (sink/make-sink (guard/guarded-writer (->ListWriter (atom []))) {:capacity 4 :start? false})
         :running (atom true :validator true?)))

(deftest close-sinks-fn-closes-every-sink-and-survives-a-bad-one
  (let [lines (atom [])
        bad (refusing-sink)
        good (sink/make-sink (guard/guarded-writer (->ListWriter lines)) {:capacity 4 :start? false})
        hook (guard/close-sinks-fn [bad good] 100)]
    (is (m/validate sink/Sink bad) "the bad sink honours the contract's input schema")
    (is (thrown? IllegalStateException (guard/close-sink! bad 100)) "it does throw on close")
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
