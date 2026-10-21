(ns hive-mcp.agent.ling.start-preflight-trifecta-test
  "Trifectas for the pure folder-trust rules of
   `hive-mcp.agent.ling.start-preflight` (candidate directories, the trust
   test, the refusal text) and a golden on `untrusted-refusal` with an
   injected IFolderTrust stub."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [hive-mcp.agent.ling.start-preflight :as sp]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private Dir
  "Absolute paths 0..3 segments deep, half with a trailing slash."
  [:string {:gen/schema [:tuple [:vector {:max 3} [:enum "ws" "hive" "a"]] :boolean]
            :gen/fmap   (fn [[segs slash?]] (str "/" (str/join "/" segs) (when (and slash? (seq segs)) "/")))}])

(defn- strip [s] (if (and (> (count s) 1) (str/ends-with? s "/")) (subs s 0 (dec (count s))) s))

(deftrifecta-from-schema trust-candidates-trifecta
  hive-mcp.agent.ling.start-preflight/trust-candidates
  {:in          [:cat Dir [:maybe [:enum "/ws" "/ws/hive" "/elsewhere"]]]
   :out         [:vector :string]
   :rel         (fn [[dir root] out]
                  (let [d (strip dir)]
                    (and (= d (first out))
                         ;; each step is the parent of the previous one
                         (every? (fn [[a b]] (and (str/starts-with? a b) (not= a b))) (partition 2 1 out))
                         (if (and root (or (= d root) (str/starts-with? d (str root "/"))))
                           (= root (peek out))
                           (= 1 (count (peek out)))))))
   :num-tests   300
   :n-cases     12
   :golden-path "test/golden/agent/start-preflight-trust-candidates.edn"})

(deftrifecta-from-schema trusted?-trifecta
  hive-mcp.agent.ling.start-preflight/trusted?
  {:in        [:cat [:vector {:max 3} [:enum "/a" "/a/b" "/c"]]
               [:maybe [:or [:set [:enum "/a" "/c" "/z"]] [:vector {:max 2} [:enum "/a/b" "/z"]]]]]
   :out       :boolean
   :rel       (fn [[cands trusted] out] (= out (boolean (some (set trusted) cands))))
   :num-tests 300
   :n-cases   12})

(deftrifecta-from-schema refusal-message-trifecta
  hive-mcp.agent.ling.start-preflight/refusal-message
  {:in        [:cat [:enum "/ws/hive/x" "/tmp"] [:enum "/ws/hive" "/tmp"]]
   :out       :string
   :rel       (fn [[cwd root] s]
                (and (str/includes? s (str "Claude Code has not trusted " root " (spawn cwd " cwd ")"))
                     (str/includes? s (str "projects[\"" root "\"]"))))
   :n-cases   4})

(defrecord ^:private StubTrust [root trusted]
  sp/IFolderTrust
  (checkout-root [_ _] root)
  (trusted-roots [_] trusted))

(deftest-golden-fn untrusted-refusal-golden
  "test/golden/agent/start-preflight-untrusted-refusal.edn"
  (fn []
    (vec (for [[label src mode cwd]
               [[:trusted-root     (->StubTrust "/ws/hive" #{"/ws/hive"}) :claude "/ws/hive/sub"]
                [:untrusted-root   (->StubTrust "/ws/hive" #{"/other"}) :claude "/ws/hive/sub"]
                [:no-root          (->StubTrust nil #{}) :claude "/tmp/x/"]
                [:not-interactive  (->StubTrust nil #{}) :headless "/tmp/x"]
                [:no-cwd           (->StubTrust nil #{}) :claude nil]]]
           [label (sp/untrusted-refusal src mode cwd)]))))

(deftest trust-candidates-stop-at-filesystem-root-test
  (is (= ["/a/b" "/a" "/"] (sp/trust-candidates "/a/b/" nil))))
