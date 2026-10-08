(ns hive-mcp.transport.exposure-trifecta-test
  "Golden + property + mutation pinning for the shared exposure policy.

   Two subjects, both pure:

     bind-host  — a listener without a secret is loopback-only, whatever was
                  requested. The property that matters: blank secret =>
                  loopback, always.
     bearer-ok? — exact `Bearer <secret>` match, or no secret configured.

   Mutants are self-contained and never call the subject."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.transport.a2a :as a2a]
            [hive-mcp.transport.exposure :as exposure]
            [hive-mcp.transport.mcp-http :as mh]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn run-bind-host
  "Unary adapter: {:secret s :requested r} -> host."
  [{:keys [secret requested]}]
  (exposure/bind-host secret requested))

(defn run-bearer-ok?
  "Unary adapter: {:secret s :header h} -> boolean."
  [{:keys [secret header]}]
  (exposure/bearer-ok? secret header))

(def ^:private gen-maybe-str
  (gen/one-of [(gen/return nil) (gen/return "") (gen/return "  ")
               gen/string-alphanumeric]))

(deftrifecta bind-host-contract
  hive-mcp.transport.exposure-trifecta-test/run-bind-host
  {:golden-path "test/golden/transport/exposure-bind-host.edn"
   :cases       {:no-secret-no-request   {:secret nil :requested nil}
                 :no-secret-wants-all    {:secret nil :requested "0.0.0.0"}
                 :blank-secret-wants-lan {:secret "   " :requested "10.0.0.5"}
                 :secret-default         {:secret "k" :requested nil}
                 :secret-blank-request   {:secret "k" :requested ""}
                 :secret-wants-lan       {:secret "k" :requested "10.0.0.5"}}
   :gen         (gen/let [s gen-maybe-str r gen-maybe-str] {:secret s :requested r})
   :pred        string?
   :num-tests   200
   :mutations   [["always-all-interfaces — exposes an unauthenticated listener"
                  (constantly "0.0.0.0")]
                 ["requested-wins — ignores the missing secret"
                  (fn [{:keys [requested]}]
                    (if (str/blank? requested) "0.0.0.0" requested))]
                 ["always-loopback — a keyed listener can never be reached"
                  (constantly "127.0.0.1")]]
   :assert      (fn []
                  (is (= "127.0.0.1" (run-bind-host {:secret nil :requested "0.0.0.0"})))
                  (is (= "0.0.0.0" (run-bind-host {:secret "k" :requested nil})))
                  (is (= "10.0.0.5" (run-bind-host {:secret "k" :requested "10.0.0.5"}))))})

(deftrifecta bearer-ok-contract
  hive-mcp.transport.exposure-trifecta-test/run-bearer-ok?
  {:golden-path "test/golden/transport/exposure-bearer-ok.edn"
   :cases       {:no-secret        {:secret nil :header nil}
                 :blank-secret     {:secret "" :header "anything"}
                 :exact            {:secret "tok" :header "Bearer tok"}
                 :missing-header   {:secret "tok" :header nil}
                 :prefix-of-secret {:secret "tok" :header "Bearer to"}
                 :longer-secret    {:secret "tok" :header "Bearer tokk"}
                 :no-scheme        {:secret "tok" :header "tok"}}
   :gen         (gen/let [s gen-maybe-str h gen-maybe-str] {:secret s :header h})
   :pred        boolean?
   :num-tests   200
   :mutations   [["always-true — auth disabled"
                  (constantly true)]
                 ["prefix-match — accepts a truncated secret"
                  (fn [{:keys [secret header]}]
                    (or (str/blank? secret)
                        (str/starts-with? (str "Bearer " secret) (str header))))]
                 ["no-scheme — accepts the bare secret"
                  (fn [{:keys [secret header]}]
                    (or (str/blank? secret) (= (str header) (str secret))))]]
   :assert      (fn []
                  (is (true? (run-bearer-ok? {:secret "tok" :header "Bearer tok"})))
                  (is (false? (run-bearer-ok? {:secret "tok" :header "Bearer to"}))))})

(deftest both-transports-delegate-to-one-policy
  (doseq [[s r] [[nil "0.0.0.0"] ["k" nil] ["k" "10.0.0.5"] ["  " "10.0.0.5"]]]
    (is (= (exposure/bind-host s r) (a2a/bind-host s r) (mh/bind-host s r))))
  (doseq [[s h] [[nil nil] ["tok" "Bearer tok"] ["tok" "Bearer to"] ["tok" "tok"]]]
    (is (= (exposure/bearer-ok? s h) (mh/bearer-ok? s h)))))
