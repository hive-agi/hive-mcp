(ns hive-mcp.service.install
  "Install the committed hive-mcp systemd USER unit, without enabling it.

   `install-plan` is pure: given the unit text, the target path and what is
   already on disk, it answers the ordered steps. `run-plan!` performs them
   through injected effects. Neither step enables or starts the unit; that
   would take over the live serving port and stays a host decision.

   The `bb install-unit` task loads this namespace on babashka, which has no
   malli, so the malli contracts sit behind a `#?(:bb .. :clj ..)` reader
   conditional: the JVM (and its contract-coverage gate) sees them, bb does
   not. The text installed is the committed
   resources/systemd/hive-mcp.service, which the unit test pins to
   `hive-mcp.service.unit/render-unit` of the default opts."
  (:require [clojure.java.io :as io]
            [clojure.java.shell :as shell]
            [clojure.string :as str]
            #?@(:bb [] :clj [[malli.core :as m]])))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def unit-name
  "File name of the unit under the systemd user directory."
  "hive-mcp.service")

(def daemon-reload-argv
  "The one systemctl call the install makes."
  ["systemctl" "--user" "daemon-reload"])

(defn unit-dir
  "The systemd user unit directory for `home`."
  [home]
  (str home "/.config/systemd/user"))

(defn install-plan
  "Ordered steps that install `unit-text` into `dir`. Pure.
   `current` is the text already at the target, or nil when absent.
   An unchanged unit yields no steps. Steps:
     {:op :mkdir :path dir}
     {:op :write :path p :content s}
     {:op :run :argv v}"
  [{:keys [unit-text dir current]}]
  (if (= current unit-text)
    []
    (let [path (str dir "/" unit-name)]
      [{:op :mkdir :path dir}
       {:op :write :path path :content unit-text}
       {:op :run :argv daemon-reload-argv}])))

(defn describe-step
  "One line naming what `step` does. Pure."
  [{:keys [op path argv]}]
  (case op
    :mkdir (str "mkdir -p " path)
    :write (str "write " path)
    :run (str/join " " argv)))

(defn run-plan!
  "Perform `steps` through `fx` {:mkdir! :write! :run-cmd!}, in order.
   Stops at the first :run whose result has a non-zero :exit.
   => {:done [step ...] :failed step-or-nil :result run-result-or-nil}."
  [{:keys [mkdir! write! run-cmd!]} steps]
  (loop [done [] [step & more :as remaining] steps]
    (if (empty? remaining)
      {:done done :failed nil :result nil}
      (case (:op step)
        :mkdir (do (mkdir! (:path step)) (recur (conj done step) more))
        :write (do (write! (:path step) (:content step)) (recur (conj done step) more))
        :run (let [r (run-cmd! (:argv step))]
               (if (zero? (:exit r 0))
                 (recur (conj done step) more)
                 {:done done :failed step :result r}))))))

(def host-fx
  "The real effects: filesystem and a subprocess."
  {:mkdir! (fn [path] (.mkdirs (io/file path)))
   :write! (fn [path content] (spit path content))
   :run-cmd! (fn [argv] (apply shell/sh argv))})

(defn- read-if-exists
  "Text at `path`, or nil when there is no file."
  [path]
  (let [f (io/file path)]
    (when (.exists f) (slurp f))))

(defn -main
  "bb install-unit [--dry-run]: copy the committed unit into
   ~/.config/systemd/user and run daemon-reload. Never enables or starts it."
  [& args]
  (let [dry? (some #{"--dry-run"} args)
        dir (unit-dir (System/getProperty "user.home"))
        unit-text (slurp (or (io/resource "systemd/hive-mcp.service")
                             (io/file "resources/systemd/hive-mcp.service")))
        steps (install-plan {:unit-text unit-text
                             :dir dir
                             :current (read-if-exists (str dir "/" unit-name))})]
    (when (empty? steps)
      (println "hive-mcp.service is already installed and unchanged."))
    (doseq [s steps]
      (println (str (when dry? "[dry-run] ") (describe-step s))))
    (when-not dry?
      (let [{:keys [failed result]} (run-plan! host-fx steps)]
        (when failed
          (binding [*out* *err*]
            (println (str "failed: " (describe-step failed) "\n" (:err result))))
          (System/exit 1))))
    (when (seq steps)
      (println "Not enabled. To run it: systemctl --user enable --now hive-mcp.service"
               "(this takes over the serving nREPL port)."))))

;; Contracts, JVM only: babashka has no malli.
#?(:bb nil
   :clj
   (do
     (def Step
       "One install step."
       [:multi {:dispatch :op}
        [:mkdir [:map [:op [:= :mkdir]] [:path :string]]]
        [:write [:map [:op [:= :write]] [:path :string] [:content :string]]]
        [:run [:map [:op [:= :run]] [:argv [:vector :string]]]]])

     (m/=> unit-dir [:=> [:cat :string] :string])

     (m/=> install-plan
           [:=> [:cat [:map [:unit-text :string] [:dir :string] [:current [:maybe :string]]]]
            [:vector Step]])

     (m/=> describe-step [:=> [:cat Step] :string])

     (m/=> run-plan!
           [:=> [:cat [:map [:mkdir! ifn?] [:write! ifn?] [:run-cmd! ifn?]] [:sequential Step]]
            [:map [:done [:vector Step]] [:failed [:maybe Step]] [:result :any]]])

     (m/=> -main [:=> [:cat [:* :string]] :any])))
