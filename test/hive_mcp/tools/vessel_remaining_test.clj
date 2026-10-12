(ns hive-mcp.tools.vessel-remaining-test
  "Magit, Projectile and the legacy memory import reach the editor only
   through the closed `:vessel :dispatch` capability (card E3-VESSEL-INFRA).

   The stub is the SPI port (hive-mcp.test.stub.swarm-host), never a concrete
   Emacs client. Op names and shapes match the translators hive-emacs
   registers in hive-emacs.{magit,projectile,memory}.translators."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing]]
            [clojure.test.check.generators :as gen]
            [hive-test.trifecta :refer [deftrifecta]]
            [hive-mcp.test.stub.memory-store :as ms]
            [hive-mcp.test.stub.swarm-host :as sh]
            [hive-mcp.tools.magit :as magit]
            [hive-mcp.tools.projectile :as projectile]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private ok {:success true :result "{}" :timed-out false})

(def ^:private legacy-export
  {:success true
   :result (json/write-str {:notes [{:id "n1" :content "a"}]
                            :snippets []
                            :conventions [{:id "c1" :content "b"}]
                            :decisions []})
   :timed-out false})

(def ^:private invocations
  "One handler invocation per converted caller, keyed by a label, with the
   exact op map it must dispatch. Every handler the card converted is listed:
   the trifecta runs each one against the stub vessel."
  {:magit-status     [#(magit/handle-magit-status {:directory "/tmp/repo"})
                      {:op :magit/status :directory "/tmp/repo"}]
   :magit-branches   [#(magit/handle-magit-branches {:directory "/tmp/repo"})
                      {:op :magit/branches :directory "/tmp/repo"}]
   :magit-log        [#(magit/handle-magit-log {:directory "/tmp/repo" :count 3})
                      {:op :magit/log :count 3 :directory "/tmp/repo"}]
   :magit-diff       [#(magit/handle-magit-diff {:directory "/tmp/repo" :target "bogus"})
                      {:op :magit/diff :target "staged" :directory "/tmp/repo"}]
   :magit-stage      [#(magit/handle-magit-stage {:directory "/tmp/repo" :files "a.clj b.clj"})
                      {:op :magit/stage :files ["a.clj" "b.clj"] :directory "/tmp/repo"}]
   :magit-commit     [#(magit/handle-magit-commit {:directory "/tmp/repo" :message "m" :all true})
                      {:op :magit/commit :message "m" :all true :directory "/tmp/repo"}]
   :magit-push       [#(magit/handle-magit-push {:directory "/tmp/repo" :remote " github "})
                      {:op :magit/push :set-upstream false :remote "github" :directory "/tmp/repo"}]
   :magit-pull       [#(magit/handle-magit-pull {:directory "/tmp/repo"})
                      {:op :magit/pull :directory "/tmp/repo"}]
   :magit-fetch      [#(magit/handle-magit-fetch {:directory "/tmp/repo" :remote "  "})
                      {:op :magit/fetch :remote nil :directory "/tmp/repo"}]
   :magit-feature    [#(magit/handle-magit-feature-branches {:directory "/tmp/repo"})
                      {:op :magit/feature-branches :directory "/tmp/repo"}]
   :project-info     [#(projectile/handle-projectile-info {:directory "/tmp/repo"})
                      {:op :project/info :directory "/tmp/repo"}]
   :project-files    [#(projectile/handle-projectile-files {:pattern ""})
                      {:op :project/files :pattern nil}]
   :project-find     [#(projectile/handle-projectile-find-file {:filename "core.clj"})
                      {:op :project/find-file :filename "core.clj"}]
   :project-search   [#(projectile/handle-projectile-search {:pattern "defn"})
                      {:op :project/search :pattern "defn"}]
   :project-recent   [#(projectile/handle-projectile-recent {})
                      {:op :project/recent}]
   :project-list     [#(projectile/handle-projectile-list-projects {})
                      {:op :project/list-projects}]})

(defn- exercise
  "Run the handler labelled LABEL against a stub vessel answering every op
   with RESPONSE (the legacy export answers its own fixture). Returns the
   handler response, the [op timeout-ms] pairs the stub received and the op
   the label expects."
  [{:keys [label response]}]
  (let [out (atom nil)
        [invoke expected] (get invocations label)]
    (ms/with-stub-store
      (fn []
        (sh/with-swarm-host
          [host (fn [op _t] (if (= :memory/legacy-export (:op op)) legacy-export response))]
          (reset! out {:response (invoke)
                       :calls (sh/calls host)
                       :expected expected}))))
    @out))

(deftrifecta remaining-callers-use-vessel
  exercise
  {:gen (gen/hash-map :label (gen/elements (vec (keys invocations)))
                      :response (gen/return ok))
   :pred #(and (= 1 (count (:calls %)))
               (= (:expected %) (ffirst (:calls %)))
               (nil? (get-in % [:response :isError])))
   :num-tests 60
   :mutations [["no-port-call" (fn [_] {:response {:type "text" :text "{}"} :calls [] :expected {}})]
               ["swallowed-error" (fn [_] {:response {:isError true} :calls [[{} nil]] :expected {}})]
               ["wrong-op" (fn [_] {:response {:type "text" :text "{}"}
                                     :calls [[{:op :magit/eval} nil]]
                                     :expected {:op :magit/status}})]]
   :assert (fn []
             ;; Compared against the FIXED table, never against what the
             ;; subject itself reports as expected.
             (doseq [[label [_ expected]] invocations]
               (let [{:keys [calls response]} (exercise {:label label :response ok})]
                 (is (= [expected] (map first calls)) (str label " dispatched the wrong op"))
                 (is (nil? (:isError response)) (str label " answered an error on success"))))
             (sh/with-swarm-host [_ (fn [_ _] {:success false :error "no vessel"})]
               (doseq [h [#(magit/handle-magit-status {:directory "/r"})
                          #(projectile/handle-projectile-recent {})]]
                 (is (true? (:isError (h))) "a failed dispatch is an error, never a success"))))})

(defn- dispatched-directory
  "The :directory of the single op a handler dispatches for DIR (a blank or
   absent caller directory), or ::absent when the op carries none."
  [{:keys [handler dir]}]
  (sh/with-swarm-host [host (fn [_ _] ok)]
    (case handler
      :magit (magit/handle-magit-status {:directory dir})
      :projectile (projectile/handle-projectile-info {:directory dir}))
    (get (ffirst (sh/calls host)) :directory ::absent)))

(deftrifecta blank-directory-falls-through
  dispatched-directory
  {:gen (gen/hash-map :handler (gen/elements [:magit :projectile])
                      :dir (gen/elements [nil "" " " "\t\n"]))
   :pred #(or (= ::absent %) (and (string? %) (not (str/blank? %))))
   :num-tests 40
   :mutations [["blank-passed-through" (fn [{:keys [dir]}] (or dir ""))]]
   :assert (fn []
             (is (= "/r" (dispatched-directory {:handler :magit :dir "/r"})))
             (is (= "/r" (dispatched-directory {:handler :projectile :dir "/r"})))
             (doseq [handler [:magit :projectile]
                     dir ["" "  "]]
               (let [d (dispatched-directory {:handler handler :dir dir})]
                 (is (or (= ::absent d) (and (string? d) (not (str/blank? d))))
                     (str handler " sent a blank :directory for " (pr-str dir)))))
             (is (= (System/getProperty "user.dir")
                    (magit/resolve-directory "  "))
                 "a blank directory falls through to the server cwd"))})


(deftest projectile-info-without-a-directory-never-sends-nil
  (testing ":project/info's :directory is optional but must be non-blank when present"
    (sh/with-swarm-host [host (fn [_ _] ok)]
      (projectile/handle-projectile-info {})
      (let [[[op]] (sh/calls host)]
        (is (= :project/info (:op op)))
        (is (or (not (contains? op :directory)) (string? (:directory op))))))))

(deftest projectile-keeps-its-text-envelope
  (sh/with-swarm-host [_ (fn [_ _] {:success true :result "[\"a.clj\"]"})]
    (is (= {:type "text" :text "[\"a.clj\"]"}
           (projectile/handle-projectile-files {:pattern "clj"}))))
  (sh/with-swarm-host [_ (fn [_ _] {:success false :error "projectile not loaded"})]
    (let [r (projectile/handle-projectile-recent {})]
      (is (true? (:isError r)))
      (is (str/includes? (:text r) "projectile not loaded")))))
