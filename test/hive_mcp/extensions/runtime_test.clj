(ns hive-mcp.extensions.runtime-test
  "hive-mcp provisions an addon's client runtime on mount: the lifecycle host
   and the mount-compose path both leave the runtime's install directory
   matching the files the addon ships."
  (:require [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-addon.lifecycle.port :as lport]
            [hive-addon.protocol :as proto]
            [hive-mcp.addons.core :as addon-core]
            [hive-mcp.extensions.lifecycle :as lcm]
            [hive-mcp.extensions.registry :as ext]
            [hive-mcp.extensions.runtime :as rt])
  (:import [java.nio.file Files]
           [java.nio.file.attribute FileAttribute]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:dynamic *source-dir* nil)

(defrecord RuntimeAddon []
  proto/IAddon
  (addon-id [_] "probe.runtime")
  (addon-type [_] :native)
  (capabilities [_] #{})
  (initialize! [_ _] {:success? true :errors []})
  (shutdown! [_] nil)
  (tools [_] [])
  (schema-extensions [_] [])
  (health [_] {:status :ok})
  (excluded-tools [_] #{})
  (hooks [_] {:probe/plugin-dir (fn [] *source-dir*)}))

(defn make-runtime-addon [_config] (->RuntimeAddon))

(def runtime-spec
  {:addon/id "probe.runtime" :addon/type :native
   :addon/init-ns "hive-mcp.extensions.runtime-test" :addon/init-fn "make-runtime-addon"
   :addon/capabilities #{}
   :addon/runtime [{:runtime/id "probe-runtime"
                    :runtime/client :vim
                    :runtime/source {:source/hook :probe/plugin-dir}
                    :runtime/on-load ["let g:probe_runtime = 1"]}]})

(defn- temp-dir []
  (.toFile (Files/createTempDirectory "hive-mcp-runtime" (make-array FileAttribute 0))))

(defn- delete-tree! [f]
  (doseq [x (reverse (file-seq (io/file f)))] (io/delete-file x true)))

(defn- tree
  "Relative path -> content of every file under DIR."
  [dir]
  (let [root (.toPath (io/file dir))]
    (into {}
          (comp (filter #(.isFile ^java.io.File %))
                (map (fn [^java.io.File f]
                       [(str (.relativize root (.toPath f))) (slurp f)])))
          (file-seq (io/file dir)))))

(defn- shipped-plugin! [dir]
  (doseq [[rel content] {"plugin/probe.vim" "command! ProbeRuntime echo 'probe'\n"
                         "autoload/probe.vim" "function! probe#hello() abort\nendfunction\n"
                         "doc/probe.txt" "*probe.txt*\n"}]
    (let [f (io/file dir rel)]
      (io/make-parents f)
      (spit f content)))
  dir)

(defn- clean [f]
  (ext/clear-all!)
  (addon-core/reset-registry!)
  (rt/reset-provisioner!)
  (try (f)
       (finally
         (rt/reset-provisioner!)
         (ext/clear-all!)
         (addon-core/reset-registry!))))

(use-fixtures :each clean)

(defn- with-world
  "Call F with {:home :source} temp dirs, the shared provisioner built over
   :home with no live activation."
  [f]
  (let [dir (temp-dir)
        home (io/file dir "home")
        source (shipped-plugin! (io/file dir "shipped" "plugin"))]
    (try
      (rt/provisioner {:home (str home) :live? false})
      (binding [*source-dir* (str source)]
        (f {:home home :source source}))
      (finally (delete-tree! dir)))))

(defn- install-dir [home]
  (io/file home ".vim" "pack" "hive" "start" "probe-runtime"))

(defn- result-for [report id]
  (some #(when (= id (:addon/id %)) %) (:mounted report)))

(deftest mounting-through-the-lifecycle-host-installs-the-shipped-runtime
  (with-world
    (fn [{:keys [home source]}]
      (let [host (lcm/host {:resolve-config (constantly {})})
            report (lport/-mount! host [runtime-spec] [runtime-spec])
            runtime (:runtime (result-for report "probe.runtime"))
            installed (install-dir home)]
        (is (:ok? report) (pr-str report))
        (is (true? (:ok? runtime)) (pr-str runtime))
        (testing "the install directory holds exactly the shipped files plus the loader"
          (is (= (tree source)
                 (dissoc (tree installed) "plugin/zz_hive_runtime.vim"))))
        (testing "the loader runs the declared on-load commands"
          (is (str/includes? (slurp (io/file installed "plugin/zz_hive_runtime.vim"))
                             "let g:probe_runtime = 1")))
        (testing "a remount refreshes a stale install"
          (spit (io/file installed "plugin/probe.vim") "\" stale copy\n")
          (spit (io/file installed "plugin/orphan.vim") "\" left behind\n")
          (addon-core/shutdown-addon! "probe.runtime")
          (addon-core/unregister-addon! "probe.runtime")
          (lport/-mount! host [runtime-spec] [runtime-spec])
          (is (= (tree source)
                 (dissoc (tree installed) "plugin/zz_hive_runtime.vim"))))
        (testing "deprovision removes what was installed"
          (is (:ok? (rt/deprovision! "probe.runtime")))
          (is (not (.exists installed))))))))

(deftest the-mount-compose-path-provisions-after-compose
  (with-world
    (fn [{:keys [home source]}]
      (let [instance (->RuntimeAddon)
            report {:mounted [{:addon/id "probe.runtime" :success? true :phase :initialized}
                              {:addon/id "probe.failed" :success? false :phase :init}]
                    :order ["probe.runtime" "probe.failed"] :ok? false}
            out (rt/provision-mounted report
                                      [runtime-spec (assoc runtime-spec :addon/id "probe.failed")]
                                      (rt/provision-fn)
                                      {"probe.runtime" instance "probe.failed" instance})]
        (is (true? (:ok? (:runtime (result-for out "probe.runtime")))))
        (is (nil? (:runtime (result-for out "probe.failed"))) "a failed mount is not provisioned")
        (is (= (tree source)
               (dissoc (tree (install-dir home)) "plugin/zz_hive_runtime.vim")))))))

(deftest provision-mounted-leaves-a-report-alone-without-a-provisioner-or-work
  (let [report {:mounted [{:addon/id "a" :success? true :runtime {:ok? true}}
                          {:addon/id "b" :success? true}]}
        calls (atom [])
        provision (fn [spec _] (swap! calls conj (:addon/id spec)) {:ok? true})]
    (is (= report (rt/provision-mounted report [{:addon/id "a"}] nil {})))
    (rt/provision-mounted report [{:addon/id "a"} {:addon/id "b"}] provision {"a" :i "b" :i})
    (is (= ["b"] @calls) "a result that already carries :runtime is not provisioned twice")))

(deftest provisioning-is-on-unless-turned-off
  (is (true? (rt/enabled? {} nil)))
  (is (false? (rt/enabled? {:runtime {:enabled? false}} nil)))
  (is (false? (rt/enabled? {} "0")))
  (is (true? (rt/enabled? {:runtime {:enabled? false}} "true"))))

(deftest live-activation-goes-to-the-registered-client-eval
  (is (nil? (rt/registered-eval :vim ["echo 1"])) "no transport registered: nothing to activate")
  (let [seen (atom nil)]
    (ext/register! rt/client-eval-key (fn [client cmds] (reset! seen [client cmds]) :done))
    (is (= :done (rt/registered-eval :vim ["echo 1"])))
    (is (= [:vim ["echo 1"]] @seen))))
