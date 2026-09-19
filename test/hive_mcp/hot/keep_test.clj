(ns hive-mcp.hot.keep-test
  "Keeps are derived from source plus the live root, and they carry a defonce
   across a real clj-reload unload/load. The integration case drives clj-reload
   over a temp directory and restores its global config afterwards."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is testing]]
            [hive-mcp.hot.keep :as keep]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(deftest top-level-defs-are-read-the-way-clj-reload-reads-them
  (is (= '[[ns probe] [defonce a] [def b] [defn f] [def c]]
         (keep/top-level-defs
          "(ns probe)\n(defonce ^:private a (atom 0))\n(def b (atom {}))\n(defn f [] 1)\n#_(def skipped 1)\n(def c 1)\n(comment (def in-comment 2))")))
  (is (empty? (keep/top-level-defs ""))))

(deftest a-keep-is-every-defonce-and-every-def-holding-a-resource
  (let [vars   {'b :resource 'c :plain}
        var-of (fn [sym] (get vars sym))]
    (is (= '{a {:tag defonce} b {:tag def}}
           (keep/keep-plan '[[ns probe] [defonce a] [def b] [defn f] [def c] [def absent]]
                           var-of
                           #(= :resource %)))
        "a defonce is kept unconditionally; a def only for a live resource; an unresolved def is not")))

(deftest a-stuck-namespace-is-pending-load-loaded-and-not-pending-unload
  (is (= '[b]
         (keep/stuck-namespaces {:to-load '[a b c] :to-unload '[c]} '#{b c d}))))

(deftest a-dead-link-is-an-alias-a-refer-or-a-var-seam-through-a-namespace-object-that-is-not-live
  (try
    (let [holder (create-ns 'probe.holder)
          seam   (create-ns 'probe.seam)
          target (create-ns 'probe.target)]
      (intern target 'thing 1)
      (intern seam 'thing (ns-resolve target 'thing))
      (binding [*ns* holder]
        (alias 't 'probe.target)
        (refer 'probe.target))
      (is (= {} (keep/dead-links "probe.")) "live links are not dead")
      (remove-ns 'probe.target)
      (is (= '{probe.holder [probe.target] probe.seam [probe.target]} (keep/dead-links "probe."))
          "a removed target is dead through an alias, a refer and a var seam alike")
      (create-ns 'probe.target)
      (is (= '{probe.holder [probe.target] probe.seam [probe.target]} (keep/dead-links "probe."))
          "a recreated target is still dead: the object differs"))
    (finally
      (remove-ns 'probe.holder)
      (remove-ns 'probe.seam)
      (remove-ns 'probe.target))))

(deftest a-repair-reloads-every-holder-and-every-tracked-target-that-is-gone
  (is (= '{:unload [h1 h2] :load [h1 h2 gone]}
         (keep/repair-plan '{h1 [gone alive] h2 [gone untracked]}
                           '#{h1 h2 gone alive}
                           '#{alive}))
      "a live target needs nothing; an untracked one cannot be loaded"))

(deftest a-repair-takes-the-dependents-of-what-it-reloads
  (require 'clj-reload.core)
  (let [state   '{:namespaces {a {:requires #{}} b {:requires #{a}} c {:requires #{b}} d {:requires #{}} e {:requires #{a}}}}
        closure (fn [present pinned] (set (keep/dependents-closure state '[a] present pinned)))]
    (is (= '#{a b c e} (closure '#{a b c d e} #{}))
        "every transitive dependent; an unrelated namespace is not touched")
    (is (= '#{a b e} (closure '#{a b d e} #{}))
        "a dependent that is not present is not loaded on the pass's behalf")
    (is (= '#{a b e} (closure '#{a b c d e} '#{c}))
        "a pinned dependent is left alone")))

(defonce ^:private probe-atom (atom 0))
(def ^:private probe-plain 1)

(def ^:private probe-seam #'probe-plain)

(deftest the-live-root-decides-what-a-def-holds
  (is (keep/live-resource? #'probe-atom))
  (is (not (keep/live-resource? #'probe-plain)))
  (is (not (keep/live-resource? #'probe-seam)) "a var seam follows the current var; keeping it would pin the old one")
  (is (keep/holds-state? 'hive-mcp.hot.keep-test)))

(defn- write! [f content]
  (spit f content)
  ;; clj-reload compares mtimes at second granularity; a rewrite in the same
  ;; second is not a change, so move the clock forward explicitly.
  (.setLastModified (io/file f) (+ (.lastModified (io/file f)) 2000)))

(deftest a-kept-defonce-survives-a-real-unload-and-load
  (require 'clj-reload.core)
  (let [dir        (.toFile (java.nio.file.Files/createTempDirectory "hive-keep" (make-array java.nio.file.attribute.FileAttribute 0)))
        src        (io/file dir "probe" "kept.clj")
        cfg-var    (find-var 'clj-reload.core/*config*)
        state      @(find-var 'clj-reload.core/*state)
        old-cfg    (when (bound? cfg-var) @cfg-var)
        old-st     @state
        init!      (requiring-resolve 'hive-hot.core/init!)
        reload!    (requiring-resolve 'hive-hot.core/reload!)
        reset-hot! (requiring-resolve 'hive-hot.core/reset-all!)
        loaded     @#'clojure.core/*loaded-libs*]
    (try
      (.mkdirs (.getParentFile src))
      (spit src "(ns probe.kept)\n(defonce counter (atom 0))\n(def registry (atom {}))\n(def plain 1)\n(def ^:clj-reload/keep declared 1)\n")
      (init! {:dirs [(str dir)] :since 0})
      ;; The temp dir is not on the classpath: load by path and register the
      ;; lib as loaded, which is all clj-reload consults.
      (load-file (str src))
      (dosync (alter loaded conj 'probe.kept))
      (let [counter  @(resolve 'probe.kept/counter)
            registry @(resolve 'probe.kept/registry)]
        (swap! counter inc)
        (write! src "(ns probe.kept)\n(defonce counter (atom 0))\n(def registry (atom {}))\n(def plain 2)\n(def ^:clj-reload/keep declared 2)\n")
        ;; An entry a previous rule left behind: clj-reload would carry it forever.
        (swap! state assoc-in [:namespaces 'probe.kept :keep 'plain] {:tag 'def})
        (testing "keeps are SET to what source declares plus what the live roots derive"
          (is (= '{probe.kept [counter declared registry]} (keep/inject-keeps! '[probe.kept])))
          (is (= '#{counter declared registry} (set (keys (get-in @state [:namespaces 'probe.kept :keep]))))
              "the stale entry is gone"))
        (let [res (reload! {})]
          (is (true? (:success res)) (str (:error res)))
          (is (= ['probe.kept] (:unloaded res)) "the namespace was unloaded, not loaded on top of itself")
          (testing "state crossed the pass by identity, code did not"
            (is (identical? counter @(resolve 'probe.kept/counter)))
            (is (= 1 @counter))
            (is (identical? registry @(resolve 'probe.kept/registry)))
            (is (= 1 @(resolve 'probe.kept/declared)) "a source-declared keep is honoured")
            (is (= 2 @(resolve 'probe.kept/plain)) "the stale entry did not pin the old value"))))
      (finally
        (remove-ns 'probe.kept)
        (dosync (alter loaded disj 'probe.kept))
        (reset-hot!)
        (when old-cfg
          (alter-var-root cfg-var (constantly old-cfg))
          (reset! state old-st))
        (doseq [f (reverse (file-seq dir))] (io/delete-file f true))))))
