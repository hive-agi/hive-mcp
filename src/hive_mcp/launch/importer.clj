(ns hive-mcp.launch.importer
  "`bb launch:import <path>`: print the :launch fragment for a local.deps.edn.

   Reads the EDN at the given path, runs hive-mcp.launch.deps/import-local-deps
   over it, and writes the fragment to STDOUT as pretty EDN. It writes no file:
   the caller pastes the answer into config.edn under :launch, or redirects it
   deliberately.

   Exit codes: 0 on success, 1 when no path is given, 2 when the file cannot be
   read or parsed. Diagnostics go to STDERR, so a redirect of STDOUT captures
   the fragment and nothing else."
  (:require [clojure.edn :as edn]
            [clojure.java.io :as io]
            [clojure.pprint :as pprint]
            [hive-mcp.launch.deps :as deps]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def usage
  "The one-line usage notice, printed to STDERR when the path is missing."
  "usage: bb launch:import <path-to-local.deps.edn>")

(defn fragment-for
  "The :launch fragment for the EDN source text SRC, or nil when SRC does not
   read as EDN."
  [src]
  (deps/import-local-deps (edn/read-string {:default tagged-literal} src)))

(defn -main
  "Print the :launch fragment for the file named by the first argument.

   Never writes a file; never reads one but that argument."
  [& args]
  (let [path (first args)]
    (cond
      (nil? path)
      (do (.println System/err usage)
          (System/exit 1))

      :else
      (let [f (io/file path)]
        (cond
          (not (.isFile f))
          (do (.println System/err (str "no such file: " path))
              (System/exit 2))

          :else
          (let [fragment (fragment-for (slurp f))]
            ;; A coordinate prints as {:local/root ".."}, not #:local{:root
            ;; ".."}: the same EDN, spelled as a person writes it by hand.
            (binding [*print-namespace-maps* false]
              (pprint/pprint fragment))
            (flush)
            (System/exit 0)))))))
