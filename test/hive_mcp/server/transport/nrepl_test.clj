(ns hive-mcp.server.transport.nrepl-test
  "Facets for the embedded nREPL starter and its `.nrepl-port` file.

   The port file lives in the working directory, which is also where a
   developer's own REPL keeps its port, so every test snapshots the file and
   puts it back. The starter is only driven with a port the probe refuses,
   which is the `already running, skip` arm: no server is ever started."
  (:require [clojure.java.io :as io]
            [clojure.test :refer [deftest is use-fixtures]]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as src]
            [hive-mcp.server.transport.nrepl :as tn]
            [hive-mcp.server.transport.schema :as ts]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-test.golden :as golden]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private port-file (io/file ".nrepl-port"))

(defn- own-the-port-file [f]
  (let [prior (when (.exists port-file) (slurp port-file))]
    (try (f)
         (finally
           (if prior
             (spit port-file prior)
             (io/delete-file port-file true))))))

(use-fixtures :each own-the-port-file)

(deftrifecta-from-schema write-port-file-round-trips
  #'tn/write-port-file!
  {:in [:cat [:or :int :string :keyword]]
   :out ts/EffectResult
   :rel (fn [[port] out]
          (and (= {:ok true} out)
               (= (str port) (slurp port-file))))
   :num-tests 50})

(golden/deftest-golden port-file-lifecycle-golden
  "test/golden/hive-mcp/server/transport/nrepl-port-file.edn"
  (let [written (tn/write-port-file! 7910)
        content (slurp port-file)
        deleted (tn/delete-port-file!)
        gone?   (not (.exists port-file))
        again   (tn/delete-port-file!)]
    {:written written :content content :deleted deleted
     :gone? gone? :delete-when-absent again}))

(deftest a-refused-port-skips-the-embedded-start
  (let [server-atom (atom nil)]
    (binding [config/*config-source*
              (src/atom-source (atom {:services {:nrepl {:port -1}}}) {})]
      (is (nil? (tn/start-embedded-nrepl! server-atom))))
    (is (nil? @server-atom) "nothing was started, so nothing was stored")))

(deftest the-server-schema-generates-a-server-record
  (is (m/validate ts/NreplServer (nrepl.server/map->Server {})))
  (is (not (m/validate ts/NreplServer {:port 1}))))
