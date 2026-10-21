(ns hive-mcp.agent.provider.collect-test
  "Golden over the provider config readers, with the host config injected
   through `hive-mcp.config.core/*config-source*` bound to a static source
   (the IConfigSource port), once with a populated config and once with a
   config whose entries have the wrong shapes."
  (:require [clojure.test :refer [deftest is]]
            [hive-mcp.agent.provider.collect :as collect]
            [hive-mcp.config.core :as config]
            [hive-mcp.config.source :as source]
            [hive-test.golden :refer [deftest-golden-fn]]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; Secret names are chosen so the env-var fallback (F02_COLLECT_...) is unset.
(def ^:private populated
  {:agent-defaults        {:explore {:model "haiku"} :coder {:model "opus"}}
   :llm-provider-priority [:anthropic :openrouter]
   :llm-providers         {:openrouter {:base-url "https://x"}}
   :secrets               {:f02-collect-a "sk-a" :f02-collect-b "sk-b"}})

;; :agent-defaults is a whole non-map here. A non-map ENTRY such as
;; {:explore "x"} is returned as-is by agent-type-defaults, outside its
;; [:maybe :map] output contract; that is a config-shape gap, not driven here.
(def ^:private malformed
  {:agent-defaults        "not-a-map"
   :llm-provider-priority :anthropic
   :llm-providers         [:not-a-map]
   :secrets               {}})

(defn- read-all []
  {:defaults-kw     (collect/agent-type-defaults :explore)
   :defaults-str    (collect/agent-type-defaults "coder")
   :defaults-none   (collect/agent-type-defaults :missing)
   :priority        (collect/config-priority)
   :providers       (collect/config-providers)
   :present         (collect/present-secret-keys [:f02-collect-a :f02-collect-b :f02-collect-zz])
   :present-set     (collect/present-secret-keys #{:f02-collect-zz})
   :secret          (collect/secret-value :f02-collect-a)
   :secret-missing  (collect/secret-value :f02-collect-zz)})

(deftest-golden-fn provider-collect-golden
  "test/golden/agent/provider-collect.edn"
  (fn []
    {:populated (binding [config/*config-source* (source/static-source populated)] (read-all))
     :malformed (binding [config/*config-source* (source/static-source malformed)] (read-all))}))

(deftest nil-inputs-test
  (binding [config/*config-source* (source/static-source populated)]
    (is (nil? (collect/agent-type-defaults nil)))
    (is (= #{} (collect/present-secret-keys nil)))))
