(ns hive-mcp.service.unit
  "Pure rendering of the hive-mcp systemd USER unit.

   The unit supervises the SERVING image (`clojure -M:nrepl`, the process that
   binds the nREPL port bb-mcp forwards tool calls to) so a crash is restarted
   instead of leaving every client facing a refused connection. Rendering is
   data -> string; the committed `resources/systemd/hive-mcp.service` is
   `(render-unit default-opts)` and a test pins that equality.

   Installing or enabling the unit is a host action, never done from here:
   enabling it takes over the live serving port."
  (:require [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def UnitOpts
  "Malli schema of the knobs a unit render accepts."
  [:map
   [:working-dir [:enum "%h/PP/hive/hive-mcp" "%h/hive-mcp"]]
   [:exec-start [:enum "/usr/bin/env clojure -M:nrepl"]]
   [:restart-sec [:int {:min 1 :max 60}]]
   [:start-limit-burst [:int {:min 1 :max 10}]]
   [:start-limit-interval-sec [:int {:min 30 :max 3600}]]])

(def default-opts
  "The shipped unit: restart on failure after 5 s, give up after 3 failures in 5 min."
  {:working-dir "%h/PP/hive/hive-mcp"
   :exec-start "/usr/bin/env clojure -M:nrepl"
   :restart-sec 5
   :start-limit-burst 3
   :start-limit-interval-sec 300})

(def RenderOpts
  "The opts `render-unit` accepts: every knob, as text or a non-negative
   count. `UnitOpts` is the narrower set the shipped unit is generated from."
  [:map
   [:working-dir :string]
   [:exec-start :string]
   [:restart-sec [:int {:min 0}]]
   [:start-limit-burst [:int {:min 0}]]
   [:start-limit-interval-sec [:int {:min 0}]]])

(defn render-unit
  "Render the hive-mcp.service text for `opts` (see `UnitOpts`).
   Always `Restart=on-failure`: a clean stop (systemctl stop) stays stopped."
  [{:keys [working-dir exec-start restart-sec start-limit-burst start-limit-interval-sec]}]
  (str "[Unit]\n"
       "Description=hive-mcp serving image (nREPL tool server)\n"
       "StartLimitIntervalSec=" start-limit-interval-sec "\n"
       "StartLimitBurst=" start-limit-burst "\n"
       "\n"
       "[Service]\n"
       "Type=simple\n"
       "Environment=PATH=/home/linuxbrew/.linuxbrew/bin:/usr/local/bin:/usr/bin:/bin\n"
       "WorkingDirectory=" working-dir "\n"
       "ExecStart=" exec-start "\n"
       "Restart=on-failure\n"
       "RestartSec=" restart-sec "\n"
       "\n"
       "[Install]\n"
       "WantedBy=default.target\n"))

(m/=> render-unit [:=> [:cat RenderOpts] [:string {:min 1}]])
