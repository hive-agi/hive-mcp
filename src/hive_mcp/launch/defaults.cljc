(ns hive-mcp.launch.defaults
  "The `:launch` defaults: what bin/hive-mcp does today when nothing is set.

   LC0 makes NO behaviour change. This namespace states today's script values
   as DATA, so LC1's planner can fall back to them and a test can prove the
   table still matches the script (`defaults` validates against
   `hive-mcp.launch.schema/LaunchConfig`; bin/hive-mcp-launcher-test.sh keeps
   the shell honest).

   Every value below was read out of bin/hive-mcp and bin/oom-policy.sh:

     HIVE_MCP_XMX             10g              -J-Xmx
     HIVE_MCP_XMS             2g               -J-Xms
     HIVE_MCP_SOFT_MAX_HEAP   8g               -XX:SoftMaxHeapSize
     MALLOC_ARENA_MAX         4                exported, not a -J flag
     HIVE_MCP_COREDUMP        0 -> false       ulimit -c + CreateCoredumpOnCrash
     HIVE_MCP_OOM_SCORE_ADJ   -500             oom_adj_set
     HIVE_MCP_OOMD_PREFERENCE avoid            systemd user scope
     HIVE_MCP_RESTART_LOCK    on -> true       flock on fd 9
     HIVE_MCP_SUPERVISE       1 -> true        oom_supervise
     HIVE_MCP_MAX_RESTARTS    5                oom_supervise
     HIVE_MCP_RESTART_WINDOW  600              oom_supervise
     HIVE_FAST_BOOT           unset -> false   the :mcp:fast-boot alias
     profile                  unset -> nil     config.edn decides, else desktop

   PATH DEFAULTS ARE NOT LITERALS. bin/hive-mcp resolves them at run time:

     heap dumps    ${XDG_STATE_HOME:-$HOME/.local/state}/hive-mcp/heapdumps
     restart lock  ${XDG_DATA_HOME:-$HOME/.local/share}/hive-mcp/restart-PORT.lock

   Writing either as a string would bake one host's home directory into a
   shareable launcher, so they are DESCRIPTORS: {:xdg :state :suffix .}. LC1
   resolves one against the environment it was handed."
  (:require [hive-mcp.launch.schema :as schema]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def heap-dump-dir
  "Where an OOM heap dump lands, as a descriptor rather than a path.

   bin/hive-mcp: ${XDG_STATE_HOME:-$HOME/.local/state}/hive-mcp/heapdumps.
   :xdg names the XDG base-directory variable LC1 reads (XDG_STATE_HOME);
   :fallback is the path under HOME when that variable is unset. Never a
   literal home directory."
  {:xdg :state
   :fallback [".local" "state"]
   :suffix "hive-mcp/heapdumps"})

(def restart-lock-file
  "The restart-lock path, as a descriptor.

   bin/hive-mcp: ${XDG_DATA_HOME:-$HOME/.local/share}/hive-mcp/restart-PORT.lock.
   :port marks the segment LC1 fills from the effective nREPL port, so a
   second instance on another port is not blocked by the first."
  {:xdg :data
   :fallback [".local" "share"]
   :suffix "hive-mcp/restart-"
   :port true
   :suffix-after-port ".lock"})

(def defaults
  "The `:launch` map bin/hive-mcp implies when the config says nothing.

   Validates against schema/LaunchConfig. The two path entries are
   descriptors, so a consumer expecting a resolved string must call LC1's
   resolver; nothing in LC0 resolves anything."
  {:jvm {:xmx "10g"
         :xms "2g"
         :soft-max-heap "8g"
         :malloc-arena-max 4
         :extra-opts []}
   :profile nil
   :deps {}
   :workspace nil
   :presets []
   :extra-paths []
   :aliases {}
   :fast-boot? false
   :forensics {:coredump? false
               :heap-dump-dir heap-dump-dir}
   :oom {:score-adj -500
         :oomd-preference :avoid}
   :restart-lock {:enabled? true
                  :file restart-lock-file
                  :by nil}
   :supervise {:enabled? true
               :max-restarts 5
               :window-s 600}
   ;; bin/hive-mcp defaults the nREPL port to 7910 only INSIDE the lock file
   ;; name (restart-${HIVE_MCP_NREPL_PORT:-7910}.lock); the server reads its
   ;; own default. nil keeps that: the launcher has no opinion of its own.
   :nrepl-port nil})

(defn validate-defaults
  "`defaults` is a valid :launch section. Answers true, or a malli
   explanation when it is not (which would be a bug in this namespace)."
  []
  (or (schema/explain defaults) true))

(m/=> validate-defaults [:=> [:cat] [:or [:= true] [:map]]])

(comment
  (validate-defaults) ;=> true
  ;; The descriptor never carries a home directory:
  heap-dump-dir
  ;=> {:xdg :state, :fallback [".local" "state"], :suffix "hive-mcp/heapdumps"}
  )