(ns hive-mcp.launch.defaults
  "The `:launch` defaults: the values bin/hive-mcp uses when nothing is set.

   xmx 10g, xms 2g, soft-max-heap 8g, malloc-arena-max 4, coredump false,
   oom score-adj -500 and oomd :avoid, restart lock on, supervise on with 5
   restarts in 600 s, fast-boot false, profile nil.

   The two path defaults are descriptors ({:xdg :state :suffix ...}), not
   literal paths; LC1 resolves them from the environment."
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