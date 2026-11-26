# The `:launch` section

Today hive-mcp boots from three places at once: `bin/hive-mcp` (219 lines of
bash), a gitignored `local.deps.edn` full of `:local/root` siblings, and a pile
of `HIVE_MCP_*` environment variables. `bin/hive-mcp` resolves the profile, the
deps overlay, the AOT fast-boot alias, the heap flags, the crash-forensics
paths, the OOM standing, the restart lock and the OOM supervisor — in bash,
where none of it can be tested without starting a server.

The `:launch` section in `~/.config/hive-mcp/config.edn` replaces that with
data. One per person, in the config file they already have, so hive-mcp can be
handed to someone else as a binary plus a config rather than as a checkout.

```edn
{:launch
 {:jvm {:xmx "24g" :xms "4g" :soft-max-heap "20g"}
  :profile :k8s-headless
  :workspace "~/PP/hive"
  :presets [:starter]
  :supervise {:max-restarts 3}
  :oom {:oomd-preference :omit}}}
```

**This page documents LC0, the data foundation. Nothing reads `:launch` yet.**
LC0 adds the malli schema (`hive-mcp.launch.schema`), the defaults
(`hive-mcp.launch.defaults`) and the environment-knob table
(`hive-mcp.launch.knobs`), all pure and all testable under babashka with
`bb test:launch`. LC1 turns a config plus the environment into a plan, LC2 turns
`:deps` into the `-Sdeps` overlay, and LC3 makes `bin/hive-mcp` run the plan —
after which every knob on this page can be set in the config instead of in the
shell.

## Precedence

```
CLI flag  >  environment variable  >  :launch in config.edn  >  built-in default
```

The environment layer is the table below: every variable works exactly as it
does now, so an existing setup keeps working unchanged after LC3. A value that
does not parse is ignored (and reported on stderr), which is what the shell's
`${VAR:-default}` does with an empty value — a typo degrades to the default
rather than stopping the server from starting.

## Knobs

One row per environment variable `bin/hive-mcp` (or the `bin/oom-policy.sh` it
sources) reads today. `:launch path` is where the same value lives in
`config.edn`, rooted at `:launch`.

| Environment variable | `:launch` path | Type | Default | Meaning |
| --- | --- | --- | --- | --- |
| `HIVE_PROFILE` | `:profile` | keyword or nil | `nil` | Server profile. When unset, `config.edn`'s `[:system :profile]` decides, else `desktop`. The `--profile` flag also overrides it. |
| `HIVE_FAST_BOOT` | `:fast-boot?` | boolean | `false` | Use the AOT classes in `target/boot-classes` (alias `:mcp:fast-boot`) so constructor namespaces load as `.class` files. **Declines when the stamp is missing or stale** — it asks for a faster boot, never a different program. |
| `HIVE_MCP_XMX` | `:jvm :xmx` | size | `10g` | JVM max heap (`-J-Xmx`). |
| `HIVE_MCP_XMS` | `:jvm :xms` | size | `2g` | JVM initial heap (`-J-Xms`). |
| `HIVE_MCP_SOFT_MAX_HEAP` | `:jvm :soft-max-heap` | size | `8g` | `-XX:SoftMaxHeapSize`: the JVM collects sooner above this instead of running the heap to `-Xmx`. |
| `MALLOC_ARENA_MAX` | `:jvm :malloc-arena-max` | integer | `4` | glibc malloc arena cap, exported to the JVM. Fewer arenas means less native memory and less fragmentation: RSS runs about heap plus 1.5–2.5 GB. |
| `HIVE_MCP_COREDUMP` | `:forensics :coredump?` | boolean | `false` | Raise `ulimit -c` to the hard limit for this JVM so a native crash (SIGSEGV) leaves a core. `-XX:+CreateCoredumpOnCrash` is always on. See `coredumpctl list java`. |
| `HIVE_MCP_HEAP_DUMP_DIR` | `:forensics :heap-dump-dir` | path | `${XDG_STATE_HOME:-$HOME/.local/state}/hive-mcp/heapdumps` | Where an OOM heap dump (`-XX:HeapDumpPath`) lands. Without it the JVM writes an `-Xmx`-sized `.hprof` into whatever the working directory was, and the next boot cannot prove it left none. |
| `HIVE_MCP_OOM_SCORE_ADJ` | `:oom :score-adj` | integer | `-500` | Kernel `oom_score_adj` for this process tree, inherited by the JVM. Lowering needs `CAP_SYS_RESOURCE`; unprivileged it stays put, which is why test JVMs *raise* theirs instead. |
| `HIVE_MCP_OOMD_PREFERENCE` | `:oom :oomd-preference` | `:avoid`, `:omit`, `:none`, `:off` | `:avoid` | `ManagedOOMPreference` for the systemd user scope the server runs in. systemd-oomd ignores `oom_score_adj` and kills whole cgroups, so the scope is its own lever. `:off` runs in the caller's cgroup. |
| `HIVE_MCP_RESTART_LOCK` | `:restart-lock :enabled?` | boolean | `true` | Take a non-blocking `flock` on the lock file, held through `exec` for the life of the JVM. A second launcher prints the holder and exits 75 instead of starting a second server. `off` skips it. |
| `HIVE_MCP_RESTART_LOCK_FILE` | `:restart-lock :file` | path | `${XDG_DATA_HOME:-$HOME/.local/share}/hive-mcp/restart-<port>.lock` | The lock file. The port comes from `HIVE_MCP_NREPL_PORT`, so a second instance on another port is not blocked. Holder and `restarts.log` sit beside it. |
| `HIVE_MCP_RESTART_BY` | `:restart-lock :by` | string | `ppid <pid> (<comm>)` | Who is launching, recorded in the holder file and `restarts.log`. hivemind lives inside the JVM being restarted, so this out-of-band file is how everyone learns who is restarting and why. |
| `HIVE_MCP_NREPL_PORT` | `:nrepl-port` | integer | `nil` (server reads its own default, `7910`) | nREPL port. The launcher only uses it to name the restart-lock file; the server component reads the same variable. |
| `HIVE_MCP_SUPERVISE` | `:supervise :enabled?` | boolean | `true` | Restart the JVM after an OOM death (exit 137, or 3 from `-XX:+ExitOnOutOfMemoryError`). `false` gives one launch, `exec`-shaped, which is what an unsupervised caller (systemd, docker) wants. |
| `HIVE_MCP_MAX_RESTARTS` | `:supervise :max-restarts` | integer | `5` | How many OOM restarts are allowed inside the window before the supervisor gives up and returns the exit status. |
| `HIVE_MCP_RESTART_WINDOW` | `:supervise :window-s` | integer | `600` | The window, in seconds, those restarts are counted over. |

`HIVE_MCP_CONFIG` is deliberately **not** in the table: it selects *which*
`config.edn` is read, so it is consumed before a `:launch` section exists and
has no path inside one.

## Path defaults are not paths

`bin/hive-mcp` resolves the heap-dump directory and the lock file from
`XDG_STATE_HOME` / `XDG_DATA_HOME`, falling back to `$HOME/.local/...`. Writing
either as a string would bake one person's home directory into a launcher other
people run, so `hive-mcp.launch.defaults` states them as **descriptors**:

```clojure
{:xdg :state  :fallback [".local" "state"]  :suffix "hive-mcp/heapdumps"}
{:xdg :data   :fallback [".local" "share"]  :suffix "hive-mcp/restart-"
 :port true   :suffix-after-port ".lock"}
```

LC1 resolves one against the environment it was handed; a test asserts that no
default in the map contains `/home/`, `~/` or `/Users/`.

## Checking a change

```bash
bb test:launch
```

That runs `hive-mcp.launch.schema-test` and `hive-mcp.launch.knobs-test` under
babashka: no JVM, no server, no backend driver. It asserts the defaults are
still the values in `bin/hive-mcp`, every knob names a key the schema declares,
every knob has a parser, and `parse-knob` is total over junk input. The same
namespaces run in the JVM suite, because they are ordinary `clojure.test`
namespaces in `test/`.
