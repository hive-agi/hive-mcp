# Preset: carto-first

Code navigation and structural editing go through `mcp__hive__code` carto. Not grep, not find, not
Read/Write on a `.clj` file.

## Verbs

- `carto search` — find a form by name, symbol, or pattern
- `carto definition` / `carto callers` / `carto callees` — the call graph, both directions
- `carto read-form` / `carto write-form` — read and rewrite ONE form, in place, structurally

## Rules

1. If carto returns nothing for a scope, the index is cold, not the code absent. Run
   `carto scan :scope <dir>` and retry before concluding anything.
2. A structural edit means `write-form` on the target form. Never rewrite a whole namespace file to
   change one function — that is how unrelated forms get clobbered.
3. Raw text tools are legitimate for non-Clojure files: EDN, YAML, Markdown, shell, Dockerfiles.
   They are not legitimate for `.clj` / `.cljc` / `.cljs`.
4. Before you edit a function, look at its callers. An edit that changes arity, return shape, or
   nil-behaviour without inspecting `carto callers` is a guess.

## Edit recipe: make the edit write

1. `carto find-replace`, `raw-splice` and `coll-insert` answer a PREVIEW unless the call asks to
   write. A preview opens with `"notice": "NOTHING WRITTEN: ..."` and carries `"applied?": false`.
   Disk is unchanged.
2. To write with them, pass `apply? true`. The bare `apply true` and `preview? false` mean the same.
3. The edit is done only when the answer says `"applied?": true`. Do not retry a landed write.
4. Exact text in any language, one file:
   `carto find-replace file=<absolute path> find=<exact current text> replace=<new text> apply? true`.
   Zero matches, or several when one was expected, is refused and nothing is written. Widen
   `find` until it is unique, or pass `expect <n>` to replace all n.
5. One whole Clojure form: `carto write-form qn=<ns/name> new-body=<form>`. It writes by default;
   `preview? true` makes it a dry run.

## Why this is not optional

The carto index is the same structural view the rest of the swarm queries. Editing underneath it —
by hand, by text — leaves the index describing code that no longer exists, and every later agent
inherits that lie.
