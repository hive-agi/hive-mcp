(ns hive-mcp.tools.cli
  "CLI-style subcommand dispatcher for consolidated tools.

   Supports n-depth nested handler trees via parse-command + resolve-handler.
   Single-word commands remain backward compatible."
  (:require [hive-mcp.tools.core :refer [mcp-error]]
            [clojure.data.json :as json]
            [clojure.string :as str]
            [taoensso.timbre :as log]
            [hive-mcp.dispatch.handler :as dispatch]))

;; =============================================================================
;; Command Normalization
;; =============================================================================

(defn- normalize-command
  "Normalize command to string. Handles keyword, string, or nil."
  [command]
  (cond
    (keyword? command) (name command)
    (string? command) command
    :else nil))

;; =============================================================================
;; Help Formatting (supports nested trees)
;; =============================================================================

(defn- collect-command-paths
  "Collect all command paths from a handler tree.
   Returns a seq of keyword vectors like [[:status] [:status :list]].
   Skips :_handler entries but includes parent path when :_handler exists.

   Classifies each value through `dispatch/current`, so a var-registered
   handler is LISTED rather than silently dropped. Before that it fell through
   to the `:else acc` arm — help omitted the command and nothing said why,
   which is the quiet half of the cost of registering by var."
  [handlers prefix]
  (reduce-kv
   (fn [acc k v]
     (if (= k :_handler)
       acc
       (let [node (dispatch/current v)]
         (cond
           (dispatch/handler? v)
           (conj acc (conj prefix k))

           (map? node)
           (let [nested (collect-command-paths node (conj prefix k))
                 ;; If subtree has _handler, also list the parent path as valid
                 with-default (if (contains? node :_handler)
                                (into [(conj prefix k)] nested)
                                nested)]
             (into acc with-default))

           :else acc))))
   [] (dispatch/current handlers)))

(defn format-help
  "Format help text listing all available commands.
   Supports nested handler trees - shows full command paths."
  [handlers]
  (let [paths (collect-command-paths handlers [])
        sorted (sort-by #(str/join " " (map name %)) paths)]
    (str "Available commands:\n"
         (str/join "\n" (map #(str "  - " (str/join " " (map name %)))
                             sorted)))))

;; =============================================================================
;; Command Parsing
;; =============================================================================

(defn parse-command
  "Parse command string into keyword path.
   \"status list\" → [:status :list]
   \"spawn\" → [:spawn]"
  [command]
  (when (and command (not (str/blank? command)))
    (->> (str/split (str/trim command) #"\s+")
         (mapv keyword))))

;; =============================================================================
;; Handler Resolution (n-depth tree walking)
;; =============================================================================

(defn resolve-handler
  "Resolve handler from nested tree given command path.
   Returns {:handler fn :path-used [...] :remaining [...]}

   Supports:
   - Leaf handlers (fn, multimethod, or a VAR holding one)
   - Nested maps with :_handler for defaults
   - Partial matches falling back to :_handler

   Every node is CLASSIFIED through `dispatch/current` and RETURNED unresolved.
   The distinction is the whole point: a var holding a subtree must be seen as
   a map so the walk descends into it, while a var holding a handler must be
   handed on AS THE VAR, because dereferencing it here would re-freeze the
   value one call before invocation and undo the seam
   (20260817195749-0d407e9c)."
  [handlers path]
  (loop [tree handlers
         used []
         remaining path]
    (let [node (dispatch/current tree)]
      (cond
        ;; No more path - check for _handler or return tree
        (empty? remaining)
        (if-let [h (or (when (dispatch/handler? tree) tree)
                       (get node :_handler))]
          {:handler h :path-used used :remaining []}
          {:tree node :path-used used})

        ;; Try next segment
        :else
        (let [seg       (first remaining)
              next-node (or (get node seg)
                            (when (keyword? seg)
                              (get node (name seg)))
                            (when (string? seg)
                              (get node (keyword seg))))]
          (cond
            ;; Leaf handler found
            (dispatch/handler? next-node)
            {:handler next-node :path-used (conj used seg) :remaining (vec (rest remaining))}

            ;; Subtree found - recurse
            (map? (dispatch/current next-node))
            (recur next-node (conj used seg) (rest remaining))

            ;; Not found - check for _handler fallback
            :else
            (if-let [default (get node :_handler)]
              {:handler default :path-used used :remaining (vec remaining)}
              {:error :not-found :path-used used :remaining (vec remaining)})))))))

;; =============================================================================
;; CLI Handler Factory (n-depth dispatch)
;; =============================================================================

(defn- delegating-subdomains
  "Root keys whose subcommands this tree CANNOT enumerate — they hand the REST
   of the command to another dispatcher.

   Two sources, unioned:
     - a node carrying :_handler (core-folded subdomains)
     - a key listed under ::opaque-roots in the tree's metadata (addon
       contributions, marked by hive-mcp.tools.composite)

   A plain leaf fn with no metadata marker is NOT delegating. Returns a vector
   sorted by name. Subtrees are recognised through `dispatch/current` for the
   same reason the walk is: a var holding a map is a map."
  [handlers]
  (let [marked (set (::opaque-roots (meta handlers)))]
    (->> handlers
         (keep (fn [[k v]]
                 (let [node (dispatch/current v)]
                   (when (or (and (map? node) (contains? node :_handler))
                             (contains? marked k))
                     k))))
         (sort-by name)
         vec)))

(def ^:private max-subdomain-hints
  "Upper bound on the last-resort subdomain list — the stage that can only say
   'one of these routes its own subcommands'. The `code` tool has 5."
  6)

(defn- qualification-hints
  "Root keys to offer as CMD's qualifying subdomain, most precise first.

   Cascade, the first non-empty stage wins and is named in :stage:
     :exact     roots whose ENUMERABLE subtree registers CMD exactly (all of them)
     :lead      opaque roots whose name is CMD's leading segment under separator
                folding (`carto_definition` -> `carto`)
     :fallback  every opaque root, capped at `max-subdomain-hints`

   Returns {:roots [kw ...] :exact? bool :stage kw}; :roots is empty when the
   tree offers nothing. :exact? is true only for :exact."
  [cmd handlers]
  (let [opaque  (delegating-subdomains handlers)
        cmd-low (str/lower-case cmd)
        owners  (->> (collect-command-paths handlers [])
                     (filter #(and (> (count %) 1)
                                   (= cmd-low
                                      (str/lower-case
                                       (str/join " " (map name (rest %)))))))
                     (map first)
                     distinct
                     (sort-by name)
                     vec)
        lead    (first (remove str/blank? (str/split cmd-low #"[-_\\s]+")))
        by-lead (when (and lead (not= lead cmd-low))
                  (vec (filter #(= lead (str/lower-case (name %))) opaque)))]
    (cond
      (seq owners)  {:roots owners  :exact? true  :stage :exact}
      (seq by-lead) {:roots by-lead :exact? false :stage :lead}
      :else         {:roots (vec (take max-subdomain-hints opaque))
                     :exact? false
                     :stage :fallback})))

(defn- auto-qualified-command
  "CMD spelled under the ONE root that can own it, or nil.

   Only a root the tree can name qualifies: an enumerable root that registers
   CMD, or the opaque root CMD's leading segment spells (`carto_callers` ->
   `carto carto_callers`). Several candidates, or the fallback list of every
   opaque root, is a guess and stays an error."
  [cmd handlers]
  (let [{:keys [roots stage]} (qualification-hints cmd handlers)]
    (when (and (contains? #{:exact :lead} stage) (= 1 (count roots)))
      (str (name (first roots)) " " cmd))))

(defn- nearest-commands
  "Command paths in HANDLERS within Levenshtein distance
   `(max 1 (quot (count cmd) 3))` of CMD, closest first, at most 3.
   Returns a vector of space-joined path strings; empty when nothing is close."
  [cmd handlers]
  (letfn [(dist [a b]
            (let [m (count a) n (count b)]
              (loop [i 0 row (vec (range (inc n)))]
                (if (= i m)
                  (peek row)
                  (recur (inc i)
                         (loop [j 0 prev (inc i) acc [(inc i)]]
                           (if (= j n)
                             acc
                             (let [c (min (inc prev)
                                          (inc (nth row (inc j)))
                                          (+ (nth row j)
                                             (if (= (nth a i) (nth b j)) 0 1)))]
                               (recur (inc j) c (conj acc c))))))))))]
    (let [budget  (max 1 (quot (count cmd) 3))
          cmd-low (str/lower-case cmd)]
      (->> (collect-command-paths handlers [])
           (map #(str/join " " (map name %)))
           (map (fn [s] [s (dist cmd-low (str/lower-case s))]))
           (filter #(<= (long (second %)) (long budget)))
           (sort-by second)
           (take 3)
           (mapv first)))))

(defn- unknown-command-error
  "Loud error for a command this tree cannot resolve. Names the valid roots;
   names the subdomains that could own the token — exactly, when this tree can
   enumerate them, otherwise the roots that route their own subcommands; and
   names the nearest known commands by edit distance."
  [command handlers]
  (let [raw   (normalize-command command)
        cmd   (when (and raw (not (str/blank? raw))) (str/trim raw))
        hints (when cmd (qualification-hints cmd handlers))
        roots (:roots hints)
        near  (when cmd (nearest-commands cmd handlers))]
    (mcp-error (str "Unknown command: " command
                    ". Valid: " (str/join ", " (sort (map name (keys handlers))))
                    (when (seq roots)
                      (str (if (:exact? hints)
                             (str ". '" cmd "' is a SUBCOMMAND — qualify it: ")
                             (str ". If '" cmd "' is a SUBCOMMAND, qualify it with "
                                  "its subdomain — one of these routes its own "
                                  "subcommands: "))
                           (str/join " | " (map #(str (name %) " " cmd) roots))))
                    (when (seq near)
                      (str ". Did you mean: " (str/join ", " near) "?"))))))

(defn make-cli-handler
  "Create a CLI-style handler that dispatches on :command param.

   handlers: map of keyword -> handler-fn (flat) or nested handler tree, or —
   preferred in this repo — the VAR holding one. The var spelling is what makes
   a consolidated tool's ROOT reload-transparent: the returned closure then
   holds an indirection rather than the map that existed when the consolidated
   namespace last loaded.

   The root is resolved ONCE, inside the returned fn, and every inspector below
   is handed the MAP. That placement is the whole design. Resolving it in the
   outer `let` would re-freeze the root, which is the capture this exists to
   undo (20260817195749-0d407e9c); resolving it per inspector instead would ask
   each of `format-help`, `unknown-command-error`, `qualification-hints`,
   `nearest-commands` and `auto-qualified-command` to know about vars, and the
   one that was forgotten would fail as a missing command rather than as a type
   error. `resolve-handler` derefs nodes further down on its own, because a
   SUBTREE may be behind a var too.

   Supports n-depth command dispatch: \"status list\" walks {:status {:list fn}}.
   Single-word commands remain backward compatible.

   Optional coerce-schema: map of {field-key [type-spec]} for MCP boundary coercion.
   When provided, string params are coerced to declared types before dispatch.
   See hive-dsl.coerce/coerce-map for type-spec syntax.

   A command the tree cannot resolve is first re-spelled under the ONE root
   that can own it (`auto-qualified-command`): `carto_callers` dispatches as
   `carto carto_callers`, and the handler sees the qualified :command. When no
   single owner exists the error names the valid roots, the subdomains that
   could own the token, and the nearest known commands by edit distance. Roots
   addons contributed are treated as opaque via the ::opaque-roots key in
   `handlers` metadata (written by hive-mcp.tools.composite).

   Returns: fn that dispatches to appropriate handler"
  ([handlers] (make-cli-handler handlers nil))
  ([handlers coerce-schema]
   (let [coerce-fn (when coerce-schema
                     (requiring-resolve 'hive-dsl.coerce/coerce-map))
         run       (fn [handler params]
                     (if coerce-fn
                       (let [coerced (coerce-fn coerce-schema params)]
                         (if (:ok coerced)
                           (handler (:ok coerced))
                           (mcp-error (str "Parameter error: " (:message coerced)))))
                       (handler params)))]
     (fn [{:keys [command] :as params}]
       (let [handlers (dispatch/current handlers)
             cmd-str  (normalize-command command)
             path     (parse-command cmd-str)]
         (cond
           ;; No command or empty -> error
           (nil? path)
           (unknown-command-error command handlers)

           ;; Help at root level
           (= [:help] path)
           {:type "text" :text (format-help handlers)}

           ;; Normal dispatch via n-depth resolve-handler
           :else
           (if-let [handler (:handler (resolve-handler handlers path))]
             (run handler params)
             (let [qualified (auto-qualified-command (str/trim cmd-str) handlers)
                   owner     (when qualified
                               (:handler (resolve-handler handlers (parse-command qualified))))]
               (if owner
                 (run owner (assoc params :command qualified))
                 (unknown-command-error command handlers))))))))))

;; =============================================================================
;; Batch Handler Factory (generic batch middleware)
;; =============================================================================

(def ^:private deprecation-warned (atom #{}))

(defn- warn-deprecation-once
  "Emit a one-shot deprecation warning per handler-map identity. Avoids
   log-flooding when a single make-batch-handler is invoked thousands of
   times. Identity is a hash of `(keys handlers)` since the closure itself
   isn't a stable comparison key."
  [handlers]
  (let [k (hash (sort (map name (keys handlers))))]
    (when-not (contains? @deprecation-warned k)
      (swap! deprecation-warned conj k)
      (taoensso.timbre/warn
       "[deprecated] make-batch-handler iterates per-op (N+1 store calls)."
       "Migrate to an explicit hive-mcp.batch.protocol/Batchable record,"
       "registered via :multi/batchable in IAddon (hooks). See"
       "hive-mcp.multi.batchables/{memory,kg,kanban}-batchable for the pattern."
       {:commands (sort (map name (keys handlers)))}))))

(defn- mcp-error-result?
  "True when a handler result is an MCP error envelope (delivered, but failed)."
  [result]
  (or (and (map? result) (true? (:isError result)))
      (and (map? result) (some? (:error result)))))

(defn make-batch-handler
  "Higher-order function: takes a handlers map (same as make-cli-handler),
   returns a handler that accepts {:operations [{:command ... :param1 ...}, ...], :parallel bool}.
   Maps each operation through existing dispatch, collects all results (no fail-fast).

   Shared params from the outer call (minus :operations/:parallel/:command) merge
   with per-op params. Per-op params win on conflict.

   Sequential by default, :parallel true uses pmap.

   Returns: {:results [...] :summary {:total N :success M :failed F}}

   ─── DEPRECATED ──────────────────────────────────────────────────────
   Per the multi IAddon-native batch architecture (decision
   20260429230453-7e7627cc, T13 Phase 3+), this iterator path is the
   slow fallback. Migrate to an explicit hive-mcp.batch.protocol/Batchable
   record (see hive-mcp.multi.batchables for canonical examples) and
   register it via :multi/batchable in your IAddon (hooks)."
  [handlers]
  (warn-deprecation-once handlers)
  (fn [{:keys [operations parallel] :as params}]
    (if (or (nil? operations) (empty? operations))
      (mcp-error "operations is required (array of {command, ...} objects)")
      (let [shared-params (dissoc params :operations :parallel :command)
            exec-fn       (if parallel pmap mapv)
            results       (exec-fn
                           (fn [op]
                             (try
                               (let [cmd-str (normalize-command (:command op))
                                     path    (parse-command cmd-str)]
                                 (if (nil? path)
                                   {:success false :command (:command op)
                                    :error "Missing or blank command in operation"}
                                   (let [resolved (resolve-handler handlers path)]
                                     (if-let [handler (:handler resolved)]
                                       (let [merged (merge shared-params (dissoc op :command))
                                             result (handler (assoc merged :command (:command op)))
                                             failed? (mcp-error-result? result)]
                                         (cond-> {:success (not failed?)
                                                  :command (:command op)
                                                  :result result}
                                           failed? (assoc :error (or (:error result)
                                                                     "operation returned an error envelope"))))
                                       (if-let [rej (:__rejection__ op)]
                                         {:success false :command (:command op) :error rej}
                                         {:success false :command (:command op)
                                          :error (str "Unknown command: " (:command op))})))))
                               (catch Exception e
                                 {:success false :command (:command op) :error (ex-message e)})))
                           operations)]
        {:type "text"
         :text (json/write-str
                {:results (vec results)
                 :summary {:total   (count operations)
                           :success (count (filter :success results))
                           :failed  (count (remove :success results))}})}))))
