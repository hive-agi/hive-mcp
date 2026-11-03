(ns hive-mcp.tools.consolidated.hot.branch
  "`hot branch`: the MCP edge of hive-mcp.dispatch.branches, so an agent can extend
   the tool tree it is calling, in the same conversation, with no restart and no
   addon to scaffold.

   The behaviour comes from one of three places, first present wins:
     code    Clojure source; the value of its last form
     symbol  a var, resolved with requiring-resolve and KEPT as a var, so a reload
             of its namespace reaches the branch
     file    a Clojure file to load; alone, the value of its last form; with
             symbol, it is loaded first and then the symbol resolves

   `code` and `file` execute in this JVM with this process's authority. That is
   the same reach `clojure_eval` already gives, and no more, but it is not a
   sandbox: treat a branch as code you are installing.

   A branch registered from here is checked the moment it lands: the target tool
   is asked for `help`, which makes it fold its tree, and a branch that tree
   refuses is withdrawn and answered as an error. So `:ok? true` means it routes."
  (:require [clojure.data.json :as json]
            [clojure.string :as str]
            [hive-mcp.dispatch.branches :as branches]
            [hive-mcp.dispatch.handler :as handler]
            [hive-mcp.tools.core :as tc :refer [mcp-error mcp-json]]
            [malli.core :as m])
  (:import [java.io PushbackReader StringReader]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def default-owner
  "Who a branch belongs to when the caller names nobody."
  "llm")

;; =============================================================================
;; Pure: what a branch answers
;; =============================================================================

(defn coerce-answer
  "R as an MCP response, so a branch written in passing can return plain data. A
   response, or a vector of them, passes untouched; a string is text; anything
   else is JSON."
  [r]
  (cond
    (string? r)                                 {:type "text" :text r}
    (and (map? r) (:type r))                    r
    (and (sequential? r) (seq r) (every? :type r)) r
    :else                                       (mcp-json r)))

(defn- answering
  "H, answering through `coerce-answer`, called through whatever holds it."
  [h]
  (fn [params] (coerce-answer (h params))))

(defn- answering-tree
  "NODE, a handler or a verb map, with every leaf `answering`."
  [node]
  (let [cur (handler/current node)]
    (if (map? cur)
      (into {} (map (fn [[k v]] [(keyword k) (answering-tree v)])) cur)
      (answering node))))

(defn- answering-wrap
  "WRAP, (fn [next] handler), with the handler it builds `answering`."
  [wrap]
  (fn [next-handler] (answering (wrap next-handler))))

;; =============================================================================
;; Boundary: source to value
;; =============================================================================

(def ^:private source-ns 'hive.branch.user)

(defn- read-forms
  "Every form in SOURCE, read with *read-eval* off."
  [source]
  (let [rdr (PushbackReader. (StringReader. source))]
    (binding [*read-eval* false]
      (loop [forms []]
        (let [form (read {:eof ::eof} rdr)]
          (if (= ::eof form) forms (recur (conj forms form))))))))

(defn- eval-source
  "The value of SOURCE's last form, evaluated in a namespace of its own that sees
   clojure.core. `ns-resolve` is not used: a form may `require` what it needs."
  [source]
  (let [target (create-ns source-ns)]
    (binding [*ns* target]
      (refer-clojure)
      (let [forms (read-forms source)]
        (when (empty? forms)
          (throw (ex-info "code holds no forms" {})))
        (reduce (fn [_ form] (eval form)) nil forms)))))

(defn- implementation
  "The value CODE, SYMBOL or FILE names. {:ok v}, or {:error message}."
  [{:keys [code symbol file]}]
  (try
    (cond
      (not (str/blank? code))
      {:ok (eval-source code)}

      (not (str/blank? symbol))
      (do (when-not (str/blank? file) (load-file file))
          {:ok (requiring-resolve (clojure.core/symbol symbol))})

      (not (str/blank? file))
      {:ok (load-file file)}

      :else
      {:error "one of code, symbol or file is required"})
    (catch Throwable t
      {:error (str "could not load the implementation: " (or (ex-message t) (str t)))})))

(defn- schema-props
  "SCHEMA, the params a branch declares, as a map keyed by string. A JSON string
   is accepted, since a client may flatten a nested argument."
  [schema]
  (let [m (if (string? schema) (json/read-str schema) schema)]
    (when (map? m)
      (into {} (map (fn [[k v]] [(if (keyword? k) (name k) (str k)) v])) m))))

;; =============================================================================
;; Boundary: the check that it routes
;; =============================================================================

(def ^:dynamic *tool-handler*
  "(fn [tool-name]) -> the tool's handler, or nil. Read per call; a test binds it."
  (fn [tool-name]
    ((requiring-resolve 'hive-mcp.tools.consolidated.multi/get-tool-handler) tool-name)))

(def ^:private probe-command
  "A command no tool routes. Asking for it makes the tool fold its tree and answer
   `unknown`, and it runs nothing a branch or an owner wrote: not even `help`, which
   an intercepting addon may answer itself without reaching the tree."
  "__branch_probe__")

(defn- fold-now!
  "Have TOOL fold its tree now. :folded when it did; :silent when it answered
   without folding, so it does not dispatch through a tree; :absent when no tool
   goes by that name."
  [tool]
  (branches/forget-fold! tool)
  (if-let [h (*tool-handler* tool)]
    (do (try (h {:command probe-command}) (catch Throwable _ nil))
        (if (branches/folded? tool) :folded :silent))
    :absent))

(defn- land
  "Answer for a registration RESULT of the branch of KIND at PATH on TOOL: what it
   said when refused; an error, withdrawing it, when the tree refused it or the
   tool has no tree; otherwise ok. So ok means it routes, or says why it may not."
  [{:keys [tool path kind owner]} result]
  (if-not (:ok? result)
    (mcp-error (:message result))
    (let [state   (fold-now! tool)
          p       (branches/parse-path path)
          mine?   (fn [{r-kind :kind r-path :path r-owner :owner}]
                    (and (= kind r-kind) (= (branches/owner-id owner) r-owner)
                         (= p (subvec r-path 0 (min (count p) (count r-path))))))
          refused (filterv mine? (branches/refusals tool))]
      (cond
        (seq refused)
        (do (branches/prune! tool path owner)
            (mcp-error (str "'" tool " " (branches/path-str p) "' was not added: "
                            (str/join ", " (distinct (map (comp name :reason) refused)))
                            ". A graft never shadows a route; use `branch around` to change one.")))

        (= :silent state)
        (do (branches/prune! tool path owner)
            (mcp-error (str tool " does not dispatch through a verb tree, so it cannot carry"
                            " branches; the branch was not added. Build its handler with"
                            " composite/build-merged-handler to open it.")))

        (= :absent state)
        (mcp-json (assoc result :warning (str "no consolidated tool named " tool
                                              " answered; the branch stays dormant until one does")))

        :else (mcp-json result)))))

(defn- required-error
  [{:keys [tool path]} path-required?]
  (cond
    (str/blank? tool)                             "tool is required"
    (and path-required? (str/blank? (str path)))  "path is required"))

;; =============================================================================
;; Verbs
;; =============================================================================

(defn- spec-of
  [{:keys [owner summary description schema order]}]
  (cond-> {:owner (or owner default-owner)}
    summary     (assoc :summary summary)
    description (assoc :description description)
    schema      (assoc :params (schema-props schema))
    order       (assoc :order (if (string? order) (parse-long order) order))))

(defn handle-add
  "Graft a handler, or a verb map, at path of tool."
  [{:keys [tool path] :as params}]
  (if-let [e (required-error params true)]
    (mcp-error e)
    (let [{:keys [ok error]} (implementation params)]
      (if error
        (mcp-error error)
        (let [spec (spec-of params)]
          (land {:tool tool :path path :kind :graft :owner (:owner spec)}
                (branches/graft! tool path (assoc spec :handler (answering-tree ok)))))))))

(m/=> handle-add [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-around
  "Wrap path of tool, and everything below it, in (fn [next] handler)."
  [{:keys [tool path] :as params}]
  (if-let [e (required-error params false)]
    (mcp-error e)
    (let [{:keys [ok error]} (implementation params)]
      (cond
        error                       (mcp-error error)
        (not (handler/handler? ok)) (mcp-error "around needs (fn [next] (fn [params] ...))")
        :else
        (let [spec (spec-of params)]
          (land {:tool tool :path path :kind :around :owner (:owner spec)}
                (branches/around! tool path (assoc spec :wrap (answering-wrap ok)))))))))

(m/=> handle-around [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn- row
  [tool {:keys [kind path owner summary order]}]
  (cond-> {:tool tool :kind kind :path (branches/path-str path) :owner owner}
    summary (assoc :summary summary)
    order   (assoc :order order)))

(defn handle-list
  "Every branch, or those of tool, each tool's refusals as of a fresh fold."
  [{:keys [tool]}]
  (let [tools (if (str/blank? tool) (branches/tool-names) [tool])]
    (doseq [t tools] (fold-now! t))
    (mcp-json {:branches (vec (mapcat (fn [t] (map (partial row t) (branches/of t))) tools))
               :refused  (vec (mapcat (fn [t]
                                        (map #(assoc (row t %) :reason (:reason %))
                                             (branches/refusals t)))
                                      tools))})))

(m/=> handle-list [:=> [:cat [:maybe :map]] tc/McpResponse])

(defn handle-remove
  "Withdraw owner's branches at path of tool, or with only owner, all of its."
  [{:keys [tool path owner]}]
  (cond
    (and (str/blank? tool) (str/blank? owner))
    (mcp-error "give tool and path, or owner alone")

    (str/blank? tool)
    (mcp-json {:removed (branches/retract-owner! owner) :owner owner})

    :else
    (let [o (or owner default-owner)
          n (branches/prune! tool (or path []) o)]
      (if (pos? n)
        (mcp-json {:removed n :tool tool :path (str path) :owner o})
        (mcp-error (str "no branch of " o " at '" tool " " path "'"))))))

(m/=> handle-remove [:=> [:cat [:maybe :map]] tc/McpResponse])

(def help-text
  "What `hot branch help` answers."
  (str/join
   "\n"
   ["hot branch: extend or wrap any path of any tool, live."
    ""
    "add     tool path (code|symbol|file) [schema summary owner]"
    "        A handler (fn [params] ...), or a verb map {:a f :b g}, at that path."
    "        It sits beside what the path routes and never shadows a route."
    "        Example: tool=git path=\"belt hello\" code=\"(fn [p] (str \\\"hi \\\" (:name p)))\""
    "                 schema={\"name\":{\"type\":\"string\"}}"
    "around  tool [path] (code|symbol|file) [order schema summary owner]"
    "        (fn [next] (fn [params] ...)) over one verb, a subtree, or the whole"
    "        tool. Call next to continue; do not call it to replace. Lowest order"
    "        runs outermost."
    "list    [tool]             what is registered, and what is refused and why"
    "remove  tool path [owner]  or owner alone for all of its branches"
    ""
    "A handler may return a string or plain data. Params it declares in schema reach"
    "it as keywords; a param nobody declares is dropped by the MCP layer."
    "code and file run in this JVM with its authority. Branches live in memory: an"
    "addon that registers from its initialize! is what survives a restart."]))

(defn handle-help
  "The usage of `hot branch`."
  [_]
  {:type "text" :text help-text})

(m/=> handle-help [:=> [:cat :any] tc/McpResponse])

(def handlers
  "The `hot branch` verbs, by var so a reload reaches them."
  {:_handler #'handle-help
   :help     #'handle-help
   :add      #'handle-add
   :around   #'handle-around
   :list     #'handle-list
   :remove   #'handle-remove})
