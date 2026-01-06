(ns hive-mcp.agent.openrouter
  "OpenRouter backend for agent delegation.
   
   Provides access to 100+ models via OpenRouter API.
   Uses OpenAI-compatible format with tool calling support."
  (:require [hive-mcp.agent.protocol :as proto]
            [clj-http.client :as http]
            [clojure.data.json :as json]
            [taoensso.timbre :as log]))

(def ^:private api-url "https://openrouter.ai/api/v1/chat/completions")

(defn- format-tools
  "Convert MCP tool format to OpenAI function format."
  [tools]
  (when (seq tools)
    (mapv (fn [{:keys [name description inputSchema]}]
            {:type "function"
             :function {:name name
                        :description description
                        :parameters inputSchema}})
          tools)))

(defn- parse-tool-calls
  "Parse OpenAI-format tool calls to internal format."
  [tool-calls]
  (mapv (fn [tc]
          {:id (:id tc)
           :name (get-in tc [:function :name])
           :arguments (json/read-str (get-in tc [:function :arguments]) :key-fn keyword)})
        tool-calls))

(defn- chat-request
  "Make chat completion request to OpenRouter."
  [api-key model messages tools]
  (let [body (cond-> {:model model
                      :messages messages}
               (seq tools) (assoc :tools (format-tools tools)))
        response (http/post api-url
                            {:headers {"Authorization" (str "Bearer " api-key)
                                       "Content-Type" "application/json"
                                       "HTTP-Referer" "https://github.com/BuddhiLW/hive-mcp"}
                             :body (json/write-str body)
                             :as :json})]
    (:body response)))

(defrecord OpenRouterBackend [api-key model]
  proto/LLMBackend

  (chat [_ messages tools]
    (log/debug "OpenRouter chat request" {:model model :msg-count (count messages)})
    (let [response (chat-request api-key model messages tools)
          choice (get-in response [:choices 0 :message])
          tool-calls (:tool_calls choice)]
      (if (seq tool-calls)
        {:type :tool_calls
         :calls (parse-tool-calls tool-calls)}
        {:type :text
         :content (:content choice)})))

  (model-name [_] model))

(defn openrouter-backend
  "Create an OpenRouterBackend.
   
   Options:
     :api-key - OpenRouter API key (required, or set OPENROUTER_API_KEY env)
     :model   - Model to use (default: anthropic/claude-3-haiku)
   
   Popular models:
     anthropic/claude-3-opus, anthropic/claude-3-sonnet, anthropic/claude-3-haiku
     openai/gpt-4-turbo, openai/gpt-3.5-turbo
     mistralai/mistral-large, mistralai/mixtral-8x7b
     meta-llama/llama-3-70b-instruct"
  [{:keys [api-key model] :or {model "anthropic/claude-3-haiku"}}]
  (let [key (or api-key (System/getenv "OPENROUTER_API_KEY"))]
    (when-not key
      (throw (ex-info "OpenRouter API key required" {:env "OPENROUTER_API_KEY"})))
    (->OpenRouterBackend key model)))
