(ns hive-mcp.prompts.infra
  "Infrastructure adapters for the prompts/permissions domain.
   
   Provides:
   - Desktop notifications via D-Bus (org.freedesktop.Notifications)
   - WebSocket channel emission for Emacs integration
   
   Uses lambdaisland/dbus-client for native Clojure D-Bus communication."
  (:require [lambdaisland.dbus.client :as dbus]
            [hive-mcp.channel :as channel]
            [taoensso.timbre :as log]))

(defonce ^:private dbus-client (atom nil))

(defn- ensure-dbus-client!
  "Lazily initialize the D-Bus session client."
  []
  (when-not @dbus-client
    (try
      (reset! dbus-client
              (dbus/init-client!
               (dbus/session-sock)
               (fn [v] (log/debug "D-Bus reply:" v))))
      (catch Exception e
        (log/warn e "Failed to initialize D-Bus client - notifications disabled")
        nil))))

(defn notify!
  "Send desktop notification via D-Bus org.freedesktop.Notifications.
   
   Options:
     :urgency  - 0=low, 1=normal (default), 2=critical
     :app-name - Application name (default: \"hive-mcp\")
     :icon     - Icon name or path (default: \"\")
     :timeout  - Timeout in ms, -1 for default (default: -1)
   
   Returns promise of notification ID, or nil if D-Bus unavailable."
  [title body & {:keys [urgency app-name icon timeout]
                 :or {urgency 1 app-name "hive-mcp" icon "" timeout -1}}]
  (ensure-dbus-client!)
  (when @dbus-client
    (try
      @(dbus/write-message
        @dbus-client
        {:type :method-call
         :headers
         {:path "/org/freedesktop/Notifications"
          :member "Notify"
          :interface "org.freedesktop.Notifications"
          :destination "org.freedesktop.Notifications"}
         :body [app-name ; app_name
                0 ; replaces_id (0 = new notification)
                icon ; app_icon
                title ; summary
                body ; body
                [] ; actions
                {"urgency" urgency} ; hints
                timeout]}) ; expire_timeout
      (catch Exception e
        (log/warn e "Failed to send D-Bus notification")
        nil))))

(defn notify-permission-request!
  "Send critical notification for permission request requiring human decision.
   
   Returns notification ID or nil."
  [agent-id tool-name summary]
  (notify! (str "🔐 Permission: " tool-name)
           (str "Agent: " agent-id "\n" summary)
           :urgency 2
           :timeout 0)) ; 0 = never expire

(defn notify-agent-blocked!
  "Send notification when agent is blocked waiting for input."
  [agent-id reason]
  (notify! (str "⏸️ Agent Blocked: " agent-id)
           reason
           :urgency 1))

(defn notify-agent-completed!
  "Send notification when agent completes a task."
  [agent-id task-summary]
  (notify! (str "✅ Agent Completed: " agent-id)
           task-summary
           :urgency 0))

;;; ---------------------------------------------------------------------------
;;; Channel Emission Adapters
;;; ---------------------------------------------------------------------------

(defn channel-emit!
  "Emit an event to WebSocket channel for Emacs integration.
   
   Wraps hive-mcp.channel/emit-event! for the prompts domain.
   
   Arguments:
     event-type - Keyword identifying the event (e.g. :prompt-pending)
     data       - Map of event data
   
   Returns result of channel emission."
  [event-type data]
  (log/debug "Emitting channel event" {:event-type event-type :data data})
  (channel/emit-event! event-type data))

(defn emit-prompt-pending!
  "Emit prompt-pending event when a new prompt requires human decision.
   
   Arguments:
     prompt - Map with :id, :agent-id, :question, :options, :created-at
   
   Notifies Emacs that a prompt is awaiting response."
  [prompt]
  (channel-emit! :prompt-pending
                 {:prompt-id (:id prompt)
                  :agent-id (:agent-id prompt)
                  :question (:question prompt)
                  :options (:options prompt)
                  :created-at (:created-at prompt)}))

(defn emit-prompt-resolved!
  "Emit prompt-resolved event when a prompt has been answered.
   
   Arguments:
     prompt - Map with :id, :agent-id, :response, :status, :resolved-at
   
   Notifies Emacs that the prompt was resolved."
  [prompt]
  (channel-emit! :prompt-resolved
                 {:prompt-id (:id prompt)
                  :agent-id (:agent-id prompt)
                  :response (:response prompt)
                  :status (:status prompt)
                  :resolved-at (:resolved-at prompt)}))

(defn emit-prompt-expired!
  "Emit prompt-expired event when a prompt times out without response.
   
   Arguments:
     prompt - Map with :id, :agent-id, :resolved-at
   
   Notifies Emacs that the prompt expired."
  [prompt]
  (channel-emit! :prompt-expired
                 {:prompt-id (:id prompt)
                  :agent-id (:agent-id prompt)
                  :resolved-at (:resolved-at prompt)}))
