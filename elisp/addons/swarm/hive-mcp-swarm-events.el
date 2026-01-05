;;; hive-mcp-swarm-events.el --- Channel event emission for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Centralized channel event emission for hive-mcp-swarm.
;; All push-based updates flow through this module.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles event emission
;; - Open/Closed: New event types via new functions, not modification
;; - Dependency Inversion: Callers depend on emit API, not channel internals
;;
;; Event types emitted:
;; - slave-spawned: New slave created
;; - slave-killed: Slave terminated
;; - task-completed: Task finished successfully
;; - task-failed: Task failed or timed out
;; - prompt-shown: Permission prompt detected (human mode)
;; - state-changed: Slave status transition

;;; Code:

(require 'cl-lib)

;; Soft dependency on channel
(declare-function hive-mcp-channel-connected-p "hive-mcp-channel")
(declare-function hive-mcp-channel-send "hive-mcp-channel")

;;;; Internal State:

(defvar hive-mcp-swarm-events--session-id nil
  "Current session ID for event correlation.")

;;;; Channel Availability:

(defun hive-mcp-swarm-events-channel-available-p ()
  "Check if the bidirectional channel is available and connected."
  (and (require 'hive-mcp-channel nil t)
       (fboundp 'hive-mcp-channel-connected-p)
       (hive-mcp-channel-connected-p)))

;;;; Core Event Emission:

(defun hive-mcp-swarm-events-emit (event-type data)
  "Emit EVENT-TYPE with DATA through the channel if connected.
EVENT-TYPE should be a string like \"task-completed\".
DATA is an alist of additional event properties."
  (when (hive-mcp-swarm-events-channel-available-p)
    (let ((event `(("type" . ,event-type)
                   ("timestamp" . ,(truncate (float-time)))
                   ("session-id" . ,hive-mcp-swarm-events--session-id)
                   ,@data)))
      (condition-case err
          (hive-mcp-channel-send event)
        (error
         (message "[swarm-events] Channel emit error: %s"
                  (error-message-string err)))))))

;;;; Typed Event Emitters:

(defun hive-mcp-swarm-events-emit-slave-spawned (slave-id name presets)
  "Emit slave-spawned event for SLAVE-ID with NAME and PRESETS."
  (hive-mcp-swarm-events-emit
   "slave-spawned"
   `(("slave-id" . ,slave-id)
     ("name" . ,name)
     ("presets" . ,(or presets [])))))

(defun hive-mcp-swarm-events-emit-slave-killed (slave-id)
  "Emit slave-killed event for SLAVE-ID."
  (hive-mcp-swarm-events-emit
   "slave-killed"
   `(("slave-id" . ,slave-id))))

(defun hive-mcp-swarm-events-emit-task-completed (task-id slave-id result)
  "Emit task-completed event for TASK-ID from SLAVE-ID with RESULT."
  (hive-mcp-swarm-events-emit
   "task-completed"
   `(("task-id" . ,task-id)
     ("slave-id" . ,slave-id)
     ("result" . ,result))))

(defun hive-mcp-swarm-events-emit-task-failed (task-id slave-id error-msg)
  "Emit task-failed event for TASK-ID from SLAVE-ID with ERROR-MSG."
  (hive-mcp-swarm-events-emit
   "task-failed"
   `(("task-id" . ,task-id)
     ("slave-id" . ,slave-id)
     ("error" . ,error-msg))))

(defun hive-mcp-swarm-events-emit-prompt-shown (slave-id prompt-text)
  "Emit prompt-shown event for SLAVE-ID with PROMPT-TEXT."
  (hive-mcp-swarm-events-emit
   "prompt-shown"
   `(("slave-id" . ,slave-id)
     ("prompt" . ,prompt-text))))

(defun hive-mcp-swarm-events-emit-state-changed (slave-id old-state new-state)
  "Emit state-changed event for SLAVE-ID from OLD-STATE to NEW-STATE."
  (hive-mcp-swarm-events-emit
   "state-changed"
   `(("slave-id" . ,slave-id)
     ("old-state" . ,(symbol-name old-state))
     ("new-state" . ,(symbol-name new-state)))))

;;;; Session Management:

(defun hive-mcp-swarm-events-set-session-id (session-id)
  "Set the SESSION-ID for event correlation."
  (setq hive-mcp-swarm-events--session-id session-id))

(defun hive-mcp-swarm-events-get-session-id ()
  "Get the current session ID."
  hive-mcp-swarm-events--session-id)

;;;; Lifecycle:

(defun hive-mcp-swarm-events-init (session-id)
  "Initialize events module with SESSION-ID."
  (setq hive-mcp-swarm-events--session-id session-id))

(defun hive-mcp-swarm-events-shutdown ()
  "Shutdown events module."
  (setq hive-mcp-swarm-events--session-id nil))

(provide 'hive-mcp-swarm-events)
;;; hive-mcp-swarm-events.el ends here
