;;; hive-mcp-swarm-tasks.el --- Task dispatch and collection for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Task dispatch and collection for hive-mcp-swarm.
;; Handles sending prompts to slaves and collecting responses.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles task dispatch/collection
;; - Open/Closed: Response extraction extensible via patterns
;; - Dependency Inversion: Callers depend on dispatch/collect API

;;; Code:

(require 'cl-lib)
(require 'subr-x)

;; Dependencies on sibling modules
(declare-function hive-mcp-swarm-terminal-send "hive-mcp-swarm-terminal")
(declare-function hive-mcp-swarm-slaves-generate-task-id "hive-mcp-swarm-slaves")

;; Dependencies on main module (variables)
(defvar hive-mcp-swarm--slaves)
(defvar hive-mcp-swarm--tasks)
(defvar hive-mcp-swarm-terminal)
(defvar hive-mcp-swarm-default-timeout)

;;;; Buffer Detection Helpers:

(defun hive-mcp-swarm-tasks--buffer-contains-p (buffer text &optional start-point)
  "Check if BUFFER contains TEXT after START-POINT.
Uses first 40 chars as signature.  Returns position if found, nil otherwise."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start-point (point-min)))
        (search-forward (substring text 0 (min 40 (length text))) nil t)))))

(defun hive-mcp-swarm-tasks--claude-responded-p (buffer text &optional start-point)
  "Check if Claude has responded to TEXT in BUFFER after START-POINT."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start-point (point-min)))
        (when (search-forward (substring text 0 (min 40 (length text))) nil t)
          (search-forward "●" nil t))))))

;;;; Response Extraction (DRY helper):

(defun hive-mcp-swarm-tasks--extract-response (buffer prompt)
  "Extract Claude's response to PROMPT from BUFFER.
Returns response string if complete, nil if still running."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (point-min))
        (let ((search-key (substring prompt 0 (min 50 (length prompt)))))
          (when (and (search-forward search-key nil t)
                     (search-forward "●" nil t))
            (let ((response-start (point)))
              (cond
               ((search-forward "\n> " nil t)
                (string-trim (buffer-substring-no-properties response-start (- (point) 3))))
               ((search-forward "\n────" nil t)
                (string-trim (buffer-substring-no-properties response-start (- (point) 5))))
               ((search-forward "\n\n\n" nil t)
                (string-trim (buffer-substring-no-properties response-start (- (point) 3))))))))))))

;;;; Filter Matching:

(defun hive-mcp-swarm-tasks--slave-matches-filter (slave filter)
  "Check if SLAVE matches FILTER criteria (:role, :status)."
  (and (or (not (plist-get filter :role))
           (equal (plist-get slave :role) (plist-get filter :role)))
       (or (not (plist-get filter :status))
           (eq (plist-get slave :status) (plist-get filter :status)))))

;;;; Task Dispatch:

(cl-defun hive-mcp-swarm-tasks-dispatch (slave-id prompt &key timeout priority context)
  "Dispatch PROMPT to SLAVE-ID (NON-BLOCKING).
TIMEOUT is milliseconds, PRIORITY is critical/high/normal/low.
Returns task-id."
  (interactive
   (list (completing-read "Slave: " (hash-table-keys hive-mcp-swarm--slaves))
         (read-string "Prompt: ")))
  (let* ((slave (gethash slave-id hive-mcp-swarm--slaves))
         (task-id (hive-mcp-swarm-slaves-generate-task-id slave-id))
         (buffer (plist-get slave :buffer)))
    (unless slave (error "Slave not found: %s" slave-id))
    (unless (buffer-live-p buffer) (error "Slave buffer is dead: %s" slave-id))
    ;; Create task record
    (puthash task-id
             (list :task-id task-id :slave-id slave-id :prompt prompt
                   :status 'dispatched :priority (or priority 'normal)
                   :timeout (or timeout hive-mcp-swarm-default-timeout)
                   :context context :dispatched-at (format-time-string "%FT%T%z")
                   :completed-at nil :result nil :error nil)
             hive-mcp-swarm--tasks)
    ;; Update slave state
    (plist-put slave :status 'working)
    (plist-put slave :current-task task-id)
    (plist-put slave :last-activity (format-time-string "%FT%T%z"))
    (plist-put slave :task-start-point (with-current-buffer buffer (point-max)))
    ;; Send prompt - NON-BLOCKING
    (condition-case err
        (hive-mcp-swarm-terminal-send buffer prompt
                                      (or (plist-get slave :terminal) hive-mcp-swarm-terminal))
      (error (message "[swarm-tasks] Dispatch error: %s" (error-message-string err))))
    (when (called-interactively-p 'any)
      (message "Dispatched task %s to %s" task-id slave-id))
    task-id))

;;;; Task Collection:

(defun hive-mcp-swarm-tasks-collect (task-id &optional timeout-ms)
  "Collect response for TASK-ID (BLOCKING).
TIMEOUT-MS defaults to 5000.  Returns task plist with :result."
  (interactive
   (list (completing-read "Task: " (hash-table-keys hive-mcp-swarm--tasks))))
  (let* ((task (or (gethash task-id hive-mcp-swarm--tasks)
                   (error "Task not found: %s" task-id)))
         (slave (gethash (plist-get task :slave-id) hive-mcp-swarm--slaves))
         (buffer (plist-get slave :buffer))
         (timeout (/ (or timeout-ms 5000) 1000.0))
         (start-time (float-time))
         result)
    ;; Poll for completion
    (while (and (< (- (float-time) start-time) timeout) (not result))
      (setq result (hive-mcp-swarm-tasks--extract-response buffer (plist-get task :prompt)))
      (unless result (sleep-for 0.5)))
    ;; Update task and slave
    (if result
        (progn
          (plist-put task :status 'completed)
          (plist-put task :result result)
          (plist-put task :completed-at (format-time-string "%FT%T%z"))
          (plist-put slave :status 'idle)
          (plist-put slave :current-task nil)
          (plist-put slave :tasks-completed (1+ (plist-get slave :tasks-completed))))
      (plist-put task :status 'timeout)
      (plist-put task :error "Collection timed out"))
    (when (called-interactively-p 'any)
      (message (if result "Collected result (%d chars)" "Collection timed out")
               (length (or result ""))))
    task))

;;;; Broadcast:

(defun hive-mcp-swarm-tasks-broadcast (prompt &optional slave-filter)
  "Send PROMPT to all slaves matching SLAVE-FILTER (:role, :status).
Returns list of task-ids."
  (let ((task-ids '()))
    (maphash (lambda (slave-id slave)
               (when (or (not slave-filter)
                         (hive-mcp-swarm-tasks--slave-matches-filter slave slave-filter))
                 (push (hive-mcp-swarm-tasks-dispatch slave-id prompt) task-ids)))
             hive-mcp-swarm--slaves)
    (nreverse task-ids)))

;;;; Non-blocking Completion Check:

(defun hive-mcp-swarm-tasks-check-completion (task-id)
  "Check if TASK-ID has completed (NON-BLOCKING).
Returns result string if complete, nil if still running."
  (when-let* ((task (gethash task-id hive-mcp-swarm--tasks))
              (slave (gethash (plist-get task :slave-id) hive-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer)))
    (hive-mcp-swarm-tasks--extract-response buffer (plist-get task :prompt))))

;;;; Lifecycle:

(defun hive-mcp-swarm-tasks-init ()
  "Initialize tasks module.")

(defun hive-mcp-swarm-tasks-shutdown ()
  "Shutdown tasks module.")

(provide 'hive-mcp-swarm-tasks)
;;; hive-mcp-swarm-tasks.el ends here
