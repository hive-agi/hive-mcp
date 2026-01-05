;;; hive-mcp-swarm.el --- Claude swarm orchestration for parallel task execution -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/hive-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Optional-Requires: claude-code-ide (recommended), vterm, or eat
;; Keywords: tools, ai, claude, orchestration, parallel
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Swarm orchestration for Claude Code instances.
;;
;; Enables a Master Claude to spawn and control multiple Slave Claude
;; instances for parallel task execution.  Each slave runs in a terminal
;; buffer and can be configured with presets (system prompts loaded from
;; markdown files).
;;
;; Terminal backend choice:
;;   - claude-code-ide (recommended): Uses claude-code-ide.el's terminal
;;     abstraction with optimized timing and anti-flicker support.  Most
;;     reliable for command submission.
;;   - vterm: Native terminal, reliable input handling.
;;   - eat (experimental): Pure Emacs Lisp, may have input submission issues.
;;
;; Set via: (setq hive-mcp-swarm-terminal 'claude-code-ide)  ; default
;;
;; Architecture:
;;
;;   Master Claude (you)
;;         │ MCP tools
;;         v
;;   hive-mcp-swarm.el (this file)
;;         │ vterm-send-string
;;         v
;;   ┌─────┴─────┐
;;   │  Slaves   │ (vterm buffers running `claude`)
;;   └───────────┘
;;
;; Features:
;; - Spawn slaves with roles/presets
;; - Dispatch prompts to slaves
;; - Collect responses
;; - Load presets from markdown files
;; - Custom preset directories (recursive .md scan)
;; - Combine multiple presets per slave
;;
;; Usage:
;;   ;; Load the addon
;;   (require 'hive-mcp-swarm)
;;   (hive-mcp-swarm-mode 1)
;;
;;   ;; Spawn a slave with preset
;;   (hive-mcp-swarm-spawn "tester" :presets '("tdd" "clarity"))
;;
;;   ;; Dispatch a task
;;   (hive-mcp-swarm-dispatch "swarm-tester-xxx" "Run all tests")
;;
;;   ;; Check status
;;   (hive-mcp-swarm-status)

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'hive-mcp-graceful)

;; Add swarm/ subdirectory to load-path for modular components
(let ((swarm-dir (expand-file-name "swarm" (file-name-directory
                                            (or load-file-name buffer-file-name)))))
  (when (file-directory-p swarm-dir)
    (add-to-list 'load-path swarm-dir)))

;; Load modular components from swarm/ directory
(require 'hive-mcp-swarm-events)
(require 'hive-mcp-swarm-prompts)
(require 'hive-mcp-swarm-presets)
(require 'hive-mcp-swarm-terminal)

;; Soft dependency on channel for push events
(declare-function hive-mcp-channel-connected-p "hive-mcp-channel")
(declare-function hive-mcp-channel-send "hive-mcp-channel")

;; Soft dependency on vterm
(declare-function vterm "vterm")
(declare-function vterm-send-string "vterm")
(declare-function vterm-send-return "vterm")

;; Soft dependency on eat
(declare-function eat "eat")
(declare-function eat-mode "eat")
(declare-function eat-exec "eat")
(declare-function eat-term-send-string "eat")

;; Soft dependency on claude-code-ide
(declare-function claude-code-ide "claude-code-ide")
(declare-function claude-code-ide--configure-vterm-buffer "claude-code-ide")
(declare-function claude-code-ide--terminal-send-string "claude-code-ide")
(declare-function claude-code-ide--terminal-send-return "claude-code-ide")
(declare-function claude-code-ide-mcp-server-get-session-context "claude-code-ide-mcp-server")
(defvar claude-code-ide-terminal-backend)

;;;; Customization:

(defgroup hive-mcp-swarm nil
  "Claude swarm orchestration."
  :group 'hive-mcp
  :prefix "hive-mcp-swarm-")

(defcustom hive-mcp-swarm-presets-dir
  (expand-file-name "presets" (file-name-directory
                               (or load-file-name buffer-file-name
                                   default-directory)))
  "Directory containing built-in preset markdown files."
  :type 'directory
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-custom-presets-dirs nil
  "List of custom directories to scan for preset .md files.
Directories are scanned recursively for .md files only."
  :type '(repeat directory)
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-claude-command "claude"
  "Command to invoke Claude Code CLI."
  :type 'string
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-terminal 'claude-code-ide
  "Terminal emulator to use for slave sessions.
- `claude-code-ide': Use claude-code-ide.el WebSocket integration - RECOMMENDED
  Provides MCP integration, robust session management, no terminal quirks.
- `vterm': Use vterm (requires native compilation) - ALTERNATIVE
- `eat': Use eat (pure Emacs Lisp) - EXPERIMENTAL, may have input issues"
  :type '(choice (const :tag "claude-code-ide (recommended)" claude-code-ide)
          (const :tag "vterm (native terminal)" vterm)
          (const :tag "eat (experimental)" eat))
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-max-slaves 30
  "Maximum number of concurrent slave instances."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-max-depth 3
  "Maximum recursion depth (slaves spawning slaves).
Depth 0 = master, 1 = child, 2 = grandchild, 3 = great-grandchild.
Beyond this depth, spawn attempts are blocked to prevent runaway recursion."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-rate-limit-window 60
  "Time window in seconds for rate limiting spawn attempts."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-rate-limit-max-spawns 10
  "Maximum number of spawns allowed within `hive-mcp-swarm-rate-limit-window'."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-default-timeout 300000
  "Default task timeout in milliseconds (5 minutes)."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-send-verify t
  "If non-nil, verify that prompts were sent to the terminal.
When enabled, dispatch will check the buffer for the prompt text
and retry if not found."
  :type 'boolean
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-send-retries 3
  "Number of retry attempts for sending prompts."
  :type 'integer
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-send-delay 0.2
  "Base delay in seconds between send and return.
This delay increases with each retry (exponential backoff)."
  :type 'float
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-return-delay 0.05
  "Delay in seconds between sending text and sending return.
Helps ensure vterm processes the text before return is sent."
  :type 'float
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-auto-approve t
  "If non-nil, automatically approve tool permission prompts.
Claude Code asks for permission before running tools. This setting
auto-sends 'y' when permission prompts are detected."
  :type 'boolean
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-auto-approve-patterns
  '("Allow\\|Deny" "Yes\\|No" "(y/n)" "[Y/n]" "[y/N]"
    "Do you want to" "Would you like to" "Proceed\\?")
  "Patterns that indicate a permission/confirmation prompt.
When any of these are found in the buffer, and `hive-mcp-swarm-auto-approve'
is non-nil, automatically send 'y' to approve."
  :type '(repeat string)
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-prompt-mode 'bypass
  "How to handle permission prompts in slaves.
- `bypass': Use --permission-mode bypassPermissions (no prompts)
- `auto': Timer-based auto-approve (legacy behavior)
- `human': Forward prompts to master for human decision"
  :type '(choice (const :tag "Bypass permissions (CLI flag)" bypass)
          (const :tag "Auto-approve (timer)" auto)
          (const :tag "Human decision (hooks)" human))
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-prompt-notify t
  "If non-nil, show notification when prompts are pending in human mode."
  :type 'boolean
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-desktop-notify t
  "If non-nil, send desktop notification via notify-send for prompts.
This alerts the human even when running master Claude in a terminal."
  :type 'boolean
  :group 'hive-mcp-swarm)

(defvar hive-mcp-swarm-prompts-buffer-name "*Swarm Prompts*"
  "Buffer name for displaying pending prompts.")

(defcustom hive-mcp-swarm-prompt-marker "❯"
  "Marker indicating Claude is ready for input."
  :type 'string
  :group 'hive-mcp-swarm)

(defcustom hive-mcp-swarm-buffer-prefix "*swarm-"
  "Prefix for swarm buffer names."
  :type 'string
  :group 'hive-mcp-swarm)

;;;; Internal State:

(defvar hive-mcp-swarm--slaves (make-hash-table :test 'equal)
  "Hash table of slave-id -> slave plist.")

(defvar hive-mcp-swarm--tasks (make-hash-table :test 'equal)
  "Hash table of task-id -> task plist.")

(defvar hive-mcp-swarm--task-counter 0
  "Counter for generating unique task IDs.")

(defvar hive-mcp-swarm--session-id nil
  "Current swarm session ID.")

(defvar hive-mcp-swarm--current-depth 0
  "Current recursion depth (set via environment).")

(defvar hive-mcp-swarm--spawn-timestamps nil
  "List of recent spawn timestamps for rate limiting.")

(defvar hive-mcp-swarm--ancestry nil
  "Ancestry chain: list of (slave-id . master-id) for loop detection.")

;; NOTE: The following state is now managed by submodules:
;; - Presets cache: hive-mcp-swarm-presets module
;; - Auto-approve timer: hive-mcp-swarm-prompts module
;; - Last approve positions: hive-mcp-swarm-prompts module
;; - Pending prompts: hive-mcp-swarm-prompts module

;;;; Channel Event Emission (Push-based updates):
;; Delegated to hive-mcp-swarm-events module for centralized event handling.

(defun hive-mcp-swarm--channel-available-p ()
  "Check if the bidirectional channel is available and connected."
  (hive-mcp-swarm-events-channel-available-p))

(defun hive-mcp-swarm--emit-event (event-type data)
  "Emit EVENT-TYPE with DATA through the channel if connected."
  (hive-mcp-swarm-events-emit event-type data))

(defun hive-mcp-swarm--emit-task-completed (task-id slave-id result)
  "Emit task-completed event for TASK-ID from SLAVE-ID with RESULT."
  (hive-mcp-swarm-events-emit-task-completed task-id slave-id result))

(defun hive-mcp-swarm--emit-task-failed (task-id slave-id error-msg)
  "Emit task-failed event for TASK-ID from SLAVE-ID with ERROR-MSG."
  (hive-mcp-swarm-events-emit-task-failed task-id slave-id error-msg))

(defun hive-mcp-swarm--emit-prompt-shown (slave-id prompt-text)
  "Emit prompt-shown event for SLAVE-ID with PROMPT-TEXT."
  (hive-mcp-swarm-events-emit-prompt-shown slave-id prompt-text))

(defun hive-mcp-swarm--emit-state-changed (slave-id old-state new-state)
  "Emit state-changed event for SLAVE-ID from OLD-STATE to NEW-STATE."
  (hive-mcp-swarm-events-emit-state-changed slave-id old-state new-state))

(defun hive-mcp-swarm--emit-slave-spawned (slave-id name presets)
  "Emit slave-spawned event for SLAVE-ID with NAME and PRESETS."
  (hive-mcp-swarm-events-emit-slave-spawned slave-id name presets))

(defun hive-mcp-swarm--emit-slave-killed (slave-id)
  "Emit slave-killed event for SLAVE-ID."
  (hive-mcp-swarm-events-emit-slave-killed slave-id))

;;;; Auto-Approve Watcher:
;; Delegated to hive-mcp-swarm-prompts module for centralized prompt handling.

(defun hive-mcp-swarm--get-terminal-type (slave)
  "Get terminal type for SLAVE plist."
  (or (plist-get slave :terminal) hive-mcp-swarm-terminal))

(defun hive-mcp-swarm-start-auto-approve ()
  "Start the auto-approve watcher timer."
  (interactive)
  (hive-mcp-swarm-prompts-start-watcher
   hive-mcp-swarm--slaves
   #'hive-mcp-swarm--get-terminal-type)
  (message "[swarm] Auto-approve watcher started"))

(defun hive-mcp-swarm-stop-auto-approve ()
  "Stop the auto-approve watcher timer."
  (interactive)
  (hive-mcp-swarm-prompts-stop-watcher)
  (message "[swarm] Auto-approve watcher stopped"))

;;;; Human Mode - Prompt Hooks:
;; Delegated to hive-mcp-swarm-prompts module for centralized prompt handling.
;; These wrappers maintain backward compatibility.

(defun hive-mcp-swarm--update-prompts-buffer ()
  "Refresh the prompts buffer with current pending prompts."
  (hive-mcp-swarm-prompts-update-buffer))

(defun hive-mcp-swarm-respond ()
  "Respond to the next pending prompt interactively."
  (interactive)
  (hive-mcp-swarm-prompts-respond))

(defun hive-mcp-swarm-approve ()
  "Approve (send \\='y\\=') to the next pending prompt."
  (interactive)
  (hive-mcp-swarm-prompts-approve))

(defun hive-mcp-swarm-deny ()
  "Deny (send \\='n\\=') to the next pending prompt."
  (interactive)
  (hive-mcp-swarm-prompts-deny))

(defun hive-mcp-swarm-list-prompts ()
  "List all pending prompts in the prompts buffer."
  (interactive)
  (hive-mcp-swarm-prompts-list))

;;;; Preset Management:
;; Delegated to hive-mcp-swarm-presets module.
;; These wrappers maintain backward compatibility.

(defun hive-mcp-swarm-reload-presets ()
  "Reload all presets from disk."
  (interactive)
  (hive-mcp-swarm-presets-reload))

(defun hive-mcp-swarm-list-presets ()
  "List all available presets (file-based + memory-based)."
  (interactive)
  (hive-mcp-swarm-presets-list))

(defun hive-mcp-swarm--build-system-prompt (presets)
  "Build combined system prompt from list of PRESETS."
  (hive-mcp-swarm-presets-build-system-prompt presets))

(defun hive-mcp-swarm-add-custom-presets-dir (dir)
  "Add DIR to custom preset directories and reload."
  (interactive "DPresets directory: ")
  (hive-mcp-swarm-presets-add-custom-dir dir))

(defun hive-mcp-swarm--role-to-presets (role)
  "Convert ROLE to list of preset names."
  (hive-mcp-swarm-presets-role-to-presets role))

;;;; Slave Management:

(defun hive-mcp-swarm--generate-slave-id (name)
  "Generate unique slave ID for NAME."
  (format "swarm-%s-%d" name (floor (float-time))))

(defun hive-mcp-swarm--generate-task-id (slave-id)
  "Generate unique task ID for SLAVE-ID."
  (cl-incf hive-mcp-swarm--task-counter)
  (format "task-%s-%03d"
          (replace-regexp-in-string "^swarm-" "" slave-id)
          hive-mcp-swarm--task-counter))

(defun hive-mcp-swarm--depth-label (depth)
  "Return human-readable label for DEPTH level."
  (pcase depth
    (0 "master")
    (1 "child")
    (2 "grandchild")
    (3 "great-grandchild")
    (_ (format "depth-%d" depth))))

(defun hive-mcp-swarm--check-depth ()
  "Check if we can spawn at current depth.
Returns the current depth if allowed, signals error if blocked."
  (let* ((depth (string-to-number (or (getenv "CLAUDE_SWARM_DEPTH") "0")))
         (master-id (getenv "CLAUDE_SWARM_MASTER"))
         (my-id (getenv "CLAUDE_SWARM_SLAVE_ID")))
    (setq hive-mcp-swarm--current-depth depth)
    ;; Track ancestry for loop detection
    (when (and my-id master-id)
      (push (cons my-id master-id) hive-mcp-swarm--ancestry))
    (when (>= depth hive-mcp-swarm-max-depth)
      (error "Recursion limit reached: %s (depth %d) cannot spawn children.
Maximum depth is %d (master → child → grandchild → great-grandchild).
This limit prevents runaway recursive spawning."
             (hive-mcp-swarm--depth-label depth)
             depth
             hive-mcp-swarm-max-depth))
    depth))

(defun hive-mcp-swarm--check-rate-limit ()
  "Check if spawn rate limit allows a new spawn.
Removes old timestamps and checks count within window."
  (let* ((now (float-time))
         (window-start (- now hive-mcp-swarm-rate-limit-window)))
    ;; Prune old timestamps
    (setq hive-mcp-swarm--spawn-timestamps
          (cl-remove-if (lambda (ts) (< ts window-start))
                        hive-mcp-swarm--spawn-timestamps))
    ;; Check limit
    (when (>= (length hive-mcp-swarm--spawn-timestamps)
              hive-mcp-swarm-rate-limit-max-spawns)
      (error "Rate limit exceeded: %d spawns in %d seconds.
Wait before spawning more slaves to prevent spawn storms."
             hive-mcp-swarm-rate-limit-max-spawns
             hive-mcp-swarm-rate-limit-window))
    ;; Record this spawn attempt
    (push now hive-mcp-swarm--spawn-timestamps)))

(defun hive-mcp-swarm--check-slave-limit ()
  "Check if we can spawn more slaves."
  (when (>= (hash-table-count hive-mcp-swarm--slaves) hive-mcp-swarm-max-slaves)
    (error "Maximum slave count (%d) reached.
Kill some slaves with `hive-mcp-swarm-kill' before spawning more."
           hive-mcp-swarm-max-slaves)))

(cl-defun hive-mcp-swarm-spawn (name &key presets cwd role terminal)
  "Spawn a new Claude slave with NAME - FULLY ASYNC.

PRESETS is a list of preset names to apply (e.g., '(\"tdd\" \"clarity\")).
CWD is the working directory (defaults to current project root).
ROLE is a predefined role that maps to presets.
TERMINAL overrides `hive-mcp-swarm-terminal' for this spawn ('vterm or 'eat).

Returns the slave-id IMMEDIATELY.  The actual spawn happens async.
Poll the slave's :status to check progress: spawning -> starting -> idle."
  (interactive
   (list (read-string "Slave name: ")
         :presets (completing-read-multiple
                   "Presets: "
                   (hive-mcp-swarm-list-presets))))
  ;; Safety checks (order matters: depth → rate → slave count)
  (hive-mcp-swarm--check-depth)
  (hive-mcp-swarm--check-rate-limit)
  (hive-mcp-swarm--check-slave-limit)

  ;; Resolve role to presets if provided
  (when (and role (not presets))
    (setq presets (hive-mcp-swarm--role-to-presets role)))

  (let* ((slave-id (hive-mcp-swarm--generate-slave-id name))
         (work-dir (or cwd (hive-mcp-swarm--project-root) default-directory))
         (term-backend (or terminal hive-mcp-swarm-terminal))
         (parent-id (or (getenv "CLAUDE_SWARM_SLAVE_ID") "master"))
         (spawn-depth (1+ hive-mcp-swarm--current-depth)))

    ;; Register slave IMMEDIATELY with "spawning" status - BEFORE any work
    (puthash slave-id
             (list :slave-id slave-id
                   :name name
                   :role role
                   :presets presets
                   :status 'spawning  ; Not starting yet - we return immediately
                   :buffer nil        ; Buffer created async
                   :terminal term-backend
                   :cwd work-dir
                   :depth spawn-depth
                   :parent-id parent-id
                   :current-task nil
                   :task-queue '()
                   :tasks-completed 0
                   :tasks-failed 0
                   :spawned-at (format-time-string "%FT%T%z")
                   :last-activity (format-time-string "%FT%T%z"))
             hive-mcp-swarm--slaves)

    ;; Log spawn intent
    (message "[swarm] Spawning %s (%s) at depth %d, parent: %s (async)"
             slave-id
             (hive-mcp-swarm--depth-label spawn-depth)
             spawn-depth
             parent-id)

    ;; Emit slave-spawned event via channel
    (hive-mcp-swarm--emit-slave-spawned slave-id name presets)

    ;; FULLY ASYNC: Defer ALL work to timer so we return IMMEDIATELY
    (run-with-timer
     0 nil
     (lambda ()
       (condition-case err
           (hive-mcp-swarm--do-spawn-async slave-id name presets work-dir term-backend)
         (error
          ;; Mark slave as errored
          (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves)))
            (plist-put slave :status 'error)
            (plist-put slave :error (error-message-string err)))
          (message "[swarm] Spawn error for %s: %s" slave-id (error-message-string err))))))

    (when (called-interactively-p 'any)
      (message "Spawning slave: %s (async)" slave-id))

    slave-id))

(defun hive-mcp-swarm--do-spawn-async (slave-id name presets work-dir term-backend)
  "Actually spawn the slave buffer for SLAVE-ID.
Called async from `hive-mcp-swarm-spawn'.  Updates slave status as work progresses."
  (let* ((slave (gethash slave-id hive-mcp-swarm--slaves))
         (buffer-name (format "%s%s*" hive-mcp-swarm-buffer-prefix name))
         (system-prompt (hive-mcp-swarm--build-system-prompt presets))
         buffer)

    (unless slave
      (error "Slave record not found: %s" slave-id))

    ;; Require terminal emulator (could potentially block on first load)
    (pcase term-backend
      ('claude-code-ide (unless (require 'claude-code-ide nil t)
                          (error "Claude-code-ide is required but not available")))
      ('vterm (unless (require 'vterm nil t)
                (error "Vterm is required but not available")))
      ('eat (unless (require 'eat nil t)
              (error "Eat is required but not available"))))

    ;; Update status to starting
    (plist-put slave :status 'starting)

    ;; Create terminal buffer
    (setq buffer (generate-new-buffer buffer-name))
    (plist-put slave :buffer buffer)

    (let* ((default-directory work-dir)
           (process-environment
            (append
             (list (format "CLAUDE_SWARM_DEPTH=%d" (plist-get slave :depth))
                   (format "CLAUDE_SWARM_MASTER=%s" (or hive-mcp-swarm--session-id "direct"))
                   (format "CLAUDE_SWARM_SLAVE_ID=%s" slave-id))
             process-environment))
           (permission-flag (pcase hive-mcp-swarm-prompt-mode
                              ('bypass "--permission-mode bypassPermissions")
                              (_ "")))
           (claude-cmd (if system-prompt
                           (let ((prompt-file (make-temp-file "swarm-prompt-" nil ".md")))
                             (with-temp-file prompt-file
                               (insert system-prompt))
                             (format "cd %s && %s %s --system-prompt %s"
                                     (shell-quote-argument work-dir)
                                     hive-mcp-swarm-claude-command
                                     permission-flag
                                     (shell-quote-argument prompt-file)))
                         (format "cd %s && %s %s"
                                 (shell-quote-argument work-dir)
                                 hive-mcp-swarm-claude-command
                                 permission-flag))))

      (pcase term-backend
        ('claude-code-ide
         ;; Use full claude-code-ide session with MCP WebSocket integration
         ;; Kill the pre-created buffer since claude-code-ide creates its own
         (when (buffer-live-p buffer)
           (kill-buffer buffer))
         ;; Ensure MCP server is running (starts it if needed)
         (let* ((port (when (fboundp 'claude-code-ide-mcp-server-ensure-server)
                        (claude-code-ide-mcp-server-ensure-server)))
                (_ (unless port
                     (error "Failed to start MCP server for claude-code-ide backend")))
                (result (claude-code-ide--create-terminal-session
                         buffer-name
                         work-dir
                         port
                         nil  ;; continue
                         nil  ;; resume
                         slave-id)))
           (setq buffer (car result))
           (plist-put slave :buffer buffer)
           (plist-put slave :process (cdr result))
           ;; Apply system prompt if present via CLI command after session starts
           (when system-prompt
             (let ((prompt-to-send system-prompt))
               (run-at-time 1.0 nil
                            (lambda ()
                              (when (buffer-live-p buffer)
                                (hive-mcp-swarm-terminal-send
                                 buffer
                                 (format "/system-prompt %s"
                                         (shell-quote-argument prompt-to-send))
                                 'claude-code-ide)))))))))
      ('vterm
       (with-current-buffer buffer
         (vterm-mode)
         (run-at-time 0.5 nil
                      (lambda ()
                        (when (buffer-live-p buffer)
                          (with-current-buffer buffer
                            (vterm-send-string claude-cmd)
                            (vterm-send-return)))))))
      ('eat
       (with-current-buffer buffer
         (eat-mode)
         (eat-exec buffer "swarm-shell" "/bin/bash" nil '("-l")))
       (run-at-time 0.5 nil
                    (lambda ()
                      (when (buffer-live-p buffer)
                        (with-current-buffer buffer
                          (when (and (boundp 'eat-terminal) eat-terminal)
                            (eat-term-send-string eat-terminal claude-cmd)
                            (eat-term-send-string eat-terminal "\r")))))))))

  ;; Log completion
  (message "[swarm] Spawned %s buffer created" slave-id)

  ;; Schedule status transition to idle
  (run-at-time
   3 nil
   (lambda ()
     (when-let* ((s (gethash slave-id hive-mcp-swarm--slaves)))
       (when (memq (plist-get s :status) '(starting spawning))
         (plist-put s :status 'idle))))))

(defun hive-mcp-swarm--project-root ()
  "Get current project root."
  (or (when (fboundp 'project-root)
        (when-let* ((proj (project-current)))
          (project-root proj)))
      default-directory))

(defun hive-mcp-swarm-kill (slave-id)
  "Kill slave SLAVE-ID without prompts.
Force-kills the buffer to prevent blocking on process/unsaved prompts.
Handles vterm/eat process cleanup to ensure no confirmation dialogs.
Emits slave-killed event via channel for push-based updates."
  (interactive
   (list (completing-read "Kill slave: "
                          (hash-table-keys hive-mcp-swarm--slaves))))
  (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves)))
    (let ((buffer (plist-get slave :buffer)))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          ;; Step 1: Mark buffer as unmodified to prevent "save buffer?" prompts
          (set-buffer-modified-p nil)
          ;; Step 2: Disable process query-on-exit for ALL processes in this buffer
          ;; This prevents "Buffer has running process; kill it?" prompts
          (when-let* ((proc (get-buffer-process buffer)))
            (set-process-query-on-exit-flag proc nil))
          ;; Also handle vterm's internal process tracking if present
          (when (and (boundp 'vterm--process) vterm--process
                     (process-live-p vterm--process))
            (set-process-query-on-exit-flag vterm--process nil)))
        ;; Step 3: Kill buffer with ALL hooks disabled
        ;; - kill-buffer-query-functions: prevents "process running" prompts
        ;; - kill-buffer-hook: prevents any cleanup hooks from blocking
        ;; - vterm-exit-functions: prevents vterm cleanup hooks
        (let ((kill-buffer-query-functions nil)
              (kill-buffer-hook nil)
              (vterm-exit-functions nil))
          (kill-buffer buffer))))
    ;; Emit slave-killed event via channel before cleanup
    (hive-mcp-swarm--emit-slave-killed slave-id)
    (remhash slave-id hive-mcp-swarm--slaves)
    ;; Clear prompt tracking for this slave
    (hive-mcp-swarm-prompts-clear-slave slave-id)
    (message "Killed slave: %s" slave-id)))

(defun hive-mcp-swarm-kill-all ()
  "Kill all slaves."
  (interactive)
  (let ((count 0))
    (maphash (lambda (id _)
               (hive-mcp-swarm-kill id)
               (cl-incf count))
             hive-mcp-swarm--slaves)
    (message "Killed %d slaves" count)))

;;;; Terminal Send Functions:

(defun hive-mcp-swarm--buffer-contains-p (buffer text &optional start-point)
  "Check if BUFFER contains TEXT after START-POINT.
Uses first 40 chars as signature. Returns position if found, nil otherwise."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start-point (point-min)))
        (let ((sig (substring text 0 (min 40 (length text)))))
          (search-forward sig nil t))))))

(defun hive-mcp-swarm--claude-responded-p (buffer text &optional start-point)
  "Check if Claude has responded to TEXT in BUFFER after START-POINT.
Looks for the prompt followed by Claude's response marker (●)."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (or start-point (point-min)))
        (let ((sig (substring text 0 (min 40 (length text)))))
          ;; Find our prompt
          (when (search-forward sig nil t)
            ;; Check if there's a response marker after it
            (search-forward "●" nil t)))))))

;;;; Task Dispatch and Collection:
;; NOTE: Terminal operations delegated to hive-mcp-swarm-terminal module
;; All send operations are NON-BLOCKING to prevent Emacs hanging during MCP calls

(cl-defun hive-mcp-swarm-dispatch (slave-id prompt &key timeout priority context)
  "Dispatch PROMPT to SLAVE-ID.

TIMEOUT is milliseconds (default: `hive-mcp-swarm-default-timeout').
PRIORITY is one of: critical, high, normal, low.
CONTEXT is additional context plist.

Returns task-id."
  (interactive
   (list (completing-read "Slave: " (hash-table-keys hive-mcp-swarm--slaves))
         (read-string "Prompt: ")))

  (let* ((slave (gethash slave-id hive-mcp-swarm--slaves))
         (task-id (hive-mcp-swarm--generate-task-id slave-id))
         (buffer (plist-get slave :buffer)))

    (unless slave
      (error "Slave not found: %s" slave-id))

    (unless (buffer-live-p buffer)
      (error "Slave buffer is dead: %s" slave-id))

    ;; Create task record
    (puthash task-id
             (list :task-id task-id
                   :slave-id slave-id
                   :prompt prompt
                   :status 'dispatched
                   :priority (or priority 'normal)
                   :timeout (or timeout hive-mcp-swarm-default-timeout)
                   :context context
                   :dispatched-at (format-time-string "%FT%T%z")
                   :completed-at nil
                   :result nil
                   :error nil)
             hive-mcp-swarm--tasks)

    ;; Update slave state
    (plist-put slave :status 'working)
    (plist-put slave :current-task task-id)
    (plist-put slave :last-activity (format-time-string "%FT%T%z"))
    (plist-put slave :task-start-point
               (with-current-buffer buffer (point-max)))

    ;; Send prompt to slave - NON-BLOCKING via terminal module
    ;; No sit-for loops - prevents Emacs hanging during MCP calls
    (let ((target-buffer buffer)
          (term-type (or (plist-get slave :terminal) hive-mcp-swarm-terminal))
          (prompt-text prompt)
          (the-task-id task-id))
      (condition-case err
          (hive-mcp-swarm-terminal-send target-buffer prompt-text term-type)
        (error
         (message "[swarm] Dispatch error for %s: %s"
                  the-task-id (error-message-string err)))))

    (when (called-interactively-p 'any)
      (message "Dispatched task %s to %s" task-id slave-id))

    task-id))

(defun hive-mcp-swarm-collect (task-id &optional timeout-ms)
  "Collect response for TASK-ID.

TIMEOUT-MS is how long to wait (default: 5000ms).
Returns the task plist with :result populated."
  (interactive
   (list (completing-read "Task: " (hash-table-keys hive-mcp-swarm--tasks))))

  (let* ((task (gethash task-id hive-mcp-swarm--tasks))
         (slave-id (plist-get task :slave-id))
         (slave (gethash slave-id hive-mcp-swarm--slaves))
         (buffer (plist-get slave :buffer))
         (prompt (plist-get task :prompt))
         (timeout (/ (or timeout-ms 5000) 1000.0))
         (start-time (float-time))
         result)

    (unless task
      (error "Task not found: %s" task-id))

    ;; Wait for completion by searching for prompt text + response marker
    ;; vterm buffers have complex structure, so we search for our prompt
    ;; then look for Claude's response marker (●) after it
    (while (and (< (- (float-time) start-time) timeout)
                (not result))
      (when (buffer-live-p buffer)
        (with-current-buffer buffer
          (save-excursion
            (goto-char (point-min))
            ;; Find our prompt in the buffer (use first 50 chars as search key)
            (let ((search-key (substring prompt 0 (min 50 (length prompt)))))
              (when (search-forward search-key nil t)
                ;; Found our prompt, now look for response marker
                (when (search-forward "●" nil t)
                  (let ((response-start (point)))
                    ;; Find end: next prompt "> " or horizontal line or double newline
                    (cond
                     ((search-forward "\n> " nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 3)))))
                     ((search-forward "\n────" nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 5)))))
                     ((search-forward "\n\n\n" nil t)
                      (setq result (string-trim
                                    (buffer-substring-no-properties
                                     response-start (- (point) 3)))))))))))))
      (unless result
        (sleep-for 0.5)))

    ;; Update task
    (if result
        (progn
          (plist-put task :status 'completed)
          (plist-put task :result result)
          (plist-put task :completed-at (format-time-string "%FT%T%z"))
          ;; Update slave stats
          (plist-put slave :status 'idle)
          (plist-put slave :current-task nil)
          (plist-put slave :tasks-completed (1+ (plist-get slave :tasks-completed))))
      (plist-put task :status 'timeout)
      (plist-put task :error "Collection timed out"))

    (when (called-interactively-p 'any)
      (if result
          (message "Collected result (%d chars)" (length result))
        (message "Collection timed out")))

    task))

(defun hive-mcp-swarm-broadcast (prompt &optional slave-filter)
  "Send PROMPT to all slaves (or those matching SLAVE-FILTER).
SLAVE-FILTER is a plist like (:role \"tester\").
Returns list of task-ids."
  (let ((task-ids '()))
    (maphash
     (lambda (slave-id slave)
       (when (or (not slave-filter)
                 (hive-mcp-swarm--slave-matches-filter slave slave-filter))
         (push (hive-mcp-swarm-dispatch slave-id prompt) task-ids)))
     hive-mcp-swarm--slaves)
    (nreverse task-ids)))

(defun hive-mcp-swarm--slave-matches-filter (slave filter)
  "Check if SLAVE matches FILTER criteria."
  (let ((matches t))
    (when-let* ((role (plist-get filter :role)))
      (unless (equal (plist-get slave :role) role)
        (setq matches nil)))
    (when-let* ((status (plist-get filter :status)))
      (unless (eq (plist-get slave :status) status)
        (setq matches nil)))
    matches))

;;;; Status and Monitoring:

(defun hive-mcp-swarm-status (&optional slave-id)
  "Get swarm status.
If SLAVE-ID is provided, get that slave's status.
Otherwise return aggregate status."
  (interactive)
  (if slave-id
      (gethash slave-id hive-mcp-swarm--slaves)
    (let ((total 0) (idle 0) (working 0) (error-count 0)
          (slaves-detail '()))
      (maphash
       (lambda (id slave)
         (cl-incf total)
         (pcase (plist-get slave :status)
           ('idle (cl-incf idle))
           ('working (cl-incf working))
           ('error (cl-incf error-count)))
         (push (list :slave-id id
                     :name (plist-get slave :name)
                     :status (plist-get slave :status)
                     :depth (plist-get slave :depth)
                     :parent-id (plist-get slave :parent-id)
                     :current-task (plist-get slave :current-task)
                     :tasks-completed (plist-get slave :tasks-completed))
               slaves-detail))
       hive-mcp-swarm--slaves)

      (let ((status `(:session-id ,hive-mcp-swarm--session-id
                      :status ,(if (> total 0) "active" "inactive")
                      :current-depth ,hive-mcp-swarm--current-depth
                      :safeguards (:max-depth ,hive-mcp-swarm-max-depth
                                   :max-slaves ,hive-mcp-swarm-max-slaves
                                   :rate-limit (:window-seconds ,hive-mcp-swarm-rate-limit-window
                                                :max-spawns ,hive-mcp-swarm-rate-limit-max-spawns
                                                :recent-spawns ,(length hive-mcp-swarm--spawn-timestamps)))
                      :slaves (:total ,total :idle ,idle :working ,working :error ,error-count)
                      :tasks (:total ,(hash-table-count hive-mcp-swarm--tasks))
                      :slaves-detail ,(nreverse slaves-detail))))
        (when (called-interactively-p 'any)
          (message "Swarm: %d slaves (%d idle, %d working), %d tasks"
                   total idle working (hash-table-count hive-mcp-swarm--tasks)))
        status))))

(defun hive-mcp-swarm-show-slave (slave-id)
  "Switch to buffer for SLAVE-ID."
  (interactive
   (list (completing-read "Show slave: "
                          (hash-table-keys hive-mcp-swarm--slaves))))
  (when-let* ((slave (gethash slave-id hive-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer)))
    (if (buffer-live-p buffer)
        (switch-to-buffer buffer)
      (message "Slave buffer is dead: %s" slave-id))))

;;;; API for MCP Tools:

(defun hive-mcp-swarm-api-spawn (name presets &optional cwd terminal)
  "API: Spawn slave NAME with PRESETS in CWD using TERMINAL backend.
Returns slave-id on success, or error plist on failure."
  (hive-mcp-with-fallback
      (hive-mcp-swarm-spawn name :presets presets :cwd cwd
                            :terminal (when terminal (intern terminal)))
    `(:error "spawn-failed" :name ,name :reason "unknown")))

(defun hive-mcp-swarm-api-dispatch (slave-id prompt &optional timeout-ms)
  "API: Dispatch PROMPT to SLAVE-ID with TIMEOUT-MS.
Returns task-id on success, or error plist on failure."
  (hive-mcp-with-fallback
      (hive-mcp-swarm-dispatch slave-id prompt :timeout timeout-ms)
    `(:error "dispatch-failed" :slave-id ,slave-id :reason "unknown")))

(defun hive-mcp-swarm-api-status ()
  "API: Get swarm status as JSON-serializable plist."
  (hive-mcp-swarm-status))

(defun hive-mcp-swarm--check-task-completion (task-id)
  "Check if TASK-ID has completed without blocking.
Returns result if complete, nil if still running."
  (when-let* ((task (gethash task-id hive-mcp-swarm--tasks))
              (slave-id (plist-get task :slave-id))
              (slave (gethash slave-id hive-mcp-swarm--slaves))
              (buffer (plist-get slave :buffer))
              (prompt (plist-get task :prompt)))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (save-excursion
          (goto-char (point-min))
          (let ((search-key (substring prompt 0 (min 50 (length prompt)))))
            (when (search-forward search-key nil t)
              (when (search-forward "●" nil t)
                (let ((response-start (point)))
                  (cond
                   ((search-forward "\n> " nil t)
                    (string-trim
                     (buffer-substring-no-properties
                      response-start (- (point) 3))))
                   ((search-forward "\n────" nil t)
                    (string-trim
                     (buffer-substring-no-properties
                      response-start (- (point) 5))))
                   ((search-forward "\n\n\n" nil t)
                    (string-trim
                     (buffer-substring-no-properties
                      response-start (- (point) 3))))))))))))))

(defun hive-mcp-swarm-api-collect (task-id &optional timeout-ms)
  "API: Collect result for TASK-ID - NON-BLOCKING with graceful fallback.
Returns immediately with current status:
- :status \"completed\" with :result if done
- :status \"polling\" if still running (client should poll again)
- :status \"timeout\" if task timed out
- :status \"error\" if task failed

TIMEOUT-MS is used to check if task has exceeded its timeout,
but this function never blocks. Wraps implementation in graceful
fallback to ensure MCP clients never receive errors."
  (hive-mcp-with-fallback
      (let* ((task (gethash task-id hive-mcp-swarm--tasks))
             (slave-id (and task (plist-get task :slave-id)))
             (slave (and slave-id (gethash slave-id hive-mcp-swarm--slaves)))
             (dispatched-at (and task (plist-get task :dispatched-at)))
             (task-timeout (or timeout-ms
                               (plist-get task :timeout)
                               hive-mcp-swarm-default-timeout))
             (elapsed-ms (when dispatched-at
                           (* 1000 (- (float-time)
                                      (float-time (date-to-time dispatched-at)))))))
        (cond
         ;; Task not found
         ((not task)
          `(:task-id ,task-id :status "error" :error "Task not found"))

         ;; Already completed
         ((eq (plist-get task :status) 'completed)
          `(:task-id ,task-id
            :status "completed"
            :result ,(plist-get task :result)))

         ;; Already timed out
         ((eq (plist-get task :status) 'timeout)
          `(:task-id ,task-id
            :status "timeout"
            :error ,(plist-get task :error)))

         ;; Check if timed out now
         ((and elapsed-ms (> elapsed-ms task-timeout))
          (plist-put task :status 'timeout)
          (plist-put task :error (format "Timed out after %dms" elapsed-ms))
          (when slave
            (plist-put slave :status 'idle)
            (plist-put slave :current-task nil)
            (plist-put slave :tasks-failed (1+ (or (plist-get slave :tasks-failed) 0))))
          ;; Emit task-failed event via channel
          (hive-mcp-swarm--emit-task-failed
           task-id slave-id (format "Timed out after %dms" elapsed-ms))
          `(:task-id ,task-id
            :status "timeout"
            :error ,(format "Timed out after %dms" elapsed-ms)
            :elapsed-ms ,elapsed-ms))

         ;; Try to get result (non-blocking check)
         (t
          (if-let* ((result (hive-mcp-swarm--check-task-completion task-id)))
              (progn
                ;; Update task
                (plist-put task :status 'completed)
                (plist-put task :result result)
                (plist-put task :completed-at (format-time-string "%FT%T%z"))
                ;; Update slave
                (when slave
                  (plist-put slave :status 'idle)
                  (plist-put slave :current-task nil)
                  (plist-put slave :tasks-completed
                             (1+ (or (plist-get slave :tasks-completed) 0))))
                ;; Emit task-completed event via channel (sub-100ms push!)
                (hive-mcp-swarm--emit-task-completed task-id slave-id result)
                `(:task-id ,task-id
                  :status "completed"
                  :result ,result
                  :elapsed-ms ,elapsed-ms))
            ;; Still running - return polling status
            `(:task-id ,task-id
              :status "polling"
              :elapsed-ms ,elapsed-ms
              :timeout-ms ,task-timeout
              :message "Task still running, poll again")))))
    ;; Graceful fallback on any error
    `(:task-id ,task-id :status "error" :result nil
      :error "collection-failed")))

(defun hive-mcp-swarm-api-list-presets ()
  "API: List available presets."
  (hive-mcp-swarm-list-presets))

(defun hive-mcp-swarm-api-kill (slave-id)
  "API: Kill SLAVE-ID.
Returns result plist. Never fails."
  (hive-mcp-with-fallback
      (progn
        (hive-mcp-swarm-kill slave-id)
        `(:killed ,slave-id))
    `(:error "kill-failed" :slave-id ,slave-id)))

(defun hive-mcp-swarm-api-kill-all ()
  "API: Kill all slaves.
Returns result plist. Never fails."
  (hive-mcp-with-fallback
      (let ((count (hash-table-count hive-mcp-swarm--slaves)))
        (hive-mcp-swarm-kill-all)
        `(:killed-count ,count))
    `(:error "kill-all-failed" :killed-count 0)))

(defun hive-mcp-swarm-api-pending-prompts ()
  "API: Get list of pending prompts awaiting human decision.
Returns list of prompts with slave-id, prompt text, and timestamp."
  (hive-mcp-with-fallback
      (let* ((pending (hive-mcp-swarm-prompts-get-pending))
             (prompts
              (mapcar (lambda (p)
                        `(:slave-id ,(plist-get p :slave-id)
                          :prompt ,(plist-get p :prompt)
                          :timestamp ,(format-time-string
                                       "%Y-%m-%dT%H:%M:%S"
                                       (plist-get p :timestamp))))
                      pending)))
        `(:count ,(length prompts)
          :prompts ,prompts
          :mode ,hive-mcp-swarm-prompts-mode))
    `(:error "pending-prompts-failed" :count 0 :prompts nil)))

(defun hive-mcp-swarm-api-respond-prompt (slave-id response)
  "API: Send RESPONSE to the pending prompt from SLAVE-ID.
Returns result plist indicating success or failure."
  (hive-mcp-with-fallback
      (if (hive-mcp-swarm-prompts-respond-to slave-id response)
          `(:success t :slave-id ,slave-id :response ,response)
        `(:success nil :error "no-pending-prompt" :slave-id ,slave-id))
    `(:error "respond-prompt-failed" :slave-id ,slave-id)))

;;;; Transient Menu:

(require 'transient nil t)

(when (featurep 'transient)
  (transient-define-prefix hive-mcp-swarm-transient ()
    "Swarm orchestration menu."
    ["hive-mcp Swarm"
     ["Slaves"
      ("s" "Spawn slave" hive-mcp-swarm-spawn)
      ("k" "Kill slave" hive-mcp-swarm-kill)
      ("K" "Kill all" hive-mcp-swarm-kill-all)
      ("v" "View slave" hive-mcp-swarm-show-slave)]
     ["Tasks"
      ("d" "Dispatch" hive-mcp-swarm-dispatch)
      ("c" "Collect" hive-mcp-swarm-collect)
      ("b" "Broadcast" hive-mcp-swarm-broadcast)]
     ["Prompts (human mode)"
      ("y" "Approve next" hive-mcp-swarm-approve)
      ("n" "Deny next" hive-mcp-swarm-deny)
      ("p" "Respond custom" hive-mcp-swarm-respond)
      ("l" "List pending" hive-mcp-swarm-list-prompts)]
     ["Info"
      ("?" "Status" hive-mcp-swarm-status)
      ("P" "List presets" hive-mcp-swarm-list-presets)
      ("r" "Reload presets" hive-mcp-swarm-reload-presets)
      ("a" "Add presets dir" hive-mcp-swarm-add-custom-presets-dir)]]))

;;;; Minor Mode:

(defvar hive-mcp-swarm-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Use C-c M-s (Meta modifier) - allowed for minor modes per Emacs conventions
    (define-key map (kbd "C-c M-s") #'hive-mcp-swarm-transient)
    map)
  "Keymap for `hive-mcp-swarm-mode'.")

;;;###autoload
(define-minor-mode hive-mcp-swarm-mode
  "Minor mode for Claude swarm orchestration.

\\{hive-mcp-swarm-mode-map}"
  :init-value nil
  :lighter " Swarm"
  :keymap hive-mcp-swarm-mode-map
  :global t
  :group 'hive-mcp-swarm
  (if hive-mcp-swarm-mode
      (progn
        (setq hive-mcp-swarm--session-id
              (format "session-%s-%04x"
                      (format-time-string "%Y%m%d")
                      (random 65535)))
        ;; Initialize modules
        (hive-mcp-swarm-events-init hive-mcp-swarm--session-id)
        (hive-mcp-swarm-presets-init)
        (hive-mcp-swarm-prompts-init)
        ;; Start watcher if enabled
        (when hive-mcp-swarm-auto-approve
          (hive-mcp-swarm-start-auto-approve))
        (message "hive-mcp-swarm enabled (session: %s, auto-approve: %s)"
                 hive-mcp-swarm--session-id
                 (if hive-mcp-swarm-auto-approve "on" "off")))
    ;; Shutdown
    (hive-mcp-swarm-stop-auto-approve)
    (hive-mcp-swarm-kill-all)
    (hive-mcp-swarm-prompts-shutdown)
    (hive-mcp-swarm-presets-shutdown)
    (hive-mcp-swarm-events-shutdown)
    (message "hive-mcp-swarm disabled")))

;;;; Addon Lifecycle:

(defun hive-mcp-swarm--addon-init ()
  "Initialize swarm addon."
  (setq hive-mcp-swarm--session-id
        (format "session-%s-%04x"
                (format-time-string "%Y%m%d")
                (random 65535)))
  ;; Initialize modules
  (hive-mcp-swarm-events-init hive-mcp-swarm--session-id)
  (hive-mcp-swarm-presets-init)
  (hive-mcp-swarm-prompts-init)
  ;; Start watcher if enabled
  (when hive-mcp-swarm-auto-approve
    (hive-mcp-swarm-start-auto-approve)))

(defun hive-mcp-swarm--addon-shutdown ()
  "Shutdown swarm addon - kill all slaves."
  (hive-mcp-swarm-stop-auto-approve)
  (hive-mcp-swarm-kill-all)
  ;; Shutdown modules
  (hive-mcp-swarm-prompts-shutdown)
  (hive-mcp-swarm-presets-shutdown)
  (hive-mcp-swarm-events-shutdown)
  (setq hive-mcp-swarm--session-id nil))

;; Register with addon system
(with-eval-after-load 'hive-mcp-addons
  (hive-mcp-addon-register
   'swarm
   :version "0.1.0"
   :description "Claude swarm orchestration for parallel task execution"
   :requires '()  ; vterm or eat checked at spawn time
   :provides '(hive-mcp-swarm-mode hive-mcp-swarm-transient)
   :init #'hive-mcp-swarm--addon-init
   :shutdown #'hive-mcp-swarm--addon-shutdown))

(provide 'hive-mcp-swarm)
;;; hive-mcp-swarm.el ends here
