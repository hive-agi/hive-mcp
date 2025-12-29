# Addon Development Guide

This guide explains how to create addons for emacs-mcp.

## Overview

Addons extend emacs-mcp with integrations for external tools, packages, and services. They follow a consistent pattern with lifecycle hooks for initialization and cleanup.

## Quick Start

1. Copy the template: `elisp/addons/emacs-mcp-addon-template.el`
2. Rename to `emacs-mcp-YOUR-ADDON.el`
3. Replace "template" with your addon name
4. Implement your integration
5. Register with `emacs-mcp-addon-register`

## File Structure

```
elisp/addons/
├── emacs-mcp-addon-template.el    # Template to copy
├── emacs-mcp-cider.el             # CIDER integration
├── emacs-mcp-vibe-kanban.el       # Vibe Kanban integration
├── emacs-mcp-package-lint.el      # MELPA tools
└── emacs-mcp-YOUR-ADDON.el        # Your addon
```

## Basic Addon Structure

```elisp
;;; emacs-mcp-my-addon.el --- Description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your@email.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: tools, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;; Description of what this addon does.

;;; Code:

(require 'emacs-mcp-api)

;;;; Customization

(defgroup emacs-mcp-my-addon nil
  "My addon for emacs-mcp."
  :group 'emacs-mcp
  :prefix "emacs-mcp-my-addon-")

(defcustom emacs-mcp-my-addon-auto-start nil
  "When non-nil, auto-start on addon load."
  :type 'boolean
  :group 'emacs-mcp-my-addon)

;;;; Internal Variables

(defvar emacs-mcp-my-addon--process nil
  "Process object for the server.")

;;;; Lifecycle Functions

(defun emacs-mcp-my-addon--addon-init ()
  "Synchronous init - runs immediately after load."
  (require 'emacs-mcp-api nil t)
  (message "emacs-mcp-my-addon: initialized"))

(defun emacs-mcp-my-addon--addon-async-init ()
  "Asynchronous init - runs in background.
Should return a process object if starting a subprocess."
  (when emacs-mcp-my-addon-auto-start
    ;; Start your server/process here
    nil))

(defun emacs-mcp-my-addon--addon-shutdown ()
  "Shutdown - runs when addon is unloaded."
  (when (and emacs-mcp-my-addon--process
             (process-live-p emacs-mcp-my-addon--process))
    (kill-process emacs-mcp-my-addon--process))
  (message "emacs-mcp-my-addon: shutdown complete"))

;;;; Public Functions

;;;###autoload
(defun emacs-mcp-my-addon-do-something ()
  "Do something useful."
  (interactive)
  ;; Your implementation
  )

;;;; Registration

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'my-addon
   :version "0.1.0"
   :description "My addon description"
   :requires '(emacs-mcp-api)
   :provides '(emacs-mcp-my-addon-do-something)
   :init #'emacs-mcp-my-addon--addon-init
   :async-init #'emacs-mcp-my-addon--addon-async-init
   :shutdown #'emacs-mcp-my-addon--addon-shutdown))

(provide 'emacs-mcp-my-addon)
;;; emacs-mcp-my-addon.el ends here
```

## Lifecycle Hooks

### `:init` - Synchronous Initialization

Runs immediately after the addon file is loaded. Use for:
- Loading required features
- Setting up keybindings
- Lightweight configuration

```elisp
(defun my-addon--addon-init ()
  (require 'emacs-mcp-api nil t)
  (define-key some-map (kbd "C-c m") #'my-addon-command))
```

### `:async-init` - Asynchronous Initialization

Runs after `:init`, in the background. Use for:
- Starting external servers
- Long-running setup tasks
- Subprocess creation

**Important:** Should return a process object if starting a subprocess.

```elisp
(defun my-addon--addon-async-init ()
  "Start server asynchronously."
  (when my-addon-auto-start
    (setq my-addon--process
          (start-process "my-server" "*my-server*"
                         "npx" "my-server"))
    my-addon--process))  ; Return process for tracking
```

### `:shutdown` - Cleanup

Runs when addon is unloaded via `emacs-mcp-addon-unload`. Use for:
- Stopping servers
- Canceling timers
- Saving state

```elisp
(defun my-addon--addon-shutdown ()
  (when (process-live-p my-addon--process)
    (kill-process my-addon--process))
  (setq my-addon--process nil))
```

## Process and Timer Tracking

The addon system can automatically track processes and timers for cleanup.

### Automatic Process Tracking

If `:async-init` returns a process, it's automatically tracked:

```elisp
(defun my-addon--addon-async-init ()
  (start-process ...))  ; Returned process is tracked
```

### Manual Timer Registration

For timers, register them explicitly:

```elisp
(let ((timer (run-with-timer 1 1 #'my-tick-function)))
  (emacs-mcp-addon-register-timer 'my-addon timer))
```

## Using the MCP API

The `emacs-mcp-api` module provides functions for interacting with MCP:

```elisp
;; Get current context (buffer, project, git info)
(emacs-mcp-api-get-context)

;; Add to memory
(emacs-mcp-api-memory-add "note" "Content here" '("tag1" "tag2"))

;; Query memory
(emacs-mcp-api-memory-query "note" '("tag1") 10)
```

## Auto-Loading

### Trigger-Based Loading

Load addon when a feature becomes available:

```elisp
;; In emacs-mcp-addons.el
(defcustom emacs-mcp-addon-auto-load-list
  '((my-addon . my-trigger-package))
  ...)
```

### Always-Load

Load addon on every Emacs startup:

```elisp
;; In user config
(add-to-list 'emacs-mcp-addon-always-load 'my-addon)
```

## Transient Menus

Add a transient menu for discoverability:

```elisp
;;;###autoload (autoload 'emacs-mcp-my-addon-transient "emacs-mcp-my-addon" nil t)
(transient-define-prefix emacs-mcp-my-addon-transient ()
  "My addon menu."
  ["My Addon"
   ["Actions"
    ("a" "Action 1" my-addon-action-1)
    ("b" "Action 2" my-addon-action-2)]
   ["Settings"
    ("s" "Toggle something" my-addon-toggle)]])
```

## Best Practices

1. **Prefix all symbols** with `emacs-mcp-ADDON-`
2. **Use `defcustom`** for user-configurable options
3. **Declare soft dependencies** with `declare-function`
4. **Return processes** from `:async-init` for tracking
5. **Handle errors gracefully** in lifecycle hooks
6. **Provide autoload cookies** for public functions
7. **Document with docstrings** for discoverability

## Example Addons

- **emacs-mcp-cider.el** - CIDER/nREPL integration with async server startup
- **emacs-mcp-vibe-kanban.el** - Task management with npx subprocess
- **emacs-mcp-package-lint.el** - MELPA compliance tools

## Testing Your Addon

```elisp
;; Load addon
(emacs-mcp-addon-load 'my-addon)

;; Check if loaded
(emacs-mcp-addon-loaded-p 'my-addon)

;; Check if running (has active processes)
(emacs-mcp-addon-running-p 'my-addon)

;; Unload addon
(emacs-mcp-addon-unload 'my-addon)

;; View all addons
M-x emacs-mcp-addon-info
```
