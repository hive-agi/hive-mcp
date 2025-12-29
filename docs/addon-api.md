# Addon API Reference

Complete reference for emacs-mcp addon development.

## Registration

### `emacs-mcp-addon-register`

Register an addon with the addon system.

```elisp
(emacs-mcp-addon-register ADDON &rest PROPS)
```

**ADDON**: Symbol identifying the addon (e.g., `'cider`, `'vibe-kanban`)

**PROPS**: Property list with the following keys:

| Key | Type | Required | Description |
|-----|------|----------|-------------|
| `:version` | string | No | Version string (e.g., "1.0.0") |
| `:description` | string | No | One-line description |
| `:requires` | list | No | Required features/packages |
| `:provides` | list | No | Provided features/commands |
| `:init` | function | No | Synchronous init hook |
| `:async-init` | function | No | Asynchronous init hook |
| `:shutdown` | function | No | Shutdown/cleanup hook |

**Example:**

```elisp
(emacs-mcp-addon-register
 'my-addon
 :version "1.0.0"
 :description "My awesome addon"
 :requires '(emacs-mcp-api some-package)
 :provides '(my-addon-mode my-addon-transient)
 :init #'my-addon--init
 :async-init #'my-addon--async-init
 :shutdown #'my-addon--shutdown)
```

## Lifecycle Hooks

### `:init`

**Signature:** `(defun my-addon--init ())`

Called synchronously immediately after the addon file is loaded.

- Runs before `:async-init`
- Should complete quickly (< 100ms)
- Errors are caught and logged

**Use cases:**
- `(require 'dependency nil t)`
- Setup keybindings
- Register hooks

### `:async-init`

**Signature:** `(defun my-addon--async-init ())`

Called after `:init`. Should not block Emacs.

- **Must return** a process object if starting a subprocess
- Returned process is automatically tracked for cleanup
- Errors are caught and logged

**Use cases:**
- Start external servers
- Spawn background processes
- Long-running initialization

**Example:**

```elisp
(defun my-addon--async-init ()
  (when my-addon-auto-start
    (start-process "server" "*server*" "my-command")))
```

### `:shutdown`

**Signature:** `(defun my-addon--shutdown ())`

Called when addon is unloaded via `emacs-mcp-addon-unload`.

- Cleanup is automatic for tracked processes/timers
- Use for additional cleanup (save state, remove hooks)
- Errors are caught and logged

**Example:**

```elisp
(defun my-addon--shutdown ()
  (save-some-state)
  (remove-hook 'some-hook #'my-function))
```

## Load/Unload Functions

### `emacs-mcp-addon-load`

Load an addon.

```elisp
(emacs-mcp-addon-load ADDON)
```

- Loads the addon file from addon directories
- Calls `:init` then `:async-init`
- Returns `t` if successful, `nil` otherwise

**Interactive:** `M-x emacs-mcp-addon-load`

### `emacs-mcp-addon-unload`

Unload an addon.

```elisp
(emacs-mcp-addon-unload ADDON)
```

- Calls `:shutdown` hook
- Kills tracked processes
- Cancels tracked timers
- Marks addon as unloaded

**Interactive:** `M-x emacs-mcp-addon-unload`

### `emacs-mcp-addon-restart`

Restart an addon.

```elisp
(emacs-mcp-addon-restart ADDON)
```

Equivalent to unload + load.

**Interactive:** `M-x emacs-mcp-addon-restart`

## Query Functions

### `emacs-mcp-addon-loaded-p`

Check if addon is loaded.

```elisp
(emacs-mcp-addon-loaded-p ADDON) ; => t or nil
```

### `emacs-mcp-addon-running-p`

Check if addon has active processes or timers.

```elisp
(emacs-mcp-addon-running-p ADDON) ; => t or nil
```

### `emacs-mcp-addon-available-p`

Check if addon file exists.

```elisp
(emacs-mcp-addon-available-p ADDON) ; => t or nil
```

### `emacs-mcp-addon-list-available`

List all available addons.

```elisp
(emacs-mcp-addon-list-available) ; => (cider vibe-kanban ...)
```

### `emacs-mcp-addon-list-loaded`

List currently loaded addons.

```elisp
(emacs-mcp-addon-list-loaded) ; => (cider ...)
```

## Process/Timer Management

### `emacs-mcp-addon-register-process`

Register a process for lifecycle tracking.

```elisp
(emacs-mcp-addon-register-process ADDON PROCESS)
```

Processes are automatically killed on addon unload.

**Note:** Returning a process from `:async-init` auto-registers it.

### `emacs-mcp-addon-register-timer`

Register a timer for lifecycle tracking.

```elisp
(emacs-mcp-addon-register-timer ADDON TIMER)
```

Timers are automatically cancelled on addon unload.

**Example:**

```elisp
(let ((timer (run-with-timer 1 1 #'my-tick)))
  (emacs-mcp-addon-register-timer 'my-addon timer))
```

### `emacs-mcp-addon-unregister-timer`

Unregister a timer.

```elisp
(emacs-mcp-addon-unregister-timer ADDON TIMER)
```

## Auto-Loading

### `emacs-mcp-addon-auto-load-list`

Customizable alist of `(ADDON . TRIGGER-FEATURE)` pairs.

```elisp
(setq emacs-mcp-addon-auto-load-list
      '((cider . cider)
        (org-ai . org-ai)
        (my-addon . my-package)))
```

When `TRIGGER-FEATURE` is loaded, `ADDON` is automatically loaded.

### `emacs-mcp-addon-always-load`

List of addons to always load on startup.

```elisp
(setq emacs-mcp-addon-always-load '(cider vibe-kanban))
```

### `emacs-mcp-addons-auto-load`

Enable auto-loading based on `emacs-mcp-addon-auto-load-list`.

```elisp
(emacs-mcp-addons-auto-load)
```

### `emacs-mcp-addons-load-always`

Load all addons in `emacs-mcp-addon-always-load`.

```elisp
(emacs-mcp-addons-load-always)
```

### `emacs-mcp-addons-setup`

Main entry point - loads always-load addons and enables auto-loading.

```elisp
(emacs-mcp-addons-setup)
```

Called automatically by `emacs-mcp-initialize` when `emacs-mcp-setup-addons` is non-nil.

## MCP API Functions

Available via `(require 'emacs-mcp-api)`:

### `emacs-mcp-api-get-context`

Get current context (buffer, project, git status).

```elisp
(emacs-mcp-api-get-context)
;; => (:buffer (:name "foo.el" :mode "emacs-lisp-mode")
;;     :project (:name "my-project" :root "/path/to/project")
;;     :git (:branch "main" :dirty t))
```

### `emacs-mcp-api-memory-add`

Add entry to persistent memory.

```elisp
(emacs-mcp-api-memory-add TYPE CONTENT TAGS)
```

- **TYPE**: "note", "snippet", "decision", etc.
- **CONTENT**: String content
- **TAGS**: List of tag strings

**Example:**

```elisp
(emacs-mcp-api-memory-add "snippet" "(defn foo [])" '("clojure" "function"))
```

### `emacs-mcp-api-memory-query`

Query persistent memory.

```elisp
(emacs-mcp-api-memory-query TYPE TAGS LIMIT)
```

Returns vector of matching entries.

## Customization Variables

### `emacs-mcp-addon-directories`

List of directories to search for addons.

```elisp
(add-to-list 'emacs-mcp-addon-directories "~/my-addons")
```

Default includes `elisp/addons/` in the emacs-mcp installation.

## Info Command

### `emacs-mcp-addon-info`

Display buffer with addon information.

```elisp
M-x emacs-mcp-addon-info
```

Shows:
- Available addons
- Loaded status
- Running status (has active processes)
- Lifecycle hooks defined
- Addon directories
