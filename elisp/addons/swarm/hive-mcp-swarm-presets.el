;;; hive-mcp-swarm-presets.el --- Preset management for swarm -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; Preset management for hive-mcp-swarm.
;; Handles loading presets from disk (.md files) and memory system.
;;
;; Design principles (SOLID/CLARITY):
;; - Single Responsibility: Only handles preset loading/building
;; - Open/Closed: New preset sources via extension, not modification
;; - Liskov Substitution: All preset sources return same format
;;
;; Preset sources (priority order):
;; 1. Memory-based (project-scoped conventions tagged "swarm-preset")
;; 2. File-based (.md files from preset directories)

;;; Code:

(require 'cl-lib)

;; Soft dependency on memory system
(declare-function hive-mcp-memory-query "hive-mcp-memory")

;;;; Customization:

(defgroup hive-mcp-swarm-presets nil
  "Preset management for swarm."
  :group 'hive-mcp-swarm
  :prefix "hive-mcp-swarm-presets-")

(defcustom hive-mcp-swarm-presets-dir
  (expand-file-name "presets" (file-name-directory
                               (or load-file-name buffer-file-name
                                   default-directory)))
  "Directory containing built-in preset markdown files."
  :type 'directory
  :group 'hive-mcp-swarm-presets)

(defcustom hive-mcp-swarm-presets-custom-dirs nil
  "List of custom directories to scan for preset .md files.
Directories are scanned recursively for .md files only."
  :type '(repeat directory)
  :group 'hive-mcp-swarm-presets)

;;;; Internal State:

(defvar hive-mcp-swarm-presets--cache nil
  "Cache of loaded presets (name -> file-path).")

;;;; File-Based Presets:

(defun hive-mcp-swarm-presets--scan-dir (dir)
  "Recursively scan DIR for .md files, return alist of (name . path)."
  (when (and dir (file-directory-p dir))
    (let ((files (directory-files-recursively dir "\\.md$" nil)))
      (mapcar (lambda (f)
                (cons (file-name-sans-extension (file-name-nondirectory f)) f))
              files))))

(defun hive-mcp-swarm-presets--load-all ()
  "Load all presets from built-in and custom directories.
Returns hash-table of name -> file-path."
  (let ((presets (make-hash-table :test 'equal)))
    ;; Built-in presets
    (dolist (entry (hive-mcp-swarm-presets--scan-dir hive-mcp-swarm-presets-dir))
      (puthash (car entry) (cdr entry) presets))
    ;; Custom presets (can override built-in)
    (dolist (dir hive-mcp-swarm-presets-custom-dirs)
      (dolist (entry (hive-mcp-swarm-presets--scan-dir dir))
        (puthash (car entry) (cdr entry) presets)))
    presets))

(defun hive-mcp-swarm-presets-reload ()
  "Reload all presets from disk."
  (interactive)
  (setq hive-mcp-swarm-presets--cache (hive-mcp-swarm-presets--load-all))
  (message "Loaded %d presets" (hash-table-count hive-mcp-swarm-presets--cache)))

(defun hive-mcp-swarm-presets--ensure-loaded ()
  "Ensure presets are loaded."
  (unless hive-mcp-swarm-presets--cache
    (hive-mcp-swarm-presets-reload)))

(defun hive-mcp-swarm-presets--get-file-content (name)
  "Get content of file-based preset NAME."
  (hive-mcp-swarm-presets--ensure-loaded)
  (when-let* ((path (gethash name hive-mcp-swarm-presets--cache)))
    (with-temp-buffer
      (insert-file-contents path)
      (buffer-string))))

;;;; Memory-Based Presets:

(defun hive-mcp-swarm-presets--list-memory ()
  "List preset names from memory system (conventions tagged swarm-preset)."
  (when (fboundp 'hive-mcp-memory-query)
    (let ((entries (hive-mcp-memory-query 'convention
                                          '("swarm-preset")
                                          nil 100 nil nil)))
      (cl-remove-duplicates
       (cl-remove-if-not
        #'identity
        (mapcar (lambda (e)
                  (cl-find-if (lambda (tag)
                                (and (not (string-prefix-p "scope:" tag))
                                     (not (string= tag "swarm-preset"))))
                              (plist-get e :tags)))
                entries))
       :test #'string=))))

(defun hive-mcp-swarm-presets--get-memory-content (name)
  "Get preset NAME from memory system (conventions tagged swarm-preset).
Memory-based presets allow project-scoped and semantically searchable presets."
  (when (fboundp 'hive-mcp-memory-query)
    (let ((entries (hive-mcp-memory-query 'convention
                                          (list "swarm-preset" name)
                                          nil 1 nil nil)))
      (when entries
        (plist-get (car entries) :content)))))

;;;; Public API:

(defun hive-mcp-swarm-presets-list ()
  "List all available presets (file-based + memory-based)."
  (interactive)
  (hive-mcp-swarm-presets--ensure-loaded)
  (let* ((file-presets (hash-table-keys hive-mcp-swarm-presets--cache))
         (memory-presets (hive-mcp-swarm-presets--list-memory))
         (all-names (cl-remove-duplicates
                     (append file-presets memory-presets)
                     :test #'string=)))
    (if (called-interactively-p 'any)
        (message "Available presets: %s (file: %d, memory: %d)"
                 (string-join (sort all-names #'string<) ", ")
                 (length file-presets)
                 (length memory-presets))
      all-names)))

(defun hive-mcp-swarm-presets-get (name)
  "Get content of preset NAME.
Priority: memory-based (project-scoped) -> file-based (.md fallback).
This allows project-specific overrides of global file presets."
  (or (hive-mcp-swarm-presets--get-memory-content name)
      (hive-mcp-swarm-presets--get-file-content name)))

(defun hive-mcp-swarm-presets-build-system-prompt (presets)
  "Build combined system prompt from list of PRESETS.
Returns concatenated content with separator, or nil if no presets found."
  (let ((contents '()))
    (dolist (preset presets)
      (when-let* ((content (hive-mcp-swarm-presets-get preset)))
        (push content contents)))
    (when contents
      (mapconcat #'identity (nreverse contents) "\n\n---\n\n"))))

(defun hive-mcp-swarm-presets-add-custom-dir (dir)
  "Add DIR to custom preset directories and reload."
  (interactive "DPresets directory: ")
  (add-to-list 'hive-mcp-swarm-presets-custom-dirs dir)
  (hive-mcp-swarm-presets-reload))

;;;; Role Mapping:

(defcustom hive-mcp-swarm-presets-role-mapping
  '(("tester" . ("tester" "tdd"))
    ("reviewer" . ("reviewer" "solid" "clarity"))
    ("documenter" . ("documenter"))
    ("refactorer" . ("refactorer" "solid" "clarity"))
    ("researcher" . ("researcher"))
    ("fixer" . ("fixer" "tdd"))
    ("clarity-dev" . ("clarity" "solid" "ddd" "tdd"))
    ("coordinator" . ("task-coordinator"))
    ("ling" . ("ling" "minimal"))
    ("worker" . ("ling")))
  "Mapping of role names to preset lists.
Each entry is (ROLE . PRESETS-LIST)."
  :type '(alist :key-type string :value-type (repeat string))
  :group 'hive-mcp-swarm-presets)

(defun hive-mcp-swarm-presets-role-to-presets (role)
  "Convert ROLE to list of preset names."
  (or (cdr (assoc role hive-mcp-swarm-presets-role-mapping))
      (list role)))

;;;; Lifecycle:

(defun hive-mcp-swarm-presets-init ()
  "Initialize presets module."
  (hive-mcp-swarm-presets-reload))

(defun hive-mcp-swarm-presets-shutdown ()
  "Shutdown presets module."
  (setq hive-mcp-swarm-presets--cache nil))

(provide 'hive-mcp-swarm-presets)
;;; hive-mcp-swarm-presets.el ends here
