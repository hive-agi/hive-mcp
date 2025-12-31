;;; emacs-mcp-projectile.el --- Projectile integration for emacs-mcp -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Pedro G. Branquinho
;; Author: Pedro G. Branquinho <pedrogbranquinho@gmail.com>
;; URL: https://github.com/BuddhiLW/emacs-mcp
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (projectile "2.0"))
;; Keywords: tools, project, mcp
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;
;; This addon integrates Projectile (project management) with emacs-mcp.
;;
;; Features:
;; - Get current project root and info
;; - List all known projectile projects
;; - Detect project type (npm, maven, lein, etc.)
;; - List project files with optional pattern filtering
;; - Find files in project by name
;; - Search/grep in project (uses rg if available)
;;
;; Usage:
;;   (emacs-mcp-addon-load 'projectile)
;;   M-x emacs-mcp-projectile-transient

;;; Code:

(require 'emacs-mcp-api)

;; Soft dependencies
(declare-function projectile-project-root "projectile")
(declare-function projectile-project-name "projectile")
(declare-function projectile-project-type "projectile")
(declare-function projectile-current-project-files "projectile")
(declare-function projectile-recentf-files "projectile")
(declare-function projectile-switch-project-by-name "projectile")
(declare-function projectile-project-p "projectile")
(declare-function projectile-invalidate-cache "projectile")

(defvar projectile-known-projects)

;; Forward declarations
(declare-function emacs-mcp-addon-register "emacs-mcp-addons")
(declare-function transient-define-prefix "transient")

;;;; Customization:

(defgroup emacs-mcp-projectile nil
  "Projectile integration for emacs-mcp."
  :group 'emacs-mcp
  :group 'projectile
  :prefix "emacs-mcp-projectile-")

(defcustom emacs-mcp-projectile-max-files 1000
  "Maximum number of files to return in project file listings."
  :type 'integer
  :group 'emacs-mcp-projectile)

(defcustom emacs-mcp-projectile-max-search-results 100
  "Maximum number of search results to return."
  :type 'integer
  :group 'emacs-mcp-projectile)

(defcustom emacs-mcp-projectile-use-ripgrep t
  "When non-nil, prefer ripgrep over grep for searching."
  :type 'boolean
  :group 'emacs-mcp-projectile)

;;;; Internal:

(defvar emacs-mcp-projectile--initialized nil
  "Whether the addon has been initialized.")

(defun emacs-mcp-projectile--ensure-projectile ()
  "Ensure projectile is available."
  (and (featurep 'projectile)
       (fboundp 'projectile-project-root)))

(defun emacs-mcp-projectile--in-project-p ()
  "Return non-nil if currently in a projectile project."
  (and (emacs-mcp-projectile--ensure-projectile)
       (projectile-project-p)))

(defun emacs-mcp-projectile--ripgrep-available-p ()
  "Return non-nil if ripgrep is available."
  (and emacs-mcp-projectile-use-ripgrep
       (executable-find "rg")))

;;;; Project Info:

(defun emacs-mcp-projectile--get-project-root ()
  "Get current projectile project root, or nil."
  (when (emacs-mcp-projectile--ensure-projectile)
    (condition-case nil
        (projectile-project-root)
      (error nil))))

(defun emacs-mcp-projectile--get-project-name ()
  "Get current projectile project name, or nil."
  (when (emacs-mcp-projectile--ensure-projectile)
    (condition-case nil
        (projectile-project-name)
      (error nil))))

(defun emacs-mcp-projectile--get-project-type ()
  "Get current projectile project type as a string."
  (when (emacs-mcp-projectile--ensure-projectile)
    (condition-case nil
        (let ((type (projectile-project-type)))
          (if type (symbol-name type) "generic"))
      (error "unknown"))))

(defun emacs-mcp-projectile--detect-extended-type (root)
  "Detect extended project type based on files in ROOT."
  (when root
    (let ((markers nil))
      (dolist (pair '(("package.json" . npm)
                      ("Cargo.toml" . cargo)
                      ("go.mod" . go-mod)
                      ("deps.edn" . clojure-deps)
                      ("project.clj" . leiningen)
                      ("shadow-cljs.edn" . shadow-cljs)
                      ("pyproject.toml" . python-pyproject)
                      ("pom.xml" . maven)
                      ("build.gradle" . gradle)
                      ("Makefile" . make)
                      ("Dockerfile" . docker)
                      (".git" . git)))
        (when (file-exists-p (expand-file-name (car pair) root))
          (push (cdr pair) markers)))
      (nreverse markers))))

;;;; File Operations:

(defun emacs-mcp-projectile--list-files (&optional pattern)
  "List project files, optionally filtered by PATTERN."
  (when (emacs-mcp-projectile--in-project-p)
    (let* ((files (projectile-current-project-files))
           (filtered (if pattern
                         (seq-filter
                          (lambda (f)
                            (or (string-match-p (regexp-quote pattern) f)
                                (string-match-p (wildcard-to-regexp pattern) f)))
                          files)
                       files)))
      (seq-take filtered emacs-mcp-projectile-max-files))))

(defun emacs-mcp-projectile--find-file-matches (filename)
  "Find files in project matching FILENAME."
  (when (emacs-mcp-projectile--in-project-p)
    (let* ((files (projectile-current-project-files))
           (matches (seq-filter
                     (lambda (f)
                       (or (string= (file-name-nondirectory f) filename)
                           (string-match-p (regexp-quote filename)
                                           (file-name-nondirectory f))))
                     files)))
      matches)))

(defun emacs-mcp-projectile--recent-files ()
  "Get recently visited files in current project."
  (when (emacs-mcp-projectile--in-project-p)
    (condition-case nil
        (projectile-recentf-files)
      (error nil))))

;;;; Search:

(defun emacs-mcp-projectile--search (pattern)
  "Search project for PATTERN using rg or grep."
  (when-let* ((root (emacs-mcp-projectile--get-project-root)))
    (let* ((default-directory root)
           (cmd (if (emacs-mcp-projectile--ripgrep-available-p)
                    (format "rg --line-number --no-heading --color=never -e %s ."
                            (shell-quote-argument pattern))
                  (format "grep -r -n -H -e %s ."
                          (shell-quote-argument pattern))))
           (output (shell-command-to-string cmd))
           (lines (split-string output "\n" t))
           (results nil))
      (dolist (line (seq-take lines emacs-mcp-projectile-max-search-results))
        (when (string-match "^\\([^:]+\\):\\([0-9]+\\):\\(.*\\)$" line)
          (push (list :file (match-string 1 line)
                      :line (string-to-number (match-string 2 line))
                      :content (string-trim (match-string 3 line)))
                results)))
      (nreverse results))))

;;;; MCP API Functions:

;;;###autoload
(defun emacs-mcp-projectile-api-project-info ()
  "Get current project info as plist."
  (when (emacs-mcp-projectile--in-project-p)
    (let ((root (emacs-mcp-projectile--get-project-root)))
      (list :name (emacs-mcp-projectile--get-project-name)
            :root root
            :type (emacs-mcp-projectile--get-project-type)
            :extended-types (emacs-mcp-projectile--detect-extended-type root)
            :file-count (length (projectile-current-project-files))
            :in-project t))))

;;;###autoload
(defun emacs-mcp-projectile-api-list-projects ()
  "List all known projectile projects."
  (if (emacs-mcp-projectile--ensure-projectile)
      (let ((projects (or (and (boundp 'projectile-known-projects)
                               projectile-known-projects)
                          nil)))
        (apply #'vector
               (mapcar (lambda (root)
                         (list :root root
                               :name (file-name-nondirectory
                                      (directory-file-name root))
                               :exists (file-directory-p root)
                               :types (emacs-mcp-projectile--detect-extended-type root)))
                       projects)))
    []))

;;;###autoload
(defun emacs-mcp-projectile-api-project-files (&optional pattern)
  "List files in current project, optionally filtered by PATTERN."
  (if (emacs-mcp-projectile--in-project-p)
      (apply #'vector (emacs-mcp-projectile--list-files pattern))
    []))

;;;###autoload
(defun emacs-mcp-projectile-api-find-file (filename)
  "Find files matching FILENAME in current project."
  (if (emacs-mcp-projectile--in-project-p)
      (let* ((matches (emacs-mcp-projectile--find-file-matches filename))
             (root (emacs-mcp-projectile--get-project-root)))
        (apply #'vector
               (mapcar (lambda (f)
                         (list :relative f
                               :absolute (expand-file-name f root)))
                       matches)))
    []))

;;;###autoload
(defun emacs-mcp-projectile-api-recent-files ()
  "Get recently visited files in current project."
  (if (emacs-mcp-projectile--in-project-p)
      (apply #'vector (or (emacs-mcp-projectile--recent-files) '()))
    []))

;;;###autoload
(defun emacs-mcp-projectile-api-search (pattern)
  "Search current project for PATTERN."
  (if (emacs-mcp-projectile--in-project-p)
      (apply #'vector (emacs-mcp-projectile--search pattern))
    []))

;;;; Interactive Commands:

;;;###autoload
(defun emacs-mcp-projectile-show-info ()
  "Display current project info."
  (interactive)
  (if (emacs-mcp-projectile--in-project-p)
      (let* ((info (emacs-mcp-projectile-api-project-info))
             (buf (get-buffer-create "*MCP Project Info*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert "=== Projectile Project Info ===\n\n")
          (insert (format "Name: %s\n" (plist-get info :name)))
          (insert (format "Root: %s\n" (plist-get info :root)))
          (insert (format "Type: %s\n" (plist-get info :type)))
          (insert (format "Extended Types: %s\n"
                          (mapconcat #'symbol-name
                                     (plist-get info :extended-types)
                                     ", ")))
          (insert (format "File Count: %d\n" (plist-get info :file-count)))
          (goto-char (point-min)))
        (display-buffer buf))
    (message "Not in a projectile project")))

;;;###autoload
(defun emacs-mcp-projectile-list-projects ()
  "Display all known projects."
  (interactive)
  (let* ((projects (emacs-mcp-projectile-api-list-projects))
         (buf (get-buffer-create "*MCP Known Projects*")))
    (with-current-buffer buf
      (erase-buffer)
      (insert "=== Known Projectile Projects ===\n\n")
      (if (= (length projects) 0)
          (insert "No projects found.\n")
        (dotimes (i (length projects))
          (let ((proj (aref projects i)))
            (insert (format "%d. %s\n   Root: %s\n   Exists: %s\n\n"
                            (1+ i)
                            (plist-get proj :name)
                            (plist-get proj :root)
                            (if (plist-get proj :exists) "yes" "NO"))))))
      (goto-char (point-min)))
    (display-buffer buf)))

;;;###autoload
(defun emacs-mcp-projectile-search-interactive (pattern)
  "Search project for PATTERN and display results."
  (interactive "sSearch pattern: ")
  (if (emacs-mcp-projectile--in-project-p)
      (let* ((results (emacs-mcp-projectile-api-search pattern))
             (buf (get-buffer-create "*MCP Project Search*")))
        (with-current-buffer buf
          (erase-buffer)
          (insert (format "=== Search Results for '%s' ===\n\n" pattern))
          (insert (format "Found: %d matches\n\n" (length results)))
          (if (= (length results) 0)
              (insert "No matches found.\n")
            (dotimes (i (length results))
              (let ((match (aref results i)))
                (insert (format "%s:%d: %s\n"
                                (plist-get match :file)
                                (plist-get match :line)
                                (plist-get match :content))))))
          (goto-char (point-min))
          (grep-mode))
        (display-buffer buf))
    (message "Not in a projectile project")))

;;;; Transient Menu:

;;;###autoload
(defun emacs-mcp-projectile-transient ()
  "MCP Projectile menu."
  (interactive)
  (if (require 'transient nil t)
      (progn
        (transient-define-prefix emacs-mcp-projectile--menu ()
          "MCP Projectile menu."
          ["emacs-mcp + Projectile"
           ["Info"
            ("i" "Project info" emacs-mcp-projectile-show-info)
            ("p" "List projects" emacs-mcp-projectile-list-projects)]
           ["Search"
            ("s" "Search" emacs-mcp-projectile-search-interactive)]])
        (emacs-mcp-projectile--menu))
    (message "Transient not available")))

;;;; Addon Lifecycle:

(defun emacs-mcp-projectile--addon-init ()
  "Initialize projectile addon."
  (require 'emacs-mcp-api nil t)
  (setq emacs-mcp-projectile--initialized t)
  (message "emacs-mcp-projectile: initialized"))

(defun emacs-mcp-projectile--addon-shutdown ()
  "Shutdown projectile addon."
  (setq emacs-mcp-projectile--initialized nil)
  (message "emacs-mcp-projectile: shutdown"))

;;;; Minor Mode:

;;;###autoload
(define-minor-mode emacs-mcp-projectile-mode
  "Minor mode for Projectile integration."
  :init-value nil
  :lighter " MCP-Proj"
  :global t
  :group 'emacs-mcp-projectile
  (if emacs-mcp-projectile-mode
      (emacs-mcp-projectile--addon-init)
    (message "emacs-mcp-projectile disabled")))

;;;; Addon Registration:

(with-eval-after-load 'emacs-mcp-addons
  (emacs-mcp-addon-register
   'projectile
   :version "0.1.0"
   :description "Projectile project management integration"
   :requires '(projectile emacs-mcp-api)
   :provides '(emacs-mcp-projectile-mode emacs-mcp-projectile-transient)
   :init #'emacs-mcp-projectile--addon-init
   :shutdown #'emacs-mcp-projectile--addon-shutdown))

(provide 'emacs-mcp-projectile)
;;; emacs-mcp-projectile.el ends here
