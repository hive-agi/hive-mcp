# Contributing to emacs-mcp

## Development Setup

1. Clone the repository:
   ```bash
   git clone https://github.com/BuddhiLW/emacs-mcp.git
   cd emacs-mcp
   ```

2. Start nREPL for Clojure development:
   ```bash
   clojure -M:nrepl  # Port 7910
   ```

3. Load the Emacs package:
   ```elisp
   (add-to-list 'load-path "/path/to/emacs-mcp/elisp")
   (require 'emacs-mcp)
   (emacs-mcp-mode 1)
   ```

## Running Tests

```bash
clojure -M:test
```

## MELPA Submission

The package is submitted to MELPA with the following recipe:

```elisp
(emacs-mcp :fetcher github
           :repo "BuddhiLW/emacs-mcp"
           :files ("elisp/*.el"))
```

### Submission Steps

1. Fork [melpa/melpa](https://github.com/melpa/melpa)
2. Add recipe to `recipes/emacs-mcp`
3. Test locally:
   ```bash
   make recipes/emacs-mcp
   ```
4. Submit PR to MELPA repository

### Package Requirements

- [x] Proper headers (Author, Maintainer, URL, Keywords, Package-Requires)
- [x] SPDX-License-Identifier
- [x] `lexical-binding: t`
- [x] `;;;###autoload` cookies on public functions
- [x] Ends with `(provide 'emacs-mcp)` and `;;; emacs-mcp.el ends here`

## Code Style

- Follow [Emacs Lisp conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Tips.html)
- Use `emacs-mcp-` prefix for all public symbols
- Use `emacs-mcp--` prefix for private symbols
- Document all public functions with docstrings

## Commit Messages

Use conventional commits:
- `feat:` New features
- `fix:` Bug fixes
- `docs:` Documentation changes
- `chore:` Maintenance tasks
- `refactor:` Code restructuring

## Pull Requests

1. Create a feature branch from `main`
2. Make your changes
3. Test thoroughly
4. Submit PR with clear description

## License

By contributing, you agree that your contributions will be licensed under the MIT License.
