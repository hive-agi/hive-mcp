(ns hive-mcp.config.sensitive
  "Which config key names name a secret. A leaf: one regex shared by the
   config tool's response redaction and the config write policy."
  (:require [malli.core :as m]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def sensitive-name-re
  "A config key name that names a secret or an authenticator."
  #"(?i)(token|secret|passw|passphrase|credential|api[-_]?key|private[-_]?key|bearer|cookie|(^|[-_.])(keys?|pwd|auth|authz|authn|certs?|pins?|salt|signing|signature)($|[-_.]))")

(defn sensitive-name?
  "True when `k` (a keyword, string or symbol) names a secret."
  [k]
  (boolean (when (or (keyword? k) (string? k) (symbol? k))
             (re-find sensitive-name-re (name k)))))

(m/=> sensitive-name? [:=> [:cat :any] :boolean])
