(ns hive-mcp.tools.kanban.create-scope
  "Which project a kanban create lands in when the caller names one.

   Friction 20261023210335-286ec3d5: multi DSL `b+` with project_id=\"desargues\"
   wrote the cards into the session cwd project and answered success. The
   create path read only the directory, so project_id (and a scope:project tag)
   were dropped. A create now honours the named project, or refuses loudly."
  (:require [clojure.string :as str]
            [malli.core :as m]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private scope-prefix "scope:project:")

(defn- tagged-project
  "The project of the first scope:project:<id> tag in `tags`, nil when none."
  [tags]
  (some (fn [t]
          (when (and (string? t) (str/starts-with? t scope-prefix))
            (subs t (count scope-prefix))))
        (when (sequential? tags) tags)))

(defn explicit-project-id
  "The project id a create's params name: :project_id (or :project-id), else a
   scope:project:<id> tag in :tags. nil when neither is given, so the caller's
   directory decides. Throws ex-info {:type :validation-error} when the
   project_id is blank or not a string, or disagrees with a scope:project tag:
   a create never lands in a project other than the one the caller named."
  [params]
  (let [params   (or params {})
        explicit (if (contains? params :project_id) (:project_id params) (:project-id params))
        tagged   (tagged-project (:tags params))]
    (when (and (some? explicit) (or (not (string? explicit)) (str/blank? explicit)))
      (throw (ex-info (str "kanban create: project_id must be a non-blank string, got "
                           (pr-str explicit))
                      {:type :validation-error})))
    (when (and explicit tagged (not= explicit tagged))
      (throw (ex-info (str "kanban create: project_id " (pr-str explicit)
                           " disagrees with the tag " scope-prefix tagged
                           "; give only one of them")
                      {:type :validation-error})))
    (or explicit tagged)))

(m/=> explicit-project-id [:=> [:cat [:maybe :map]] [:maybe :string]])
