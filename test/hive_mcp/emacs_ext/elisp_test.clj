(ns hive-mcp.emacs-ext.elisp-test
  "The elisp-generation façade forwards to the registered builders. The
   builders registered here are hive-spi.editor.elisp's — the same pure
   functions hive-emacs re-exports — so the façade's answer is checked
   against the source of truth, not a copy."
  (:require [clojure.string :as str]
            [clojure.test :refer [deftest is testing use-fixtures]]
            [hive-mcp.emacs-ext.elisp :as elisp]
            [hive-mcp.test.stub.extensions :as ext-stub]
            [hive-schemas.test :refer [deftrifecta-from-schema]]
            [hive-spi.editor.elisp :as el]))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:private builders
  {:emacs/format-elisp                el/format-elisp
   :emacs/require-and-call            el/require-and-call
   :emacs/require-and-call-json       el/require-and-call-json
   :emacs/require-and-call-text       el/require-and-call-text
   :emacs/require-and-call-plist-json el/require-and-call-plist-json})

(use-fixtures :each (ext-stub/extensions-fixture builders))

(def ^:private Name
  "Elisp-shaped identifiers, so the generated source reads like real calls."
  [:enum 'hive-mcp-magit 'hive-mcp-api-status :hive-mcp-kanban "olympus-ui"])

(deftrifecta-from-schema require-and-call-json-trifecta elisp/require-and-call-json
  {:in [:cat Name Name [:* [:or :int :string :boolean]]]
   :out elisp/ElispSource
   :rel (fn [[feature fn-sym & args] src]
          (and (= src (apply el/require-and-call-json feature fn-sym args))
               (str/includes? src (str "(require '" feature " nil t)"))
               (str/includes? src "json-encode")))
   :num-tests 200})

(deftrifecta-from-schema require-and-call-plist-json-trifecta elisp/require-and-call-plist-json
  {:in [:cat Name Name [:maybe [:map-of [:enum :a :limit :query] [:maybe [:or :int :string]]]]]
   :out elisp/ElispSource
   :rel (fn [[feature fn-sym params] src]
          (and (= src (el/require-and-call-plist-json feature fn-sym params))
               ;; nil-valued params are dropped from the plist
               (every? (fn [[k v]] (= (nil? v) (not (str/includes? src (str ":" (name k) " ")))))
                       params)))
   :num-tests 200})

(deftest each-builder-names-its-own-shape
  (is (= "(goto-line 42)" (elisp/format-elisp "(goto-line %d)" 42)))
  (is (str/includes? (elisp/require-and-call 'f 'g 1) "(error \"f not loaded\")"))
  (is (str/includes? (elisp/require-and-call-text 'f 'g) "\"Error: f not loaded\""))
  (is (not (str/includes? (elisp/require-and-call-text 'f 'g) "json-encode"))))

(deftest without-hive-emacs-every-builder-answers-nil
  (ext-stub/without-extensions
   (keys builders)
   (fn []
     (testing "a missing builder is a loud nil, not a stub string"
       (is (nil? (elisp/format-elisp "(x)")))
       (is (nil? (elisp/require-and-call 'f 'g)))
       (is (nil? (elisp/require-and-call-json 'f 'g)))
       (is (nil? (elisp/require-and-call-text 'f 'g)))
       (is (nil? (elisp/require-and-call-plist-json 'f 'g {})))))))
