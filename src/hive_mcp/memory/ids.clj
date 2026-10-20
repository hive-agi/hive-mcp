(ns hive-mcp.memory.ids
  "Leaf identity helpers for memory entries: content hashing, id generation, timestamps."
  (:require [malli.core :as m]
            [hive-spi.memory.ids :as ids]))

(def content-hash
  "Compute SHA-256 hash of normalized content."
  ids/content-hash)

;; Facade alias: `(def x spi/x)` captures the VALUE, so instrumentation of the
;; spi var never reaches a caller going through this facade — own m/=>.
;; :gen/elements keeps the regex outputs generatable (mg needs test.check.regexp
;; for bare :re).
(m/=> content-hash [:=> [:cat :any] [:and {:gen/elements ["0000000000000000000000000000000000000000000000000000000000000000"]} :string [:re #"^[0-9a-f]{64}$"]]])

(def generate-id
  "Generate a unique timestamped ID for memory entries."
  ids/generate-id)

(m/=> generate-id [:=> [:cat] [:and {:gen/elements ["20261019120000-00000000"]} :string [:re #"^\d{14}-[0-9a-f]{8}$"]]])

(def iso-timestamp
  "Return current ISO 8601 timestamp."
  ids/iso-timestamp)

(m/=> iso-timestamp [:=> [:cat] :string])
