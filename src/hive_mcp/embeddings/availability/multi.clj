(ns hive-mcp.embeddings.availability.multi
  (:require [hive-spi.log.ports :as log-ports]))

(defmulti secret-available?
  "Open dispatch port on :impl. Contract: [:map [:impl :keyword]] -> :boolean.
   Unhandled dispatch -> false (an impl nobody taught us about is not usable),
   logged at :warn through the hive-spi log port. Extend via defmethod."
  :impl)

(defmethod secret-available? :default
  [subject]
  ;; Callers filter provider specs on this answer's truthiness: an error map
  ;; here would read as "available" and put an unknown impl on the ladder.
  (log-ports/warn "embeddings: no secret-available? method for impl"
                  (pr-str (:impl subject)) "- treating it as unavailable")
  false)
