(ns hive-mcp.fixtures.requires-missing-dep
  "A subdomain present on the classpath whose ns form requires a namespace
   that is not: the load throws for a DEPENDENCY, which is a failure to
   answer loudly, not an absent subdomain."
  (:require [hive-mcp.fixtures.no-such-dependency]))

(def handlers
  {:catchup (fn [_] {:type "text" :text "caught up"})})
