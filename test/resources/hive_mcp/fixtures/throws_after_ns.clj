(ns hive-mcp.fixtures.throws-after-ns
  "A subdomain whose load throws in a body form AFTER its ns form while the
   system property hive-mcp.fixtures.throws-after-ns.fail is set, and loads
   cleanly once it is cleared. The ns form has already registered the lib in
   clojure.core/*loaded-libs* when the throw comes, which is what a retry
   must see past. While hive-mcp.fixtures.throws-after-ns.no-var is set it
   loads without its handlers var.")

(when-not (System/getProperty "hive-mcp.fixtures.throws-after-ns.no-var")
  (def handlers
    {:catchup (fn [_] {:type "text" :text "caught up"})})
  (def tools
    [{:name "throws-after-ns"
      :inputSchema {:properties {"command"   {:type "string"}
                                 "max_slots" {:type "integer" :description "fixture param"}}}}]))

(when (System/getProperty "hive-mcp.fixtures.throws-after-ns.fail")
  (throw (ex-info "fixture body form threw after its ns form" {:fixture ::throws-after-ns})))
