(ns hive-mcp.hot.lifecycle.port
  "The seam between the lifecycle runner and a host it drives.

   Protocol stratum (CPPB). Two methods, role sized (ISP): the runner needs to
   act on a host and to look at it, nothing else. Any host (the model, a live
   hive-mcp reached through its hot handlers, a recording decorator) is
   substitutable for any other (LSP): the runner never learns which it has.")

(defprotocol IHotHost
  (apply-op! [host op]
    "Do Op (hive-mcp.hot.lifecycle.domain/Op) to the host. Returns an Outcome:
     :applied, :refused or :noop.")
  (observe [host]
    "The host as it looks now: an Observation."))
