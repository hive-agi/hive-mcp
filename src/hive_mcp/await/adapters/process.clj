(ns hive-mcp.await.adapters.process
  "Adapter: the IProcessRunner the :k8s-log-regex source runs kubectl
   through. ProcessBuilder over an argv vector (no shell), stdin closed,
   stdout capped at :max-bytes, the process destroyed (forcibly) when it
   outlives :timeout-ms, so a hung kubectl never outlives its poll."
  (:require [hive-mcp.await.sources.boundary :as b])
  (:import (java.io ByteArrayOutputStream InputStream)
           (java.lang ProcessBuilder$Redirect)
           (java.util.concurrent TimeUnit)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(defn- drain
  "A future reading `in` into a byte buffer, keeping at most `cap` bytes
   (the rest is read and dropped, so the child never blocks on a full pipe)."
  [^InputStream in cap]
  (future
    (let [buf (ByteArrayOutputStream.)
          chunk (byte-array 8192)]
      (loop [truncated? false]
        (let [n (.read in chunk)]
          (if (neg? n)
            {:text (.toString buf "UTF-8") :truncated? truncated?}
            (let [room (- (long cap) (.size buf))]
              (when (pos? room) (.write buf chunk 0 (int (min n room))))
              (recur (or truncated? (> n room))))))))))

(defrecord ProcessRunner []
  b/IProcessRunner
  (run-process! [_ argv {:keys [timeout-ms max-bytes]}]
    (let [pb (doto (ProcessBuilder. ^java.util.List (mapv str argv))
               (.redirectInput ProcessBuilder$Redirect/PIPE))
          p  (.start pb)
          _  (.close (.getOutputStream p))
          out (drain (.getInputStream p) (or max-bytes (* 4 1024 1024)))
          err (drain (.getErrorStream p) 65536)
          done? (.waitFor p (long timeout-ms) TimeUnit/MILLISECONDS)]
      (when-not done?
        (.destroyForcibly p)
        (.waitFor p 2000 TimeUnit/MILLISECONDS))
      (let [o (deref out 2000 {:text "" :truncated? true})
            e (deref err 2000 {:text "" :truncated? true})]
        {:exit (if done? (.exitValue p) -1)
         :out (:text o)
         :err (:text e)
         :timed-out? (not done?)
         :truncated? (boolean (:truncated? o))}))))

(def default-runner
  "The process runner."
  (->ProcessRunner))
