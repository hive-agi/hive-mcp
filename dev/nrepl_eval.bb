#!/usr/bin/env bb
;; usage: bb dev/nrepl_eval.bb <file.clj|->   evaluates against nREPL on 7947
(require '[bencode.core :as b])
(let [code (slurp (let [f (first *command-line-args*)] (if (= f "-") *in* f)))
      sock (java.net.Socket. "127.0.0.1" 7947)
      in (java.io.PushbackInputStream. (.getInputStream sock))
      out (.getOutputStream sock)
      s (fn [x] (if (bytes? x) (String. ^bytes x) x))]
  (b/write-bencode out {"op" "eval" "code" code "id" "1" "ns" "user"})
  (loop []
    (let [m (b/read-bencode in)
          m (into {} (map (fn [[k v]] [(s k) v]) m))]
      (when-let [o (m "out")] (print (s o)) (flush))
      (when-let [o (m "err")] (print (s o)) (flush))
      (when-let [o (m "value")] (println "=>" (s o)))
      (when-let [o (m "ex")] (println "EX" (s o)))
      (if (some #(= "done" (s %)) (m "status"))
        nil
        (recur)))))
(System/exit 0)
