(ns hive-mcp.await.adapters.jdbc
  "Adapter: the ISqlRunner the :sql-predicate await source queries Postgres
   through. Plain java.sql, so the only runtime need is the PostgreSQL JDBC
   driver on the classpath.

   READ-ONLY BY CONSTRUCTION, in layers, every one applied on every query:
     1. the source's guard refused anything but one SELECT at create time;
     2. a fresh connection with autocommit off, setReadOnly, then
        `SET TRANSACTION READ ONLY` as the transaction's first statement,
        so Postgres itself refuses a write (SQLSTATE 25006);
     3. `SET LOCAL statement_timeout` plus the JDBC query timeout;
     4. setMaxRows(row-cap + 1): at most row-cap rows are read, and the
        extra one only says the result was truncated;
     5. the transaction is ALWAYS rolled back, and the connection closed.
   Values are bound through a PreparedStatement (setObject per `?`), never
   spliced into the text.

   The connection value is a URL from a secret: `jdbc:postgresql://..` or
   `postgres(ql)://user:pass@host:port/db?..`. It is never logged and never
   appears in an error this adapter raises."
  (:require [clojure.string :as str]
            [hive-mcp.await.sources.boundary :as b]
            [malli.core :as m])
  (:import (java.net URI URLDecoder)
           (java.sql Connection DriverManager PreparedStatement ResultSet
                     ResultSetMetaData SQLException)
           (java.util Properties)))

;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def connect-timeout-s
  "Seconds to wait for a connection."
  10)

(defn- decode [s] (when s (URLDecoder/decode ^String s "UTF-8")))

(defn connection-spec
  "{:jdbc-url s :user s? :password s?} for a connection URL. A
   postgres:// URL's credentials move out of the URL into properties. Pure."
  [url]
  (let [u (str/trim (str url))]
    (cond
      (str/starts-with? u "jdbc:postgresql:")
      {:jdbc-url u}

      (re-find #"^postgres(ql)?://" u)
      (let [uri  (URI. u)
            [usr pw] (some-> (.getRawUserInfo uri) (str/split #":" 2))]
        (cond-> {:jdbc-url (str "jdbc:postgresql://" (.getHost uri)
                                (when (pos? (.getPort uri)) (str ":" (.getPort uri)))
                                (or (.getRawPath uri) "")
                                (when-let [q (.getRawQuery uri)] (str "?" q)))}
          usr (assoc :user (decode usr))
          pw  (assoc :password (decode pw))))

      :else
      (throw (ex-info "await sql-predicate: the secret is not a postgres connection URL"
                      {:error :await/bad-connection})))))

(m/=> connection-spec [:=> [:cat :any] [:map [:jdbc-url :string]]])

(defn- props [{:keys [user password]} timeout-ms]
  (doto (Properties.)
    (cond-> user (.setProperty "user" user))
    (cond-> password (.setProperty "password" password))
    (.setProperty "connectTimeout" (str connect-timeout-s))
    (.setProperty "socketTimeout" (str (+ 5 (quot (long timeout-ms) 1000))))
    (.setProperty "ApplicationName" "hive-await-sql-predicate")
    (.setProperty "readOnly" "true")))

(defn- read-rows [^ResultSet rs row-cap]
  (let [^ResultSetMetaData md (.getMetaData rs)
        cols (mapv #(keyword (.getColumnLabel md (int %))) (range 1 (inc (.getColumnCount md))))]
    (loop [rows [] n 0]
      (if (.next rs)
        (if (>= n row-cap)
          {:rows rows :truncated? true}
          (recur (conj rows (into {} (map-indexed (fn [i c] [c (.getObject rs (int (inc i)))])) cols))
                 (inc n)))
        {:rows rows :truncated? false}))))

(defn- plain-value
  "A JDBC value as plain data the registry can persist as EDN."
  [v]
  (cond
    (or (nil? v) (string? v) (boolean? v) (integer? v) (double? v)) v
    (instance? java.math.BigDecimal v) v
    (number? v) v
    :else (str v)))

(defrecord JdbcRunner []
  b/ISqlRunner
  (run-select! [_ {:keys [url sql params row-cap statement-timeout-ms]}]
    (let [spec (connection-spec url)
          t-ms (long statement-timeout-ms)]
      (try
        (with-open [^Connection c (DriverManager/getConnection ^String (:jdbc-url spec)
                                                               (props spec t-ms))]
          (.setAutoCommit c false)
          (.setReadOnly c true)
          (try
            (with-open [s (.createStatement c)]
              (.execute s "SET TRANSACTION READ ONLY")
              ;; an integer of our own, not caller text
              (.execute s (str "SET LOCAL statement_timeout = " t-ms)))
            (with-open [^PreparedStatement ps (.prepareStatement c ^String sql)]
              (.setQueryTimeout ps (int (max 1 (quot (+ t-ms 999) 1000))))
              (.setMaxRows ps (int (inc (long row-cap))))
              (doseq [[i v] (map-indexed vector params)]
                (.setObject ps (int (inc i)) v))
              (with-open [rs (.executeQuery ps)]
                (update (read-rows rs (long row-cap)) :rows
                        (fn [rows] (mapv #(update-vals % plain-value) rows)))))
            (finally
              (.rollback c))))
        (catch SQLException e
          ;; the message names the SQL error, never the URL or a credential
          (throw (ex-info (str "await sql-predicate: query failed: " (.getMessage e))
                          {:error :await/sql-failed :sql-state (.getSQLState e)})))))))

(def default-runner
  "The process SQL runner."
  (->JdbcRunner))
