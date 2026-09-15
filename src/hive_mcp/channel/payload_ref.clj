(ns hive-mcp.channel.payload-ref
  "Elide an over-budget payload to a CONTEXTUAL ID the reader can fetch.

   The channel already bounded what one shout may cost a reader, by truncating
   at a character cap and appending an ellipsis. That bounds the cost and LOSES
   the tail: nothing anywhere holds what was cut, so the information is gone the
   moment it is too long.

   Storing the full body under an id and putting the id in the row costs the
   same handful of characters on the wire and loses nothing. The reader sees the
   head of the message plus `[ref:<id>]`, and fetches the rest only if the head
   turns out to matter. That is the whole trade: a reader pays full price only
   for what it decides to read.

   Pure calculation here (`needs-ref?`, `preview`, `render`); the one effectful
   entry point (`elide!`) takes the store as an argument, so a test drives it
   with a plain atom and nothing reaches the live context store."
  (:require [clojure.string :as str]))
;; Copyright (C) 2026 Pedro Gomes Branquinho (BuddhiLW) <pedrogbranquinho@gmail.com>
;;
;; SPDX-License-Identifier: AGPL-3.0-or-later

(def ^:const default-cap
  "Characters a payload may occupy inline before it is worth a reference.
   Below this the id plus marker costs more than the text it replaces."
  400)

(def ^:const ref-marker-cost
  "Characters `render` spends on the marker itself: \" [ref:]\" plus a ctx id.
   Used to keep the rendered result inside the cap rather than just under it
   before the marker is appended."
  48)

(defn needs-ref?
  "Is `s` long enough that a reference costs less than the text?"
  ([s] (needs-ref? s default-cap))
  ([s cap]
   (and (string? s) (> (count s) cap))))

(defn preview
  "The head of `s` that travels inline, cut at a word boundary when one is
   near the end so the reader gets whole words rather than a severed one."
  [s budget]
  (if-not (and (string? s) (pos? budget) (> (count s) budget))
    s
    (let [head (subs s 0 budget)
          last-space (str/last-index-of head " ")]
      (if (and last-space (> last-space (int (* 0.6 budget))))
        (subs head 0 last-space)
        head))))

(defn render
  "The inline text for an elided payload: its preview plus the fetch marker.
   `ctx-id` is opaque; the reader hands it back to the store to get the body."
  [preview-text ctx-id]
  (str preview-text "… [ref:" ctx-id "]"))

(defn parse-ref
  "The context id inside a rendered payload, or nil when there is none. The
   inverse of `render` for the one field a reader acts on."
  [s]
  (when (string? s)
    (second (re-find #"\[ref:([^\]]+)\]" s))))

(defn elide!
  "Bound `s` to `cap` characters, keeping what does not fit.

   `put!` is called with the FULL string and must return an id the reader can
   fetch by; it is called only when the payload actually exceeds the cap, so a
   swarm of short messages never touches the store.

   Returns {:text <what travels> :ref <id>} when elided, {:text s} otherwise.
   A `put!` that throws or returns nothing degrades to a plain truncation
   rather than failing the shout — a message that arrives cut is worth more
   than a message that does not arrive."
  ([s put!] (elide! s default-cap put!))
  ([s cap put!]
   (if-not (needs-ref? s cap)
     {:text s}
     (let [head (preview s (max 1 (- cap ref-marker-cost)))
           id (try (put! s) (catch Exception _ nil))]
       (if id
         {:text (render head id) :ref (str id)}
         {:text (str head "…")})))))
