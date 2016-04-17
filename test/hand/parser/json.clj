(ns hand.parser.json
  (:require [hand.parser :refer (lex parse process-file)]
            [clojure.java.io :as io]))

(def conjv (fnil conj []))

(declare lex-punctuation lex-keyword
         lex-start-number lex-number lex-decimal-point lex-decimal
         lex-start-string lex-string lex-escape)

(defn lex-start [value ^Character chr]
  (cond (Character/isLetter chr)
        (lex-keyword value chr)

        (or (Character/isDigit chr) (= \- chr))
        (lex-start-number value chr)

        (= \" chr)
        (lex-start-string value chr)

        (Character/isWhitespace chr)
        [lex-start :next]

        :otherwise
        (lex-punctuation value chr)))

(defn lex-punctuation [value ^Character chr]
  (case chr
    \{ [lex-start :token [:open-curly]]
    \} [lex-start :token [:close-curly]]
    \[ [lex-start :token [:open-bracket]]
    \] [lex-start :token [:close-bracket]]
    \, [lex-start :token [:comma]]
    \: [lex-start :token [:colon]]
    nil))

(defn lex-start-string [_ ^Character chr]
  (if (= \" chr)
    [lex-string :next []]))

(defn lex-string [value ^Character chr]
  (cond (= \\ chr) [lex-escape :next value]
        (= \" chr) [lex-start :token [:string (apply str value)]]
        (> (int chr) 32) [lex-string :next (conj value chr)]))

(defn lex-escape [value ^Character chr]
  ; TODO: use proper whitelist and allow \uXXXX
  [lex-string :next (conj value chr)])

(defn finish-number [value]
  [:number (Double/valueOf ^String (apply str value))])

(defn lex-start-number [value ^Character chr]
  (cond (= \- chr) [lex-number :next [chr]]
        (= \0 chr) [lex-decimal-point :next [chr]]
        (Character/isDigit chr) [lex-number :next [chr]]))

(defn lex-number [value ^Character chr]
  (cond (Character/isDigit chr) [lex-number :next (conj value chr)]
        (= \. chr) (lex-decimal-point value chr)
        :else [lex-start :token-end (finish-number value)]))

(defn lex-decimal-point [value ^Character chr]
  (if (= \. chr)
    [lex-decimal :next (conj value chr)]))

(declare parse-record-field parse-record-value)

(defn parse-begin-record [stack [tok-type :as tok]]
  (case tok-type
    :word
    (parse-record-field (cons {} stack) tok)
    :eof
    [:eof]
    nil))

(defn parse-record [[fields :as stack] [tok-type :as tok]]
  (case tok-type
    :word
    (parse-record-field stack tok)
    :divider
    [:emit parse-begin-record (cons {:type :record :fields fields} stack)]
    nil))

(defn parse-record-field [stack [tok-type tok-value]]
  (if (= :word tok-type)
    [:next parse-record-value (cons tok-value stack)]))

(defn parse-record-value [[field fields & stack] [tok-type tok-value]]
  (if (#{:word :number} tok-type)
    [:next parse-record (cons (assoc fields field tok-value) stack)]))

(defn lex-and-parse []
  (comp
    (lex lex-start)
    (parse parse-record)))

(def lex-file (partial process-file (lex lex-start)))
(def parse-file (partial process-file (lex-and-parse)))

(defn run []
  (prn "lex")
  (prn (lex-file (io/resource "hand/parser/json.json")))
  ; (prn "parse")
  ; (prn (parse-file (io/resource "hand/parser/json.json")))
  )
