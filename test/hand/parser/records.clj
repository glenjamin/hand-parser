(ns hand.parser.records
  (:require [hand.parser :refer (lex parse process-file)]
            [clojure.java.io :as io]))

(def conjv (fnil conj []))

(declare lex-word lex-number lex-divider)

(defn lex-start [value ^Character chr]
  (cond (Character/isLetter chr)
        (lex-word value chr)

        (Character/isDigit chr)
        (lex-number value chr)

        (= \- chr)
        (lex-divider value chr)

        (Character/isWhitespace chr)
        [lex-start :next]))

(defn lex-word [value ^Character chr]
  (cond (Character/isLetter chr)
        [lex-word :next (conjv value chr)]

        :else
        [lex-start :token-end [:word (apply str value)]]))

(defn lex-number [value ^Character chr]
  (cond (Character/isDigit chr)
        [lex-number :next (conjv value chr)]

        :else
        (let [num (Integer/valueOf ^String (apply str value))]
          [lex-start :token-end [:number num]])))

(defn lex-divider [value ^Character chr]
  (case chr
    \-
    [lex-divider :next (conjv value chr)]

    [lex-start :token-end [:divider]]))


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
  (prn (lex-file (io/resource "hand/parser/records.txt")))
  (prn "parse")
  (prn (parse-file (io/resource "hand/parser/records.txt"))))
