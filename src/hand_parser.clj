(ns hand-parser
  (:require [clojure.java.io :as io])
  (:import [java.io Reader LineNumberReader]))

(defn readuction [^LineNumberReader reader xf f]
  (let [init (f)
        f (xf f)]
    (loop [acc init]
      (let [c (.read reader)]
        (if (= -1 c)
          (f acc)
          (let [c (char c)
                acc (f acc c)
                latest (-> acc unreduced last)]
            (if (= :error (first latest))
              (let [n (.getLineNumber reader)]
                (throw (ex-info (str "Unexpected `" (second latest) "` on line " n)
                                {:char c :line n})))
              (recur acc))))))))

(defn lex [initial]
  (fn [rf]
    (let [state (volatile! {:value nil :lexer initial})]
      (fn
        ([] (rf))
        ([acc] (rf (if (reduced? acc) acc (rf acc [:eof]))))
        ([acc chr]
         (let [{:keys [lexer value]} @state
               [lexer action value] (lexer value chr)]
           (if (nil? lexer)
             (ensure-reduced (rf acc [:error chr]))
             (do
               (vswap! state assoc :lexer lexer)
               (case action

                 :token ; -> emit token, re-consume current character
                 (do
                   (vswap! state assoc :value nil)
                   (recur (rf acc value) chr))

                 ; next -> ready to consume next character
                 :next
                 (do
                   (vswap! state assoc :value value)
                   acc))))))))))

(defn parse [initial]
  (fn [rf]
    (let [state (volatile! {:stack (list) :parser initial})]
      (fn
        ([] (rf))
        ([acc] (rf acc))
        ([acc tok]
         (let [{:keys [parser stack]} @state
               [action parser stack] (parser stack tok)]
           (if (nil? action)
             (throw (ex-info "failed to parse" {:token tok :stack stack}))
             ; (ensure-reduced (rf acc [:error tok]))
             (do
               (vswap! state assoc :parser parser)
               (case action
                 :emit
                 (let [[node & stack] stack]
                   (vswap! state assoc :stack stack)
                   (rf acc node))

                 :next
                 (do
                   (vswap! state assoc :stack stack)
                   acc)

                 :eof acc)))))))))

(def conjv (fnil conj []))

(defn lex-word [value ^Character chr]
  (cond (Character/isLetter chr)
        [lex-word :next (conjv value chr)]

        :else
        [lex-start :token [:word (apply str value)]]))

(defn lex-number [value ^Character chr]
  (cond (Character/isDigit chr)
        [lex-number :next (conjv value chr)]

        :else
        (let [num (Integer/valueOf ^String (apply str value))]
          [lex-start :token [:number num]])))

(defn lex-divider [value ^Character chr]
  (case chr
    \-
    [lex-divider :next (conjv value chr)]

    [lex-start :token [:divider]]))

(defn lex-start [value ^Character chr]
  (cond (Character/isLetter chr)
        (lex-word value chr)

        (Character/isDigit chr)
        (lex-number value chr)

        (= \- chr)
        (lex-divider value chr)

        (Character/isWhitespace chr)
        [lex-start :next]))

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

(defn process-file [xform filename]
  (with-open [stream (io/reader filename)]
    (let [reader (LineNumberReader. stream)]
      (.setLineNumber reader 1)
      (readuction reader xform conj))))

(def lex-file (partial process-file (lex lex-start)))
(def parse-file (partial process-file (lex-and-parse)))
