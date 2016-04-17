(ns hand.parser
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

                 :token ; emit token
                 (do
                   (vswap! state assoc :value nil)
                   (rf acc value))

                 :token-end ; emit token but re-use current character
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
             (ensure-reduced (rf acc [:error tok]))
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

(defn process-file [xform filename]
  (with-open [stream (io/reader filename)]
    (let [reader (LineNumberReader. stream)]
      (.setLineNumber reader 1)
      (readuction reader xform conj))))
