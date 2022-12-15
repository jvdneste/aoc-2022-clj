(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as s]
   [clojure.test :refer [deftest is run-tests]]))

(defn execute-noop [state] [state])
  
(defn execute-addx [state args]
  (let [x (Integer/parseInt (first args))]
    [state (+ state x)]))

(defn execute [states instruction]
  (let [state (last states)
        [op & args] instruction]
    (case op
      "noop" (execute-noop state)
      "addx" (execute-addx state args))))

(defn parse-line [line]
  (s/split line #" "))

(defn result-of-lines [lines]
  (let [state 1
        instructions (map parse-line lines)
        states (vec (mapcat identity (reductions execute [state state] instructions)))
        cycles [20 60 100 140 180 220]
        strengths (map #(* (get states %) %) cycles)]
    (apply + strengths)))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest result-of-file-test-test
  (is (= 13140 (result-of-file "inputs/day10.test.input"))))

(deftest result-of-file-test
  (is (= 16020 (result-of-file "inputs/day10.input"))))

(run-tests)
