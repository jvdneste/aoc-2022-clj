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

(defn render-cell [index x]
  (let [sprite (contains? #{(dec x) x (inc x)} (dec index))]
    (if sprite "#" ".")))

(defn render-row [row]
  (map-indexed render-cell row))

(defn render [states]
  (let [rows (partition 40 states)]
    (map render-row rows)))

(defn result-of-lines [lines]
  (let [x 1
        instructions (map parse-line lines)
        states (vec (mapcat identity (reductions execute [x x] instructions)))]
    (doseq [row (render states)]
      (println (s/join row)))))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(result-of-file "inputs/day10.input")
