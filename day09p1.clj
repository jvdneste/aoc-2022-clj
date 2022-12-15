(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as s]
   [clojure.test :refer [deftest is run-tests]]))

(defn parse-line [line]
  (let [[direction steps-str] (s/split line #" ")
        steps (Integer/parseInt steps-str)]
    (repeat steps direction)))

(defn head-move [move]
  (case move
    "U" [0 1]
    "D" [0 -1]
    "L" [-1 0]
    "R" [1 0]))

(defn tail-move [offset move]
  (let [[ox oy] offset
        [mx my] move
        too-far (or (> (Math/abs (+ ox mx)) 1) (> (Math/abs (+ oy my)) 1))]
    (if too-far offset [0 0])))

(defn position-move [position move]
  (let [[[tx ty][hx hy]] position
        shift [(- hx tx) (- hy ty)]
        [hdx hdy] (head-move move)
        [tdx tdy] (tail-move shift [hdx hdy])]
    [[(+ tx tdx) (+ ty tdy)][(+ hx hdx) (+ hy hdy)]]))

(defn result-of-lines [lines]
  (let [moves (mapcat parse-line lines)
        start-position (repeat 10 [0 0])
        path (reductions position-move start-position moves)
        tail-positions (set (map first path))]
    (count tail-positions)))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest result-of-lines-test
  (let [lines-str "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
        lines (s/split-lines lines-str)]
    (is (= 13 (result-of-lines lines)))))

(deftest result-of-file-test
  (is (= 6357 (result-of-file "inputs/day09.input"))))

(run-tests)
