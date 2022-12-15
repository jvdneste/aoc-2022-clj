(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as s]
   [clojure.test :refer [deftest is run-tests]]))

(defn head-move [move]
  (case move
    "U" [0 1]
    "D" [0 -1]
    "L" [-1 0]
    "R" [1 0]))

(defn parse-line [line]
  (let [[direction steps-str] (s/split line #" ")
        steps (Integer/parseInt steps-str)
        move (head-move direction)]
    (repeat steps move)))

(defn tail-move [offset move]
  (let [[ox oy] offset
        [mx my] move
        [ax ay] [(+ ox mx) (+ oy my)]
        too-far (or (> (Math/abs ax) 1) (> (Math/abs ay) 1))]
    (if too-far [(Integer/signum ax) (Integer/signum ay)] [0 0])))

(defn position-move-pair [move pair]
  (let [[[hx hy] [tx ty]] pair
        offset [(- hx tx) (- hy ty)]]
    (tail-move offset move)))

(defn position-move [position move]
  (let [position-pairs (partition 2 1 position)
        moves (reductions position-move-pair move position-pairs)]
    (map (fn [[x y] [dx dy]] [(+ x dx) (+ y dy)]) position moves)))

(defn result-of-lines [lines knots]
  (let [moves (mapcat parse-line lines)
        initial (repeat knots [0 0])
        path (reductions position-move initial moves)
        tail-positions (set (map last path))]
    (count tail-positions)))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines 10)))

(deftest result-of-lines-test-1
  (let [lines-str "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
        lines (s/split-lines lines-str)]
    (is (= 13 (result-of-lines lines 2)))))

(deftest result-of-lines-test-2
  (let [lines-str "R 4\nU 4\nL 3\nD 1\nR 4\nD 1\nL 5\nR 2"
        lines (s/split-lines lines-str)]
    (is (= 1 (result-of-lines lines 10)))))

(deftest result-of-lines-test-3
  (let [lines-str "R 5\nU 8\nL 8\nD 3\nR 17\nD 10\nL 25\nU 20"
        lines (s/split-lines lines-str)]
    (is (= 36 (result-of-lines lines 10)))))

(deftest result-of-file-test
  (is (= 2627 (result-of-file "inputs/day09.input"))))

(run-tests)
