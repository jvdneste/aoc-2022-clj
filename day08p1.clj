(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as s]
   [clojure.test :refer [deftest is run-tests]]))

(defn tree-height-xy [trees x y]
  (get (get trees y) x))

(defn visible-tree? [[[_ _ h1] [_ _ h2]]]
  (< h1 h2))

(defn with-max [tree1 tree2]
  (let [[_ _ h1] tree1
        [x2 y2 h2] tree2]
    [x2 y2 (max h1 h2)]))

(defn visible-trees-strip [trees tree-strip]
  (let [with-heights (map (fn [[x y]] [x y (tree-height-xy trees x y)]) tree-strip)
        with-heights-max (reductions with-max (first with-heights) (rest with-heights))
        with-heights-pairs (partition 2 1 with-heights-max)
        visible-pairs (filter visible-tree? with-heights-pairs)
        visible-trees (map second visible-pairs)]
    (cons (first tree-strip) (map (fn [[x y _]] [x y]) visible-trees))))

(defn visible-trees [trees tree-strips]
  (apply concat (map #(visible-trees-strip trees %) tree-strips)))

(defn result-of-lines [lines]
  (let [trees (mapv (fn [line] (mapv #(Integer/parseInt (str %)) line)) lines)
        width (count (first trees))
        height (count trees)
        left-to-right (visible-trees trees (map (fn [y] (map (fn [x] [x y]) (range width))) (range height)))
        right-to-left (visible-trees trees (map (fn [y] (map (fn [x] [x y]) (reverse (range width)))) (range height)))
        top-to-bottom (visible-trees trees (map (fn [x] (map (fn [y] [x y]) (range height))) (range width)))
        bottom-to-top (visible-trees trees (map (fn [x] (map (fn [y] [x y]) (reverse (range height)))) (range width)))
        visible (set (concat left-to-right right-to-left top-to-bottom bottom-to-top))]
    (count visible)))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest visible-trees-strip-test
  (is (= [[0 0][3 0]] (visible-trees-strip [[3 0 3 7 3]] [[0 0] [1 0] [2 0] [3 0] [4 0]])))
  (is (= [[0 0] [1 0]] (visible-trees-strip [[2 5 5 1 2]] [[0 0] [1 0] [2 0] [3 0] [4 0]])))
  (is (= [[0 0]] (visible-trees-strip [[6 5 3 3 2]] [[0 0] [1 0] [2 0] [3 0] [4 0]])))
  (is (= [[0 0] [2 0] [4 0]] (visible-trees-strip [[3 3 5 4 9]] [[0 0] [1 0] [2 0] [3 0] [4 0]])))
  (is (= [[0 0] [1 0] [3 0]] (visible-trees-strip [[3 5 3 9 0]] [[0 0] [1 0] [2 0] [3 0] [4 0]]))))

(deftest result-of-lines-test
  (let [lines-str "30373\n25512\n65332\n33549\n35390"
        lines (s/split-lines lines-str)]
    (is (= 21 (result-of-lines lines)))))

(deftest result-of-file-test
  (is (= 1816 (result-of-file "inputs/day08.input"))))

(run-tests)
