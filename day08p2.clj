(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as s]
   [clojure.test :refer [deftest is run-tests]]))

(defn tree-height-xy [trees x y]
  (get (get trees y) x))

(defn viewing-distance [heights height]
  (let [max-distance (count heights)
        distance (count (take-while #(< % height) heights))]
    (min max-distance (inc distance))))

(defn scenic-score [tree-heights tree map-height map-width]
  (let [[x y h] tree
        right (map #(tree-height-xy tree-heights % y) (range (inc x) map-width))
        left (map #(tree-height-xy tree-heights % y) (reverse (range 0 x)))
        down (map #(tree-height-xy tree-heights x %) (range (inc y) map-height))
        up (map #(tree-height-xy tree-heights x %) (reverse (range 0 y)))
        viewing-distances (map #(viewing-distance % h) [right left up down])]
    (apply * viewing-distances)))

(defn result-of-lines [lines]
  (let [tree-heights (mapv (fn [line] (mapv #(Integer/parseInt (str %)) line)) lines)
        map-height (count tree-heights)
        map-width (count (first tree-heights))
        trees (mapcat identity (map-indexed (fn [y row] (map-indexed (fn [x height] [x y height]) row)) tree-heights))
        scenic-scores (map #(scenic-score tree-heights % map-height map-width) trees)]
    (apply max scenic-scores)))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest result-of-lines-test
  (let [lines-str "30373\n25512\n65332\n33549\n35390"
        lines (s/split-lines lines-str)]
    (is (= 8 (result-of-lines lines)))))

(deftest result-of-file-test
  (is (= 383520 (result-of-file "inputs/day08.input"))))

(run-tests)
