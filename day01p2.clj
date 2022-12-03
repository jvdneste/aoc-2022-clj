(ns main
  (:require
   [clojure.java.io]
   [clojure.test :refer [deftest is run-tests]]))

(defn empty-inventory? [coll]
  (or (empty? coll) (empty? (first coll))))

(defn read-inventories-str [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (filter #(not (empty-inventory? %)) (partition-by empty? (line-seq rdr))))))

(defn inventory-parse [inventory-str]
  (map #(Integer/parseInt %) inventory-str))

(defn read-inventories-num [filename]
  (let [inventories-str (read-inventories-str filename)]
    (map inventory-parse inventories-str)))

(defn sum-top-three [inventories]
  (let [sums (map #(reduce + %) inventories)
        sorted-sums (sort > sums)
        top-three (take 3 sorted-sums)]
    (reduce + top-three)))

(deftest input-test
  (let [inventories (read-inventories-num "day01.input")
        result (sum-top-three inventories)]
    (is (= 199357 result))))

(run-tests)
