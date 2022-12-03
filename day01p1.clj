(ns main
  (:import
   (java.nio.file Paths))
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

(defn max-inventory [inventories]
  (apply max (map #(reduce + %) inventories)))

(deftest input-test
  (let [inventories (read-inventories-num "day01.input")
        result (max-inventory inventories)]
    (is (= 67450 result))))

(run-tests)
