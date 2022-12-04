(ns main
  (:require
   [clojure.java.io]
   [clojure.test :refer [deftest is run-tests]]))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn inventory-parse [text]
  (let [length (count text)
        pivot (quot length 2)]
    (map set (split-at pivot text))))

(defn inventory-shared-item [inventory]
  (let [[part-1 part-2] inventory
        shared (filter #(contains? part-1 %) part-2)]
    (first shared)))

(defn item-priority [item]
  (let [num (int item)]
    (if (>= num 97)
      (- num 96)
      (+ 26 (- num 64)))))

(defn result-of-lines [lines]
  (let [inventories (map inventory-parse lines)
        shared-items (map inventory-shared-item inventories)
        priorities (map item-priority shared-items)]
    (reduce + priorities)))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest item-priority-test
  (is (= 1 (item-priority \a)))
  (is (= 26 (item-priority \z)))
  (is (= 27 (item-priority \A)))
  (is (= 52 (item-priority \Z))))

(deftest inventory-shared-item-test
  (is (= \p (inventory-shared-item (inventory-parse "vJrwpWtwJgWrhcsFMMfFFhFp"))))
  (is (= \L (inventory-shared-item (inventory-parse "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"))))
  (is (= \P (inventory-shared-item (inventory-parse "PmmdzqPrVvPwwTWBwg"))))
  (is (= \v (inventory-shared-item (inventory-parse "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"))))
  (is (= \t (inventory-shared-item (inventory-parse "ttgJtRGJQctTZtZT"))))
  (is (= \s (inventory-shared-item (inventory-parse "CrZsJsPPZsGzwwsLwLmpwMDw")))))

(deftest result-of-lines-test
  (let [line-1 "vJrwpWtwJgWrhcsFMMfFFhFp"
        line-2 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
        line-3 "PmmdzqPrVvPwwTWBwg"
        line-4 "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
        line-5 "ttgJtRGJQctTZtZT"
        line-6 "CrZsJsPPZsGzwwsLwLmpwMDw"
        lines [line-1 line-2 line-3 line-4 line-5 line-6]]
    (is (= 157 (result-of-lines lines)))))

(deftest result-of-file-test
  (is (= 8109 (result-of-file "inputs/day03.input"))))

(run-tests)
