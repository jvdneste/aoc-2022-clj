(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.test :refer [deftest is run-tests]]))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn group-shared-item [group]
  (let [shared (apply clojure.set/intersection group)]
    (first shared)))

(defn item-priority [item]
  (let [num (int item)]
    (if (>= num 97)
      (- num 96)
      (+ 26 (- num 64)))))

(defn result-of-lines [lines]
  (let [inventories (map set lines)
        groups (partition 3 inventories)
        shared-items (map group-shared-item groups)
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

(deftest result-of-lines-test
  (let [line-1 "vJrwpWtwJgWrhcsFMMfFFhFp"
        line-2 "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL"
        line-3 "PmmdzqPrVvPwwTWBwg"
        line-4 "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn"
        line-5 "ttgJtRGJQctTZtZT"
        line-6 "CrZsJsPPZsGzwwsLwLmpwMDw"
        lines [line-1 line-2 line-3 line-4 line-5 line-6]]
    (is (= 70 (result-of-lines lines)))))

(deftest result-of-file-test
  (is (= 2738 (result-of-file "inputs/day03.input"))))

(run-tests)
