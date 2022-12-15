(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.test :refer [deftest is run-tests]]))

(defn advance [current item]
  (concat (rest current) (list item)))

(defn is-unique [current]
  (let [[_ items] current]
    (= 14 (count (set items)))))

(defn result-of-lines [lines]
  (let [line (first lines)
        [init input] (split-at 14 line)
        all-chunks (reductions advance init input)
        all-chunks-indexed (map-indexed list all-chunks)
        beacon (first (filter is-unique all-chunks-indexed))]
    beacon))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest result-of-lines-test
  (is (= '(5 (\q \m \g \b \l \j \s \p \h \d \z \t \n \v)) (result-of-lines ["mjqjpqmgbljsphdztnvjfqwrcgsmlb"]))))

(deftest result-of-file-test
  (is (= '(2299 (\s \q \z \g \d \b \f \l \h \v \p \w \c \r)) (result-of-file "inputs/day06.input"))))

(run-tests)
