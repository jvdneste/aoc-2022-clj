(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.test :refer [deftest is run-tests]]))

(defn advance [current item]
  (concat (rest current) (list item)))

(defn is-unique [current]
  (let [[_ items] current]
    (= 4 (count (set items)))))

(defn result-of-lines [lines]
  (let [line (first lines)
        [init input] (split-at 4 line)
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

(deftest result-of-file-test
  (is (= '(1888 [\b \q \g \m]) (result-of-file "inputs/day06.input"))))

(run-tests)
