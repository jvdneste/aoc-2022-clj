(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as s]
   [clojure.test :refer [deftest is run-tests]]))

(defn result-of-lines [lines]
  (let []
    ))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest result-of-lines-test
  (let [lines-str "Sabqponm\nabcryxxl\naccszExk\nacctuvwj\nabdefghi"
        lines (s/split-lines lines-str)]
    (is (= 31 (result-of-lines lines)))))

(deftest result-of-file-test
  (is (= 1 (result-of-file "inputs/day12.input"))))

(run-tests)
