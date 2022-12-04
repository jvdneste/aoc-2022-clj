(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is run-tests]]))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn interval-parse [text]
  (let [number-strings (str/split text #"-")]
    (map #(Integer/parseInt %) number-strings)))

(defn line-parse [line]
  (let [assignments  (str/split line #",")
        [[x1 y1] [x2 y2]] (map interval-parse assignments)]
    [[x1 (inc y1)] [x2 (inc y2)]]))

(defn allen-relation [assignment]
  (let [[[p1lower p1upper] [p2lower p2upper]] assignment]
    (if (< p1lower p2lower)
      (if (< p1upper p2lower)
        :precedes
        (if (= p1upper p2lower)
          :meets
          (if (< p1upper p2upper)
            :overlaps
            (if (= p1upper p2upper)
              :is-finished-by
              :contains))))
      (if (= p1lower p2lower)
        (if (< p1upper p2upper)
          :starts
          (if (= p1upper p2upper)
            :equal
            :is-started-by))
        (if (< p1lower p2upper)
          (if (< p1upper p2upper)
            :during
            (if (= p1upper p2upper)
              :finishes
              :is-overlapped-by))
          (if (= p1lower p2upper)
            :is-met-by
            :is-preceded-by))))))

(defn overlaps? [relation]
  (contains? #{:contains :during :starts :is-started-by :finishes :is-finished-by :equal :overlaps :is-overlapped-by} relation))

(defn result-of-lines [lines]
  (let [parsed-lines (map line-parse lines)
        allen-relations (map allen-relation parsed-lines)]
    (count (filter overlaps? allen-relations))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest allen-relation-test
  (is (= :precedes (allen-relation [[1 4] [5 8]])))
  (is (= :is-preceded-by (allen-relation [[5 8] [1 4]])))
  (is (= :meets (allen-relation [[1 4] [4 7]])))
  (is (= :is-met-by (allen-relation [[4 7] [1 4]])))
  (is (= :overlaps (allen-relation [[1 4] [3 6]])))
  (is (= :is-overlapped-by (allen-relation [[3 6] [1 4]])))
  (is (= :starts (allen-relation [[1 4] [1 6]])))
  (is (= :is-started-by (allen-relation [[1 6] [1 4]])))
  (is (= :during (allen-relation [[2 4] [1 6]])))
  (is (= :contains (allen-relation [[1 6] [2 4]])))
  (is (= :is-finished-by (allen-relation [[1 4] [2 4]])))
  (is (= :finishes (allen-relation [[2 4] [1 4]])))
  (is (= :equal (allen-relation [[1 4] [1 4]]))))

(deftest line-parse-test
  (is (= [[2 5] [6 9]] (line-parse "2-4,6-8"))))

(deftest result-of-lines-test
  (let [line-1 "2-4,6-8"
        line-2 "2-3,4-5"
        line-3 "5-7,7-9"
        line-4 "2-8,3-7"
        line-5 "6-6,4-6"
        line-6 "2-6,4-8"
        lines [line-1 line-2 line-3 line-4 line-5 line-6]]
    (is (= 4 (result-of-lines lines)))))

(deftest result-of-file-test
  (is (= 900 (result-of-file "inputs/day04.input"))))

(run-tests)
