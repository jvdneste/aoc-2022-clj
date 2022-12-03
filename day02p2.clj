(ns main
  (:require
   [clojure.java.io]
   [clojure.string :as str]
   [clojure.test :refer [deftest is run-tests]]))

(defrecord EncodedRound [left right]
  Object (toString [_] (format "EncodedRound{:left %s :right %s}" left right)))

(defn encoded-round [left right]
  (->EncodedRound left right))

(defn encoded-round-parse [line]
  (let [[left right] (str/split line #" ")]
    (encoded-round left right)))

(defrecord DecodedRound [opponent result encoded]
  Object (toString [_] (format "DecodedRound{:opponent %s :result %s :encoded %s}" opponent result encoded)))

(defn decode-opponent [value]
  (case value
    "A" :rock
    "B" :paper
    "C" :scissors))

(defn decode-result [value]
  (case value
    "X" :lose
    "Y" :draw
    "Z" :win))

(defn decoded-round [encoded]
  (let [{left :left right :right} encoded]
    (->DecodedRound (decode-opponent left) (decode-result right) encoded)))

(defn rounds-read-file [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (map encoded-round-parse (line-seq rdr)))))

(defrecord Score [opponent me]
  Object (toString [_] (format "Score{:opponent %d :me %d}" opponent me)))

(defn score [opponent me]
  (->Score opponent me))

(defn score-of-round [round]
  (let [{opponent :opponent result :result} round]
    (case [opponent result]
      [:rock :draw] (score 4 4)
      [:rock :win] (score 1 8)
      [:rock :lose] (score 7 3)
      [:paper :lose] (score 8 1)
      [:paper :draw] (score 5 5)
      [:paper :win] (score 2 9)
      [:scissors :win] (score 3 7)
      [:scissors :lose] (score 9 2)
      [:scissors :draw] (score 6 6)
      [nil nil] (score 0 0))))

(defn score-add [score-1 score-2]
  (score (+ (:opponent score-1) (:opponent score-2)) (+ (:me score-1) (:me score-2))))

(defn winner-of-rounds [rounds]
  (let [scores (map score-of-round rounds)]
    (reduce score-add scores)))

(defn winner-of-file [filename]
  (let [encoded-rounds (rounds-read-file filename)
        decoded-rounds (map decoded-round encoded-rounds)]
    (winner-of-rounds decoded-rounds)))

(deftest encoded-round-parse-test
  (is (= (encoded-round "A" "X") (encoded-round-parse "A X")))
  (is (= (encoded-round "B" "Y") (encoded-round-parse "B Y")))
  (is (= (encoded-round "C" "Z") (encoded-round-parse "C Z"))))

(deftest decoded-round-test
  (let [decoded (decoded-round (encoded-round "A" "X"))]
    (is (= :rock (:opponent decoded)))
    (is (= :lose (:result decoded))))
  (let [decoded (decoded-round (encoded-round "B" "Y"))]
    (is (= :paper (:opponent decoded)))
    (is (= :draw (:result decoded))))
  (let [decoded (decoded-round (encoded-round "C" "Z"))]
    (is (= :scissors (:opponent decoded)))
    (is (= :win (:result decoded)))))

(deftest winner-of-rounds-test
  (let [round-1 (decoded-round (encoded-round "A" "Y")) ; 4 4
        round-2 (decoded-round (encoded-round "B" "X")) ; 8 1
        round-3 (decoded-round (encoded-round "C" "Z")) ; 3 7
        rounds [round-1 round-2 round-3]]
    (is (= (score 15 12) (winner-of-rounds rounds)))))

(deftest winner-of-file-test
  (is (= (score 13658 10560) (winner-of-file "day02.input"))))

(run-tests)
