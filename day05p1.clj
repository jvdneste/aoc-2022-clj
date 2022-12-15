(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is run-tests]]))

(def ignored (set " []"))

(defn stack-add-item [stacks index item]
  (if (contains? ignored item)
    stacks
    (update stacks (quot index 4) conj item)))

(defn stack-line-add [stacks line]
  (reduce-kv stack-add-item stacks (vec line)))

(defn parse-stack-visualisation [stack-visualisation]
  (let [reversed-stack-lines (reverse stack-visualisation)
        stack-lines (rest reversed-stack-lines)]
    (reduce stack-line-add {} stack-lines)))

(defn parse-move-line [line]
  (let [matches (re-matches #"\w+\s(\d+)\s\w+\s(\d+)\s\w+\s(\d+)" line)
        [n from to] (map #(Integer/parseInt %) (rest matches))]
    [n (dec from) (dec to)]))

(defn parse-move-descriptions [move-descriptions]
  (map parse-move-line move-descriptions))

(defn parse-lines [lines]
  (let [[stack-lines move-lines] (split-with #(not (str/starts-with? % "move")) lines)
        stacks (parse-stack-visualisation stack-lines)
        moves (parse-move-descriptions move-lines)]
    [stacks moves]))

(defn apply-move [stacks move]
  (let [[n from to] move
        from-stack (get stacks from)
        [from-removed from-remaining] (split-at n from-stack)
        with-removed (assoc stacks from from-remaining)]
    (update with-removed to #(concat (reverse from-removed) %))))

(defn result-of-lines [lines]
  (let [[stacks moves] (parse-lines lines)]
    (reduce apply-move stacks moves)))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest parse-stack-visualisation-test
  (let [line-1 "    [D]    "
        line-2 "[N] [C]    "
        line-3 "[Z] [M] [P]"
        line-4 " 1   2   3 "
        lines [line-1 line-2 line-3 line-4]]
    (is (= {0 '(\N \Z), 1 '(\D \C \M), 2 '(\P)} (parse-stack-visualisation lines)))))

(deftest parse-move-line-test
  (is (= [3 1 8] (parse-move-line "move 3 from 2 to 9"))))

(deftest apply-move-test
  (is (= {0'(\D \N \Z), 1 '(\C \M), 2 '(\P)} (apply-move {0 '(\N \Z), 1 '(\D \C \M), 2 '(\P)} [1 1 0]))))

(deftest result-of-file-test
  (is (= {0 '(\T \V \J \H \1), 7 '(\C \P \B \M \Q \H \S \S \V \Z \8), 1' (\L \W \W \Z \T \J \F \2), 4 '(\B \G \Z \B \C \V \5), 6 '(\H \R \M \Z \W \C \7), 3 '(\G \P \D \4), 2 '(\F \R \P \W \B \3), 5 '(\Z \V \S \L \V \G \6), 8 '(\N \B \D \Q \D \J \L \R \T \9)} (result-of-file "inputs/day05.input"))))

(run-tests)
