(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.test :refer [deftest is run-tests]]))

(defn tick-turn [state index item]
  (let [{operation :operation modulo :mod [t f] :targets} (get state index)
        stress (operation (bigint item))
        stress-reduced (rem stress (apply * (map :mod state)))
        target (if (= 0 (rem stress modulo)) t f)]
    (update-in state [target :items] #(conj % stress-reduced))))

(defn tick-round [state index]
  (let [items (get-in state [index :items])
        state-cleared (assoc-in state [index :items] [])
        state-with-counter (update-in state-cleared [index :counter] #(+ % (count items)))]
    (reduce (fn [state item] (tick-turn state index item)) state-with-counter items)))

(defn tick-round-full [state indices]
  (reduce tick-round state indices))

(defn result-of-lines [monkeys]
  (let [monkeys-with-counters (vec (map #(assoc % :counter 0) monkeys))
        monkeys-after-twenty-rounds (reduce tick-round-full monkeys-with-counters (repeat 10000 (range (count monkeys))))
        top-monkeys (sort > (map :counter monkeys-after-twenty-rounds))]
    (apply * (take 2 top-monkeys))))

(def test-monkeys [{:items [79 98]
                    :operation #(* % 19)
                    :mod 23
                    :targets [2 3]}
                   {:items [54 65 75 74]
                    :operation #(+ % 6)
                    :mod 19
                    :targets [2 0]}
                   {:items [79 60 97]
                    :operation #(* % %)
                    :mod 13
                    :targets [1 3]}
                   {:items [74]
                    :operation #(+ % 3)
                    :mod 17
                    :targets [0 1]}])

(deftest test-result-test
  (is (= 2713310158 (result-of-lines test-monkeys))))

(def initial-monkeys [{:items [65 78]
                       :operation #(* % 3)
                       :mod 5
                       :targets [2 3]}
                      {:items [54 78 86 79 73 64 85 88]
                       :operation #(+ % 8)
                       :mod 11
                       :targets [4 7]}
                      {:items [69 97 77 88 87]
                       :operation #(+ % 2)
                       :mod 2
                       :targets [5 3]}
                      {:items [99]
                       :operation #(+ % 4)
                       :mod 13
                       :targets [1 5]}
                      {:items [60 57 52]
                       :operation #(* % 19)
                       :mod 7
                       :targets [7 6]}
                      {:items [91 82 85 73 84 53]
                       :operation #(+ % 5)
                       :mod 3
                       :targets [4 1]}
                      {:items [88 74 68 56]
                       :operation #(* % %)
                       :mod 17
                       :targets [0 2]}
                      {:items [54 82 72 71 53 99 67]
                       :operation inc
                       :mod 19
                       :targets [6 0]}])

(deftest result-test
  (is (= 23612457316 (result-of-lines initial-monkeys))))

(run-tests)