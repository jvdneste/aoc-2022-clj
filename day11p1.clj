(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as s]
   [clojure.test :refer [deftest is run-tests]]))

(defn tick-turn [state index item]
  (let [{operation :operation target-fn :target} (get state index)
        stress (quot (operation item) 3)
        target (target-fn stress)]
    (update-in state [target :items] #(conj % stress))))

(defn tick-round [state index]
  (let [items (get-in state [index :items])
        state-cleared (assoc-in state [index :items] [])
        state-with-counter (update-in state-cleared [index :counter] #(+ % (count items)))]
    (reduce (fn [state item] (tick-turn state index item)) state-with-counter items)))

(defn result-of-lines [monkeys]
  (let [monkeys-with-counters (vec (map #(assoc % :counter 0) monkeys))
        monkeys-after-twenty-rounds (reduce tick-round monkeys-with-counters (mapcat identity (repeat 20 (range (count monkeys)))))
        top-monkeys (sort > (map :counter monkeys-after-twenty-rounds))]
    (apply * (take 2 top-monkeys))))

(def test-monkeys [{:items [79 98]
                    :operation #(* % 19)
                    :target #(if (= 0 (rem % 23)) 2 3)}
                   {:items [54 65 75 74]
                    :operation #(+ % 6)
                    :target #(if (= 0 (rem % 19)) 2 0)}
                   {:items [79 60 97]
                    :operation #(* % %)
                    :target #(if (= 0 (rem % 13)) 1 3)}
                   {:items [74]
                    :operation #(+ % 3)
                    :target #(if (= 0 (rem % 17)) 0 1)}])

(deftest test-result-test
  (is (= 10605 (result-of-lines test-monkeys))))

(def initial-monkeys [{:items [65 78]
                       :operation #(* % 3)
                       :target #(if (= 0 (rem % 5)) 2 3)}
                      {:items [54 78 86 79 73 64 85 88]
                       :operation #(+ % 8)
                       :target #(if (= 0 (rem % 11)) 4 7)}
                      {:items [69 97 77 88 87]
                       :operation #(+ % 2)
                       :target #(if (= 0 (rem % 2)) 5 3)}
                      {:items [99]
                       :operation #(+ % 4)
                       :target #(if (= 0 (rem % 13)) 1 5)}
                      {:items [60 57 52]
                       :operation #(* % 19)
                       :target #(if (= 0 (rem % 7)) 7 6)}
                      {:items [91 82 85 73 84 53]
                       :operation #(+ % 5)
                       :target #(if (= 0 (rem % 3)) 4 1)}
                      {:items [88 74 68 56]
                       :operation #(* % %)
                       :target #(if (= 0 (rem % 17)) 0 2)}
                      {:items [54 82 72 71 53 99 67]
                       :operation inc
                       :target #(if (= 0 (rem % 19)) 6 0)}])

(deftest result-test
  (is (= 110264 (result-of-lines initial-monkeys))))

(run-tests)