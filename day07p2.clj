(ns main
  (:require
   [clojure.java.io]
   [clojure.set]
   [clojure.string :as str]
   [clojure.test :refer [deftest is run-tests]]))

(defn process-command-cd [state line]
  (let [dirname (subs line 5)]
    (case dirname
      "/" (assoc state :current-work-dir [])
      ".." (update state :current-work-dir rest)
      (update state :current-work-dir #(cons dirname %)))))

(defn process-dir [state line]
  (let [dirname (subs line 4)
        path (cons dirname (:current-work-dir state))]
    (assoc-in state [:paths path] {:type :dir})))

(defn process-file [state line]
  (let [[_ size-str filename] (re-matches #"(\d+)\s(.*)" line)
        size (Integer/parseInt size-str)
        path (cons filename (:current-work-dir state))]
    (assoc-in state [:paths path] {:type :file :size size})))

(defn process-line [state line]
  (cond
    (str/starts-with? line "$ cd ") (process-command-cd state line)
    (str/starts-with? line "$ ls") state
    (str/starts-with? line "dir ") (process-dir state line)
    :else (process-file state line)))

(defn sizes-add-size [dir-sizes path size]
  (let [with-size-added (update dir-sizes path #(if (nil? %) size (+ % size)))]
    (if (empty? path)
      with-size-added
      (sizes-add-size with-size-added (rest path) size))))

(defn sizes-add [dir-sizes [path info]]
  (let [size (:size info)]
    (sizes-add-size dir-sizes (rest path) size)))

(defn calc-dir-sizes [paths]
  (let [files (filter (fn [[_ info]] (= :file (:type info))) paths)]
    (reduce sizes-add {} files)))

(defn result-of-lines [lines]
  (let [initial-state {:current-work-dir [] :paths {[] {:type :dir}}}
        state (reduce process-line initial-state lines)
        sizes (calc-dir-sizes (:paths state))
        space-needed (- 30000000 (- 70000000 (get sizes [])))
        sizes-filtered (filter (fn [[_ size]] (> size space-needed)) sizes)
        smallest (apply min-key second sizes-filtered)]
    (second smallest)))

(defn read-lines [filename]
  (with-open [rdr (clojure.java.io/reader filename)]
    (vec (line-seq rdr))))

(defn result-of-file [filename]
  (let [lines (read-lines filename)]
    (result-of-lines lines)))

(deftest process-line-test
  (let [state {:current-work-dir [] :paths {[] {:type :dir}}}]
    (is (= {:current-work-dir [], :paths {[] {:type :dir} ["filename.ext"] {:type :file :size 12135}}}
           (process-line state "12135 filename.ext")))
    (is (= {:current-work-dir [], :paths {[] {:type :dir} ["dirname"] {:type :dir}}}
           (process-line state "dir dirname")))
    (is (= {:current-work-dir ["dirname"] :paths {[] {:type :dir}}}
           (process-line state "$ cd dirname")))
    (is (= {:current-work-dir [], :paths {}}
           (process-line {:current-work-dir ["dirname"] :paths {}} "$ cd ..")))))

(deftest result-of-lines-test
  (let [lines-str "$ cd /\n$ ls\ndir a\n14848514 b.txt\n8504156 c.dat\ndir d\n$ cd a\n$ ls\ndir e\n29116 f\n2557 g\n62596 h.lst\n$ cd e\n$ ls\n584 i\n$ cd ..\n$ cd ..\n$ cd d\n$ ls\n4060174 j\n8033020 d.log\n5626152 d.ext\n7214296 k"
        lines (str/split-lines lines-str)]
    (is (= 24933642 (result-of-lines lines)))))

(deftest result-of-file-test
  (is (= 1300850 (result-of-file "inputs/day07.input"))))

(run-tests)
