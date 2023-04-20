;; Problem 42
;; The nth term of the sequence of triangle numbers is given by, tn = ½n(n+1); so the first ten triangle numbers are:

;; 1, 3, 6, 10, 15, 21, 28, 36, 45, 55, ...

;; By converting each letter in a word to a number corresponding to its alphabetical position and adding these values we form a word value. For example, the word value for SKY is 19 + 11 + 25 = 55 = t10. If the word value is a triangle number then we shall call the word a triangle word.

;; Using words.txt (right click and 'Save Link/Target As...'), a 16K text file containing nearly two-thousand common English words, how many are triangle words?

(ns project-euler-clj.problem-042
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st])
  (:require [clojure.math.combinatorics :as combo]))


(def source-file "./src/project_euler_clj/data/p042_words.txt")
(def word-list (st/split (st/replace  (slurp source-file) "\"" "") #","))

(defn char-to-int
  [c]
  (inc (- (int c) (int \a))))
(char-to-int \A)

(defn word-to-num
  [word]
  (reduce + (map char-to-int
                 word)))

(defn lazy-triagle-nums
  ([] (lazy-triagle-nums 1 2))
  ([sum n] (cons sum (lazy-seq (lazy-triagle-nums (+ sum n) (inc n))))))

(take 5 (lazy-triagle-nums))

(defn is-triangular?
  [n]
  (= n (last (take-while #(<= % n)
                         (lazy-triagle-nums)))))

(is-triangular? 16)
(word-to-num "aaa")

(map st/lower-case
     word-list)

(count word-list)
(defn solve
  []
  (count (filter #(-> %
                      (st/lower-case)
                      (word-to-num)
                      (is-triangular?))
                 word-list)))
