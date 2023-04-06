;; By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.

;; 3
;; 7 4
;; 2 4 6
;; 8 5 9 3

;; That is, 3 + 7 + 4 + 9 = 23.

;; Find the maximum total from top to bottom of the triangle below:

;; 75
;; 95 64
;; 17 47 82
;; 18 35 87 10
;; 20 04 82 47 65
;; 19 01 23 75 03 34
;; 88 02 77 73 07 63 67
;; 99 65 04 28 06 16 70 92
;; 41 41 26 56 83 40 80 70 33
;; 41 48 72 33 47 32 37 16 94 29
;; 53 71 44 65 25 43 91 52 97 51 14
;; 70 11 33 28 77 73 17 78 39 68 17 57
;; 91 71 52 38 17 14 91 43 58 50 27 29 48
;; 63 66 04 68 89 53 67 30 73 16 69 87 40 31
;; 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23

;; NOTE: As there are only 16384 routes, it is possible to solve this problem by trying every route. However, Problem 67, is the same challenge with a triangle containing one-hundred rows; it cannot be solved by brute force, and requires a clever method! ;o)

(ns project-euler-clj.problem-18
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(def small-triangle-str
"3
7 4
2 4 6
8 5 9 3")

(def the-triangle-str
"75
95 64
17 47 82
18 35 87 10
20 04 82 47 65
19 01 23 75 03 34
88 02 77 73 07 63 67
99 65 04 28 06 16 70 92
41 41 26 56 83 40 80 70 33
41 48 72 33 47 32 37 16 94 29
53 71 44 65 25 43 91 52 97 51 14
70 11 33 28 77 73 17 78 39 68 17 57
91 71 52 38 17 14 91 43 58 50 27 29 48
63 66 04 68 89 53 67 30 73 16 69 87 40 31
04 62 98 27 23 09 70 98 73 93 38 53 60 04 23")
;; => #'project-euler-clj.problem-18/the-triangle-str

(defn parse-triangle
  [triangle-str]
  (map #(map (fn [n] (Integer/parseInt n)) (st/split % #" ")) (st/split-lines triangle-str)))
(def the-triangle (parse-triangle the-triangle-str))
(def small-triangle (parse-triangle small-triangle-str))

(defn get-ijth
  [triangle i j]
  (nth (nth triangle i) j))

(def triangle
  (loop [remain the-triangle
         triangle {}
         row 0]
    (if (= (count remain) 0)
        triangle
        (recur (rest remain)
               (conj (reduce conj
                             triangle
                             (map-indexed (fn [column value] {[row column] value})
                                          (first remain))))
               (inc row)))))

(defn filter-lower-triangle
  [[i j] triangle]
  (filter (fn [[[x y] v]] (<= j x) (<= y (- x i))) triangle))
(defn in-lower-cone?
  [[i j] [x y]]
  (and (<= j y) (<= (- x i) (- y j))))
(defn in-upper-cone?
  [[i j] [x y]]
  (and (>= j y) (>= (- x i) (- y j))))
;; project-euler-clj.core> (apply max-key val triangle)
;; [[7 0] 99]
;; project-euler-clj.core> (apply max-key #(nth % 1) ((comp rest sort) (filter-lower-triangle [7 0] triangle)))
;; [[14 7] 98]

;; (defn look-for-max-path
;;   [triangle]
;;   (loop []))

(defn path-seeds
  [length]
  (common/product (repeat length [0 1])))

(defn render-path
  [path-seed]
  (reduce (fn [acc-seq x] (conj acc-seq (+ x (last acc-seq))))
          [0]
          path-seed))

(defn get-loci
  [columns]
  (map #(map-indexed (fn [x y] (vector x y)) %) columns))


(defn paths-to-vals
  [triangle paths]
  (map #(map (fn [[i j]] (vector [i j] (get-ijth triangle i j))) %)
       paths))

(defn sum-vals
  [val-list]
  (reduce (fn [acc [[i j] val]] (+ val acc))
          0
          val-list))

(defn get-max-seq
  [val-lists]
  (apply max-key sum-vals val-lists))

(get-max-seq (paths-to-vals small-triangle
                            (get-loci (map render-path
                                           (path-seeds (dec (count small-triangle)))))))
