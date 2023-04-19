;; Problem 39
;; If p is the perimeter of a right angle triangle with integral length sides, {a,b,c}, there are exactly three solutions for p = 120.

;; {20,48,52}, {24,45,51}, {30,40,50}

;; For which value of p ≤ 1000, is the number of solutions maximised?

(ns project-euler-clj.problem-039
  (:require [project-euler-clj.common :as common]))

(defn is-pythagorean-triangle?
  [a b c]
  (= (+ (* a a) (* b b))
     (* c c)))

(defn gen-triples-with-sum
  [sum]
  (apply concat (map #(map (fn [x] [% x (- sum (+ % x))])
                           (range 1 (- sum %)))
                     (range 1 (dec sum)))))

(defn solve
  []
  (last (sort-by (fn [[m triple]] (count triple))
                 (filter (fn [[m triple]] (< 0 (count triple)))
                         (map (fn [sum]
                                [sum
                                 (filter (fn [y] (apply is-pythagorean-triangle? y))
                                         (gen-triples-with-sum sum))])
                              (range 3 1001))))))
