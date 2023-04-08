;; Problem 24
;; A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. The lexicographic permutations of 0, 1 and 2 are:

;; 012   021   102   120   201   210

;; What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

(ns project-euler-clj.problem-024
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))


(def target (int 1e6))

(def memo-factorial (memoize (fn [n] (common/factorial n))))

(defn is-target-within?
  [target inf sup]
  (and (<= inf target) (<= target sup)))


(defn get-the-list
  []
  (loop [source-digits (range 10)
         num-remains 9
         current-pos 0
         solution []]
    (if (= 0 num-remains)
      (concat solution source-digits)
      (let [[index digit] (first (filter (fn [[i d]]
                                           (is-target-within?
                                            target
                                            (+ current-pos (* i (memo-factorial num-remains)))
                                            (+ current-pos (* (inc i) (memo-factorial num-remains)))))
                                         (map-indexed vector source-digits)))]
        (recur (remove #{digit} source-digits)
               (dec num-remains)
               (+ current-pos (* index (memo-factorial num-remains)))
               (conj solution digit))))))

(defn get-the-list2
  []
  (loop [source-digits (range 10)
         remainder (int 1e6)
         solution []]
    (println source-digits remainder solution)
    (if (= (count source-digits) 0)
      solution
      (let [sublen (dec (count source-digits))
            quotient (quot remainder (memo-factorial sublen))
            new-rem (rem remainder (memo-factorial sublen))
            index  (if (= new-rem 0)
                     (dec quotient)
                     quotient)
            digit (nth source-digits index)]
        (recur (remove #{digit} source-digits)
               (if (= new-rem 0)
                 (memo-factorial sublen)
                 new-rem)
               (conj solution digit))))))

(get-the-list)
(get-the-list2)
(defn solve []
  (reduce + (map (fn [[i d]]  (* d (int (Math/pow 10 i))))
                 (map-indexed vector (reverse (get-the-list))))))
