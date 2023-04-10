;; Problem 25

;; The Fibonacci sequence is defined by the recurrence relation:

;; Fn = Fn−1 + Fn−2, where F1 = 1 and F2 = 1.
;; Hence the first 12 terms will be:

;; F1 = 1
;; F2 = 1
;; F3 = 2
;; F4 = 3
;; F5 = 5
;; F6 = 8
;; F7 = 13
;; F8 = 21
;; F9 = 34
;; F10 = 55
;; F11 = 89
;; F12 = 144
;; The 12th term, F12, is the first term to contain three digits.

;; What is the index of the first term in the Fibonacci sequence to contain 1000 digits?

(ns project-euler-clj.problem-025
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(defn add-very-bigints
  [a-in b-in
   & {modulus :mod :or {modulus (bigint 1e100)}}]
  (loop [a (reverse (if (sequential? a-in) a-in [a-in]))
         b (reverse (if (sequential? b-in) b-in [b-in]))
         carry-over 0
         sum []]
    (if (and (empty? a) (empty? b) (= carry-over 0))
      sum
      (let [a-first (if (nil? (first a)) 0 (first a))
            b-first (if (nil? (first b)) 0 (first b))]
        (recur
         (rest a)
         (rest b)
         (bigint (quot (+ carry-over a-first b-first) modulus))
         (cons  (bigint (mod (+ carry-over a-first b-first) modulus)) sum))))))

(defn how-many-digits
  [very-bigint
   & {mod-exp :mod-exp :or {mod-exp 100}}]
  (+ (let [upper-part (first very-bigint)]
       (cond
         (= upper-part 0) 0
         (mod upper-part 10) (inc (int (Math/log10 upper-part)))
         :else (int (Math/ceil (Math/log10 upper-part)))))
     (* mod-exp (count (rest very-bigint)))))

(defn lazy-big-fibonacci-seq
  ([] (lazy-big-fibonacci-seq '(1N) '(1N)))
  ([n-2 n-1] 
   (cons n-2 (lazy-seq (lazy-big-fibonacci-seq n-1 (add-very-bigints n-2 n-1)))))
  ([mod-exp]
   (lazy-big-fibonacci-seq '(1N) '(1N) mod-exp))
  ([n-2 n-1 mod-exp]
   (cons n-2 (lazy-seq (lazy-big-fibonacci-seq n-1
                                               (add-very-bigints n-2
                                                                 n-1
                                                                 :mod (bigint (Math/pow 10 mod-exp)))
                                               mod-exp)))))

(defn solve [mod-exp]
  (take-last 2
             (take-while #(> 1000 (how-many-digits (nth %1 1) :mod-exp mod-exp))
                         (map-indexed vector (lazy-big-fibonacci-seq mod-exp)))))
