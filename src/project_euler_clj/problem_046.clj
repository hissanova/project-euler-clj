;; Problem 46
;; <p>It was proposed by Christian Goldbach that every odd composite number can be written as the sum of a prime and twice a square.</p>
;; <p class="margin_left">9 = 7 + 2×1<sup>2</sup><br />
;; 15 = 7 + 2×2<sup>2</sup><br />
;; 21 = 3 + 2×3<sup>2</sup><br />
;; 25 = 7 + 2×3<sup>2</sup><br />
;; 27 = 19 + 2×2<sup>2</sup><br />
;; 33 = 31 + 2×1<sup>2</sup></p>
;; <p>It turns out that the conjecture was false.</p>
;; <p>What is the smallest odd composite that cannot be written as the sum of a prime and twice a square?</p>

(ns project-euler-clj.problem-046
  (:require [project-euler-clj.common :as common]))

(defn is-a-square?
  [n]
  (every? even? (map :exp (common/prime-factor n))))

(defn find-sums
  [n primes-upto-n]
  (filter (fn [x] (is-a-square? (nth x 1)))
          (map (fn [x] [x
                        (/ (- n x) 2)])
               primes-upto-n)))

(defn solve
  []
  (let [odd-seq (rest (filter odd? (range)))]
  (loop [remaing-seq odd-seq
         wanted-sums []
         primes-so-far []]
    (let [n (first remaing-seq)]
      (if (common/is-prime? n)
        (recur (rest remaing-seq)
               wanted-sums
               (conj primes-so-far n))
        (if-let [sums (not-empty (find-sums n primes-so-far))]
          (recur (rest remaing-seq)
                 (conj wanted-sums sums)
                 primes-so-far)
          n))))))

