;; Problem 74

;; <p>The number $145$ is well known for the property that the sum of the factorial of its digits is equal to $145$:
;; $$1! + 4! + 5! = 1 + 24 + 120 = 145.$$</p>
;; <p>Perhaps less well known is $169$, in that it produces the longest chain of numbers that link back to $169$; it turns out that there are only three such loops that exist:</p>
;; \begin{align}
;; &amp;169 \to 363601 \to 1454 \to 169\\
;; &amp;871 \to 45361 \to 871\\
;; &amp;872 \to 45362 \to 872
;; \end{align}
;; <p>It is not difficult to prove that EVERY starting number will eventually get stuck in a loop. For example,</p>
;; \begin{align}
;; &amp;69 \to 363600 \to 1454 \to 169 \to 363601 (\to 1454)\\
;; &amp;78 \to 45360 \to 871 \to 45361 (\to 871)\\
;; &amp;540 \to 145 (\to 145)
;; \end{align}
;; <p>Starting with $69$ produces a chain of five non-repeating terms, but the longest non-repeating chain with a starting number below one million is sixty terms.</p>
;; <p>How many chains, with a starting number below one million, contain exactly sixty non-repeating terms?</p>


(ns project-euler-clj.problem-074
  (:require [project-euler-clj.common :as common]))

(defn sum-fact-of-digits
  [n]
  (reduce +
              (map common/factorial
                   (common/num-to-digit-seq n))))

(defn- get-non-repeating-chain
  [init]
  (loop [x init
         chain []]
    (if (some #{x} chain)
      chain
      (recur (sum-fact-of-digits x)
             (conj chain x)))))

(get-non-repeating-chain 69)
(defn solve [limit len]
  (count (filter (fn [chain] (= len (count chain)))
                 (map get-non-repeating-chain
                      (range 1 limit)))))
