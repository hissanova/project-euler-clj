;; Problem 57
;; <p>It is possible to show that the square root of two can be expressed as an infinite continued fraction.</p>
;; <p class="center">$\sqrt 2 =1+ \frac 1 {2+ \frac 1 {2 +\frac 1 {2+ \dots}}}$</p>
;; <p>By expanding this for the first four iterations, we get:</p>
;; <p>$1 + \frac 1 2 = \frac  32 = 1.5$<br />
;; $1 + \frac 1 {2 + \frac 1 2} = \frac 7 5 = 1.4$<br />
;; $1 + \frac 1 {2 + \frac 1 {2+\frac 1 2}} = \frac {17}{12} = 1.41666 \dots$<br />
;; $1 + \frac 1 {2 + \frac 1 {2+\frac 1 {2+\frac 1 2}}} = \frac {41}{29} = 1.41379 \dots$<br /></p>
;; <p>The next three expansions are $\frac {99}{70}$, $\frac {239}{169}$, and $\frac {577}{408}$, but the eighth expansion, $\frac {1393}{985}$, is the first example where the number of digits in the numerator exceeds the number of digits in the denominator.</p>
;; <p>In the first one-thousand expansions, how many fractions contain a numerator with more digits than the denominator?</p>
(ns project-euler-clj.problem-057
  (:require [project-euler-clj.common :as common]))

(defn get-sqrt-appr
  [iter]
  (if (= iter 0)
    (rationalize 1)
    (+ 1 (/ (+ 1 (get-sqrt-appr (dec iter)))))))

(defn solve
  []
  (count (filter (fn [fr] (> (count (str (numerator fr)))
                             (count (str (denominator fr)))))
                 (map get-sqrt-appr (range 1 1001)))))
