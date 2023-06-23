;; Problem 71

;; <p>Consider the fraction, $\dfrac n d$, where $n$ and $d$ are positive integers. If $n \lt d$ and $\operatorname{HCF}(n,d)=1$, it is called a reduced proper fraction.</p>
;; <p>If we list the set of reduced proper fractions for $d \le 8$ in ascending order of size, we get:
;; $$\frac 1 8, \frac 1 7, \frac 1 6, \frac 1 5, \frac 1 4, \frac 2 7, \frac 1 3, \frac 3 8, \mathbf{\frac 2 5}, \frac 3 7, \frac 1 2, \frac 4 7, \frac 3 5, \frac 5 8, \frac 2 3, \frac 5 7, \frac 3 4, \frac 4 5, \frac 5 6, \frac 6 7, \frac 7 8$$</p>
;; <p>It can be seen that $\dfrac 2 5$ is the fraction immediately to the left of $\dfrac 3 7$.</p>
;; <p>By listing the set of reduced proper fractions for $d \le 1\,000\,000$ in ascending order of size, find the numerator of the fraction immediately to the left of $\dfrac 3 7$.</p>

(ns project-euler-clj.problem-071
  (:require [project-euler-clj.common :as common]))

;; (defn find-coprimes
;;   [n]
;;   (cons 1 (filter #(not= 0 (rem n %))
;;                   (range 1 n))))

;; (defn proper-frac-list
;;   [d]
;;   (map (fn [n] (/ n d))
;;        (find-coprimes d)))

;; (proper-frac-list 8)

;; (defn get-the-nearest-lower
;;   [target candidates]
;;   (last (take-while #(> target %)
;;                     candidates)))

;; (get-the-nearest-lower (/ 3 7) (proper-frac-list 2))


(defn get-the-nearest-lower2
  [target denom]
  (let [t-denom (denominator target)
        init-numera (if (= 0 (rem denom t-denom))
                      (dec (* (numerator target)
                              (quot denom t-denom)))
                      (int (* target denom)))]
    (if-let [numera (first (filter (fn [x] (not= 0 (rem denom x)))
                                   (range init-numera 0 -1)))]
      (/ numera denom))))
(get-the-nearest-lower2 (/ 3 7) 49)

(defn solve [limit]
  (numerator
   (second
    (last
     (take limit
           (iterate (fn [[denom sol]]
                      [(inc denom)
                       (if-let [candidate (get-the-nearest-lower2 (/ 3 7)
                                                                  denom)]
                         (if (> candidate sol)
                           candidate
                           sol)
                         sol)])
                    [1 0]))))))
