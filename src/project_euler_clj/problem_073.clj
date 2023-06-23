;; Problem 73

;; <p>Consider the fraction, $\dfrac n d$, where $n$ and $d$ are positive integers. If $n \lt d$ and $\operatorname{HCF}(n, d)=1$, it is called a reduced proper fraction.</p>
;; <p>If we list the set of reduced proper fractions for $d \le 8$ in ascending order of size, we get:
;; $$\frac 1 8, \frac 1 7, \frac 1 6, \frac 1 5, \frac 1 4, \frac 2 7, \frac 1 3, \mathbf{\frac 3 8, \frac 2 5, \frac 3 7}, \frac 1 2, \frac 4 7, \frac 3 5, \frac 5 8, \frac 2 3, \frac 5 7, \frac 3 4, \frac 4 5, \frac 5 6, \frac 6 7, \frac 7 8$$</p>
;; <p>It can be seen that there are $3$ fractions between $\dfrac 1 3$ and $\dfrac 1 2$.</p>
;; <p>How many fractions lie between $\dfrac 1 3$ and $\dfrac 1 2$ in the sorted set of reduced proper fractions for $d \le 12\,000$?</p>



(ns project-euler-clj.problem-073
  (:require [project-euler-clj.common :as common]))

(defn reduced-fracs
  ([d] (reduced-fracs d (range 1 d)))
  ([d numerators]
   (map last
        (filter (fn [[n d f]] (= [n d]
                                 [(numerator f) (denominator f)]))
                (map (fn [n] [n d (/ n d)])
                     numerators)))))

(let [d 1000000]
  (reduced-fracs d (range (inc (quot d 3))
                          (inc (quot d 2)))))

(defn solve [limit]
  (count (flatten
          (filter not-empty
                  (map (fn [d] (reduced-fracs d (range (inc (quot d 3))
                                                       (inc (quot d 2)))))
                       (range 3 limit))))))
