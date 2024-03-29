;; Problem 70

;; <p>Euler's totient function, $\phi(n)$ [sometimes called the phi function], is used to determine the number of positive numbers less than or equal to $n$ which are relatively prime to $n$. For example, as $1, 2, 4, 5, 7$, and $8$, are all less than nine and relatively prime to nine, $\phi(9)=6$.<br>The number $1$ is considered to be relatively prime to every positive number, so $\phi(1)=1$. </p>
;; <p>Interestingly, $\phi(87109)=79180$, and it can be seen that $87109$ is a permutation of $79180$.</p>
;; <p>Find the value of $n$, $1 \lt n \lt 10^7$, for which $\phi(n)$ is a permutation of $n$ and the ratio $n/\phi(n)$ produces a minimum.</p>

(ns project-euler-clj.problem-070
  (:require [project-euler-clj.common :as common]))

(defn solve []
  (sort-by #(/ (first %) (second %))
           (filter (fn [sq] (apply = (map (comp sort common/num-to-digit-seq) sq)))
                   (pmap (fn [n] [n (common/phi n)])
                        (range 2 (int 1e7))))))
