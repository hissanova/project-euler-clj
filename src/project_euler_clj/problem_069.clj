;; Problem 69

;; <p>Euler's totient function, $\phi(n)$ [sometimes called the phi function], is defined as the number of positive integers not exceeding $n$ which are relatively prime to $n$. For example, as $1$, $2$, $4$, $5$, $7$, and $8$, are all less than or equal to nine and relatively prime to nine, $\phi(9)=6$.</p>
;; <div class="center">
;; <table class="grid center"><tr><td><b>$n$</b></td>
;; <td><b>Relatively Prime</b></td>
;; <td><b>$\phi(n)$</b></td>
;; <td><b>$n/\phi(n)$</b></td>
;; </tr><tr><td>2</td>
;; <td>1</td>
;; <td>1</td>
;; <td>2</td>
;; </tr><tr><td>3</td>
;; <td>1,2</td>
;; <td>2</td>
;; <td>1.5</td>
;; </tr><tr><td>4</td>
;; <td>1,3</td>
;; <td>2</td>
;; <td>2</td>
;; </tr><tr><td>5</td>
;; <td>1,2,3,4</td>
;; <td>4</td>
;; <td>1.25</td>
;; </tr><tr><td>6</td>
;; <td>1,5</td>
;; <td>2</td>
;; <td>3</td>
;; </tr><tr><td>7</td>
;; <td>1,2,3,4,5,6</td>
;; <td>6</td>
;; <td>1.1666...</td>
;; </tr><tr><td>8</td>
;; <td>1,3,5,7</td>
;; <td>4</td>
;; <td>2</td>
;; </tr><tr><td>9</td>
;; <td>1,2,4,5,7,8</td>
;; <td>6</td>
;; <td>1.5</td>
;; </tr><tr><td>10</td>
;; <td>1,3,7,9</td>
;; <td>4</td>
;; <td>2.5</td>
;; </tr></table></div>
;; <p>It can be seen that $n = 6$ produces a maximum $n/\phi(n)$ for $n\leq 10$.</p>
;; <p>Find the value of $n\leq 1\,000\,000$ for which $n/\phi(n)$ is a maximum.</p>

(ns project-euler-clj.problem-069
  (:require [project-euler-clj.common :as common]))

(common/construct-from-factors (common/prime-factor 12))
(let [n 16
      p-factors (common/prime-factor n)]
  (- n
     (inc (reduce +
                  (map (comp dec common/construct-from-factors)
                       (map-indexed (fn [i fac] (assoc p-factors i (update fac :exp dec)))
                                      p-factors))))))

(defn solve []
  (apply max-key
         second
         (pmap (fn [n] [n (float (/ n (common/phi n)))])
                      (range 1 1000001))))
