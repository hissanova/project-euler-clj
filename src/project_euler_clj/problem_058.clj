;; Problem 58
;; <p>Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.</p>
;; <p class="center monospace"><span class="red"><b>37</b></span> 36 35 34 33 32 <span class="red"><b>31</b></span><br>
;; 38 <span class="red"><b>17</b></span> 16 15 14 <span class="red"><b>13</b></span> 30<br>
;; 39 18 <span class="red"> <b>5</b></span>  4 <span class="red"> <b>3</b></span> 12 29<br>
;; 40 19  6  1  2 11 28<br>
;; 41 20 <span class="red"> <b>7</b></span>  8  9 10 27<br>
;; 42 21 22 23 24 25 26<br><span class="red"><b>43</b></span> 44 45 46 47 48 49</p>
;; <p>It is interesting to note that the odd squares lie along the bottom right diagonal, but what is more interesting is that 8 out of the 13 numbers lying along both diagonals are prime; that is, a ratio of 8/13 ≈ 62%.</p>
;; <p>If one complete new layer is wrapped around the spiral above, a square spiral with side length 9 will be formed. If this process is continued, what is the side length of the square spiral for which the ratio of primes along both diagonals first falls below 10%?</p>
(ns project-euler-clj.problem-058
  (:require [project-euler-clj.common :as common]))

(take 100 (cons '(1) (map (fn [n] )
                          (drop 1 ))))

(defn get-corners
  [n]
  (map #(+ (common/pow (- n 2) 2)
           (* % (dec n)))
       (range 1 5)))

(defn solve
  []
  (let [odd-gen (filter odd? (range))]
    (loop [n (first odd-gen)
           corners 1 
           prime-corners 0
           remain (rest odd-gen)]
      (if (or (and (< 1 n)
                   (> 0.1 (/ prime-corners corners))))
        n
        (let [n-next (first remain)
              nex-corners (get-corners n-next)]
          (recur 
           n-next
           (+ corners 4)
           (+ prime-corners (count (filter common/is-prime? nex-corners)))
           (rest remain)))))))
