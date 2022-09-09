;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?

(defn get-primes-upto
  [limit]
  (loop [primes [2]
         candidate 3]
    (if (> candidate limit)
      primes
      (recur (if (every? #(not= (mod candidate %) 0) primes)
               (conj primes candidate)
               primes)
             (+ candidate 2)))))

(do (def target 600851475143)
    (def prime-limit (int (Math/sqrt target))))

(every? #(not= (mod 7 %) 0) [2 3 5])
(get-primes-upto 10)

(defn divides?
  [a b]
  (= (mod a b) 0))
(divides? 10 4)

(defn count-exp
  [target prime & {:keys [exp] :or {exp 0}}]
  (if (divides? target prime)
    (count-exp (Math/divideExact target prime) prime :exp (inc exp))
    exp))
(count-exp 10 2)

(defn not-divisible-by?
  [target primes]
  (not (reduce #(or %1 %2) false (map #(divides? target %) primes))))
(not-divisible-by? 10 [3 4])

(defn prime-factor
  [target]
  (let [limit (int (Math/sqrt target))]
    (loop [prime-factors [{:factor 2 :exp (count-exp target 2)}]
           candidate 3]
      (if (> candidate limit)
        prime-factors
        (if (not-divisible-by? candidate (map :factor prime-factors))
          (recur (let [exp (count-exp target candidate)]
                   (if (> exp 0)
                     (conj prime-factors
                           {:factor candidate :exp exp})
                     prime-factors))
                 (+ candidate 2))
          (recur prime-factors (+ candidate 2))
          )))))
  
(not (divisible-by? 7 (map :factor [{:factor 2 :exp 0}
                                    {:factor 3 :exp 0}])))
(println (prime-factor 600851475143))
(reduce * (map #(Math/pow (:factor %) (:exp %)) [{:factor 2 :exp 3}
                                                 {:factor 2 :exp 3}]))
(reduce * (map #(Math/pow (:factor %) (:exp %)) (prime-factor 600851475143)))


