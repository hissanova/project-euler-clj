(ns project-euler-clj.common)
  
(defn get-primes-upto
  [limit]
  (loop [primes [2]
         candidate 3]
    (if (> candidate limit)
      primes
      (recur (if (every? #(not= (mod candidate %) 0) (filter #(<= % (Math/sqrt candidate)) primes))
               (conj primes candidate)
               primes)
             (+ candidate 2)))))

(defn divides?
  [a b]
  (= (mod a b) 0))

(defn count-exp
  [target prime & {:keys [exp] :or {exp 0}}]
  (if (<= target 0)
    (throw (Exception. (format "Invalid value: %d" target))))
  (if (divides? target prime)
    (count-exp (Math/divideExact target prime) prime :exp (inc exp))
    exp))

(defn not-divisible-by?
  [target primes]
  (not (reduce #(or %1 %2) false (map #(divides? target %) primes))))

(defn construct-from-factors
  [factors]
  (bigint (reduce * (map #(Math/pow (:factor %) (:exp %)) factors))))


(defn prime-factor
  [target]
  (if (>= 0 target)
    (throw (Exception. (format "prime factors cannot be evaluated for %d" target))))
  (let [limit (bigint (Math/sqrt target))]
    (loop [prime-factors (let [exp (count-exp target 2)]
                             (if (> exp 0)
                               [{:factor 2 :exp exp}]
                               []))
           candidate 3]
      (if (> candidate limit)
        (let [result (construct-from-factors prime-factors)]
          (if (= target result)
            prime-factors
            (conj prime-factors {:factor (/ target result) :exp 1})))
        (if (not-divisible-by? candidate (map :factor prime-factors))
          (recur (let [exp (count-exp target candidate)]
                   (if (> exp 0)
                     (conj prime-factors
                           {:factor candidate :exp exp})
                     prime-factors))
                 (+ candidate 2))
          (recur prime-factors (+ candidate 2)))))))

