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
