(ns project-euler-clj.common)

(defn product
  [xs]
  (if (= (count xs) 1)
    (map vector (first xs))
    (apply concat (map (fn [e] (map (fn [y] (concat [e] y))
                                    (product (rest xs))))
                       (first xs)))))

(defn factorial
  ([n]
   (if (< n 0)
     (throw (Exception. (format "Invalid argument %d" n))))
   (factorial n 1))
  ([n prod]
   (if (or (= n 0) (= n 1))
     prod
     (recur (dec n) (* n prod)))))

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

(def parseInt #(Integer/parseInt %))

(defn divides?
  [a b]
  (= (mod a b) 0))

(defn count-exp
  [target prime & {:keys [exp] :or {exp 0}}]
  (if (<= target 0)
    (throw (Exception. (format "Invalid value: %d" target))))
  (if (divides? target prime)
    (count-exp (Math/divideExact (int target) (int prime)) prime :exp (inc exp))
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
    (loop [prime-factors (let [exp (count-exp target 2N)]
                             (if (> exp 0)
                               [{:factor 2N :exp exp}]
                               []))
           candidate 3N]
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
                 (+ candidate 2N))
          (recur prime-factors (+ candidate 2N)))))))

