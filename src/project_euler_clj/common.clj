(ns project-euler-clj.common)

(defn product
  [xs]
  (if (= (count xs) 1)
    (map vector (first xs))
    (apply concat (map (fn [e] (map (fn [y] (concat [e] y))
                                    (product (rest xs))))
                       (first xs)))))

(defn get-exceeds?-func
  [sup]
  (fn [n] (and sup (>= n sup))))

(defn lazy-fibonacci-seq
  ([] (lazy-fibonacci-seq [1N 1N]))
  ([[n-2 n-1]] (cons n-2 (lazy-seq (lazy-fibonacci-seq [n-1 (+ n-2 n-1)])))))

(take 5 (lazy-fibonacci-seq))

(defn fibonacci-seq
  ""
  [& {limit :limit upto :upto-nth
        :or {limit nil upto nil}}]
  (let [exceeds-limit? (get-exceeds?-func limit)
        exceeds-nth? (get-exceeds?-func upto)]
    (loop [fibo-seq [1 1]
           new-term (reduce + (take-last 2 fibo-seq))]
      (if (or (exceeds-limit? new-term)
              (exceeds-nth? (count fibo-seq)))
        fibo-seq
        (recur (conj fibo-seq new-term)
               (+ (last fibo-seq) new-term))))))

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

(defn get-powers-of-prime-factors [n]
  (for [fac-exp (prime-factor n)]
    (map #(int (Math/pow (:factor fac-exp) %1))
         (range (inc (:exp fac-exp))))))

(defn get-proper-divisors [n]
  (filter (fn [x] (> n x)) (map #(reduce * %1) (product (get-powers-of-prime-factors n)))))

(defn sum-proper-divisors [n]
  (reduce + (get-proper-divisors n)))

(defn is-amicable? [n]
  (let [sum (sum-proper-divisors n)]
    (if (= n (sum-proper-divisors sum))
      (list n sum))))

(defn is-abundant-number? [n]
  (< n (sum-proper-divisors n)))
