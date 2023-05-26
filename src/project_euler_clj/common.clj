(ns project-euler-clj.common)

(defn pow
  [base p]
  (reduce * (repeat p base)))

(defmacro lazy-seq-gen
  [init-term init-d d]
  (let [func (gensym 'func)]
    `(defn ~func
       ([] (~func ~init-term ~init-d))
       ([n# diff#] (cons n# (lazy-seq (~func (+ n# diff#)
                                       (+ diff# ~d))))))))

(def tri-seq (lazy-seq-gen 1 2 1))
(def square-seq (lazy-seq-gen 1 3 2))
(def penta-seq (lazy-seq-gen 1 4 3))
(def hexa-seq (lazy-seq-gen 1 5 4))

(defn gen-n-digits-nums
  [n]
  (range (int (Math/pow 10 (dec n)))
         (int (Math/pow 10 n))))

(defn- add-new-digit
  [rep new-digit]
  (update rep :body #(conj % new-digit)))

(defn logn
  [n]
  (if (>= 0 n)
    (throw (Exception. (format "No support for the base %d" n)))
      (if (= 10 n)
        (fn [x] (Math/log10 x))
        (fn [x] (/ (Math/log x)
                   (Math/log n))))))

;; There's a problem in handling big integers!!
(defn num-to-digits
  ([n] (num-to-digits n 10))
  ([n base]
   (loop [current-num n
          digit-rep {:base base :body []}
          max-exp (if (= 0 current-num)
                    current-num
                    (bigint ((logn base) current-num)))]
     (if (= 0 max-exp)
       (add-new-digit digit-rep {:digit current-num :pow 0})
       (let [current-power (bigint (pow base max-exp))]
         (recur
          (mod current-num current-power)
          (add-new-digit digit-rep {:digit (quot current-num current-power)
                                    :pow max-exp})
          (dec max-exp)))))))

(defn num-to-digit-seq
  ([n] (map #(Integer/parseInt (str %))
            (str n)))
  ([n base]
   (map :digit (:body (num-to-digits n base)))))

(defn digits-to-number
  [digits-rep]
  (reduce + (map #(bigint (* (% :digit)
                          (Math/pow (digits-rep :base)
                                    (% :pow))))
                 (digits-rep :body))))

(defn digit-seq-to-num
  [digits-seq]
  (reduce + (map (fn [[p d]] (* d (bigint (Math/pow 10 p))))
                 (map-indexed vector (reverse digits-seq)))))

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

(def memo-primes-upto (memoize get-primes-upto))

(defn get-prime-table
  []
  (slurp "prime_table.txt"))

(defn get-primes-upto!
  [limit]
  (let [prime-table (get-prime-table)
        candidate (inc (last prime-table))]
    (loop [primes prime-table
           candidate 3]
      (if (> candidate limit)
        primes
        (recur (if (every? #(not= (mod candidate %) 0) (filter #(<= % (Math/sqrt candidate)) primes))
                 (conj primes candidate)
                 primes)
               (+ candidate 2))))))


(def parseInt #(Integer/parseInt %))

(defn divides?
  [a b]
  (= (mod a b) 0))

(defn count-exp
  [target prime & {:keys [exp] :or {exp 0}}]
  (if (<= target 0)
    (throw (Exception. (format "Invalid value: %d" target))))
  (if (divides? target prime)
    (count-exp (quot (bigint target) (bigint prime)) prime :exp (inc exp))
    exp))

(defn not-divisible-by?
  [target primes]
  (not (reduce #(or %1 %2) false (map #(divides? target %) primes))))

(defn construct-from-factors
  [factors]
  (bigint (reduce * (map #(Math/pow (% :factor) (% :exp)) factors))))


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
    (map #(int (Math/pow (fac-exp :factor) %))
         (range (inc (fac-exp :exp))))))

(def memo-prime-factor (memoize prime-factor))

(defn is-prime?
  [n]
  (let [prime-fac (memo-prime-factor n)]
    (and (= 1 (count prime-fac))
         (= 1 ((nth prime-fac 0) :exp)))))

(defn get-proper-divisors [n]
  (filter (fn [x] (> n x)) (map #(reduce * %) (product (get-powers-of-prime-factors n)))))

(defn sum-proper-divisors [n]
  (reduce + (get-proper-divisors n)))

(defn is-amicable? [n]
  (let [sum (sum-proper-divisors n)]
    (if (= n (sum-proper-divisors sum))
      (list n sum))))

(defn is-abundant-number? [n]
  (< n (sum-proper-divisors n)))

(defn reverse-n
  [n]
  (digit-seq-to-num (reverse (num-to-digit-seq n))))

(defn palindrome?
  [n]
  (= n (reverse-n n)))

