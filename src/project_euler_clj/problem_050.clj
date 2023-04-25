;; Problem 50
;; The prime 41, can be written as the sum of six consecutive primes:

;; 41 = 2 + 3 + 5 + 7 + 11 + 13
;; This is the longest sum of consecutive primes that adds to a prime below one-hundred.

;; The longest sum of consecutive primes below one-thousand that adds to a prime, contains 21 terms, and is equal to 953.

;; Which prime, below one-million, can be written as the sum of the most consecutive primes?


(ns project-euler-clj.problem-050
  (:require [project-euler-clj.common :as common]))


(defn slide-window
  [w-size sq]
  (loop [windows []
         remain sq]
    (if (empty? remain)
      (filter #(= w-size (count %))
              windows)
      (let [term (first remain)]
        (recur (conj (vec (map (fn [w] (if (not= w-size (count w))
                                         (conj w term)
                                         w))
                               windows))
                     [term])
               (rest remain))))))

(slide-window 3 (range 10))
(partition 3 (range 10))

(rem (count (common/get-primes-upto (int 1e3))) 21)

(def memo-primes-upto (memoize common/get-primes-upto))

(defn accumulate
  [num-seq]
  (loop [acc [(first num-seq)]
         remain (rest num-seq)]
    (if (empty? remain)
      acc
      (recur (conj acc (+ (last acc) (first remain)))
             (rest remain)))))

(defn max-window-length
  [limit num-seq]
  (count (take-while #(< % limit) (accumulate num-seq))))


(defn solve
  "This takes ages. Don't use this for big limit numbers."
  [min-window limit]
  (let [primes (memo-primes-upto limit)]
    (apply concat
           (filter not-empty
                   (pmap (fn [w-size] (filter identity
                                              (pmap (fn [sq] (if (or (odd? w-size)
                                                                     (contains? (set sq) 2))
                                                               (let [sum (apply + sq)]
                                                                 (if (and (< sum limit)
                                                                          (common/is-prime? sum))
                                                                   [sq sum]))))
                                                    (slide-window w-size primes))))
                         (range min-window (inc (max-window-length limit primes))))))))

(time (do (count (nth (last (solve 81 30000))
                      0))))
(defn- get-new-longest
  [longest-seq longest-length primes-so-far new-prime]
  (let [candidates (map (fn [len] (conj (vec (take-last len primes-so-far))
                                        new-prime))
                        (reverse (range longest-length
                                        (inc (count primes-so-far)))))
        new-longest-seq (first (filter identity (map  (fn [sq] (let [sum' (apply + sq)]
                                                                 (if (common/is-prime? sum')
                                                                   [sq sum'])))
                                                      candidates)))]
    (if (and (not-empty new-longest-seq)
             (<= longest-length
                 (count (first new-longest-seq))))
      [(conj longest-seq new-longest-seq) (count (first new-longest-seq))]
      [longest-seq longest-length])))

(defn solve2 [limit]
  (take-last 2 (let [primes (filter common/is-prime? (drop 2 (range)))]
                 (loop [prime (first primes)
                        remain (rest primes)
                        primes-so-far []
                        longest-seq []
                        longest-length 0]
                   (if (and (not-empty longest-seq)
                            (< limit (last (last longest-seq))))
                     longest-seq
                     (let [[new-longest-seq new-longest-length] (get-new-longest longest-seq
                                                                                 longest-length
                                                                                 primes-so-far
                                                                                 prime)]
                       (recur (first remain)
                              (rest remain)
                              (conj primes-so-far prime)
                              new-longest-seq
                              new-longest-length)))))))

