;; Problem 49
;; The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways: (i) each of the three terms are prime, and, (ii) each of the 4-digit numbers are permutations of one another.

;; There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

;; What 12-digit number do you form by concatenating the three terms in this sequence?

(ns project-euler-clj.problem-049
  (:require [project-euler-clj.common :as common]))

(defn add-to-candidates-or-delete-them-all
  [n candidates]
  (if (and (common/is-prime? n))
    (if (or (empty? candidates)
            (= (set (common/num-to-digit-seq n))
               (set (common/num-to-digit-seq (last candidates)))))
      (conj candidates n)
      [])
    []))

(add-to-candidates-or-delete-them-all nil [])

(defn find-k-consec-nums
  [init-num-seq k condition]
  (loop [num-seq init-num-seq
         candidates []]
    (if (empty? num-seq)
      (if (= k (count candidates))
        candidates
        nil)
      (recur (rest num-seq)
             (condition (first num-seq)
                        candidates)))))

(find-k-consec-nums (range 1487 10000 3330)
                    3
                    add-to-candidates-or-delete-them-all)

(map (fn [diff] (filter empty
                        (map (fn [init] (find-k-consec-nums (range init 10000 diff)
                                                            3
                                                            add-to-candidates-or-delete-them-all))
                             (range 1000 (+ 1000 diff)))))
     (range 2 4501 2))


(defn diff-seq
  [num-seq]
  (let [head (first num-seq)]
    (rest (reduce (fn [acc n] (conj acc (- n (+ head (apply + acc)))))
                  []
                  num-seq))))
(diff-seq [0 3 4 6])

(defn find-arithmetic-seqs
  [num-seq]
  (let [head (first num-seq)]
    (loop [sec (second num-seq)
           diff (- sec head)
           remain (take-last (- (count num-seq) 2) num-seq)
           seqs []]
      (if (empty? remain)
        seqs
        (recur )))))

(defn solve
  []
  (map (fn [[k v]] [k (sort v)])
     (filter (fn [x] (<= 3 (count (val x))))
             (reduce (fn [acc n] (update acc
                                         (set (common/num-to-digit-seq n))
                                         #(cons n %)))
                     {}
                     (filter common/is-prime? (range 1000 10000))))))

