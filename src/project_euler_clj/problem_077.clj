;; Problem 77

;; <p>It is possible to write ten as the sum of primes in exactly five different ways:</p>
;; \begin{align}
;; &amp;7 + 3\\
;; &amp;5 + 5\\
;; &amp;5 + 3 + 2\\
;; &amp;3 + 3 + 2 + 2\\
;; &amp;2 + 2 + 2 + 2 + 2
;; \end{align}
;; <p>What is the first value which can be written as the sum of primes in over five thousand different ways?</p>


(ns project-euler-clj.problem-077
  (:require [project-euler-clj.common :as common]))


(defn get-sum
  [n mx partition-num-map]
  (apply + (map :partitions
                (filter (fn [x]
                          (<= (:max x) mx))
                        (partition-num-map n)))))

(defn solve []
  (loop [n 3
         partition-num-map {1 [{:max 1,:partitions 0}],
                            2 [{:max 2 :partitions 1},
                               {:max 1 :partitions 0}]}]
    (let [last-partition (last (last (sort (vec partition-num-map))))]
      (if (< 5000 (apply + (map :partitions
                             (rest last-partition))))
        [(dec n) last-partition]
        (recur (inc n)
               (assoc partition-num-map
                      n
                      (concat [{:max n :partitions 1}]
                              (mapv (fn [m] {:max (- n m)
                                             :partitions (get-sum m
                                                                  (- n m)
                                                                  partition-num-map)})
                                    (filter (fn [m] (common/is-prime? (- n m)))
                                            (range 1 n))))))))))
