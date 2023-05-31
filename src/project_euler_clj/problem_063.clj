;; Problem 63
;; <p>The $5$-digit number, $16807=7^5$, is also a fifth power. Similarly, the $9$-digit number, $134217728=8^9$, is a ninth power.</p>
;; <p>How many $n$-digit positive integers exist which are also an $n$th power?</p>

(ns project-euler-clj.problem-063
  (:require [project-euler-clj.common :as common]))

(defn solve
  []
  (count (apply concat
                (map (fn [[base s]] (filter #(= base (second %)) s))
                     (take-while (fn [[num sq]] (contains? (set (map second sq)) num))
                                 (map (fn [x] [x
                                               (vec (map (fn [n] (let [p (common/pow (bigint n) x)]
                                                                   [p
                                                                    (count (common/num-to-digit-seq p))]))
                                                         (range 1 10)))])
                                      (drop 1 (range))))))))
