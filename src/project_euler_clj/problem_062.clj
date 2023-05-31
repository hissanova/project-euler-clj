;; Problem 62
;; <p>The cube, $41063625$ ($345^3$), can be permuted to produce two other cubes: $56623104$ ($384^3$) and $66430125$ ($405^3$). In fact, $41063625$ is the smallest cube which has exactly three permutations of its digits which are also cube.</p>
;; <p>Find the smallest cube for which exactly five permutations of its digits are cube.</p>

(ns project-euler-clj.problem-062
  (:require [project-euler-clj.common :as common])
  (:require [clojure.math.combinatorics :as combo]))

(defn solve
  []
  (let [seq-gen (map #(common/pow % 3) (range))]
    (loop [term (first seq-gen)
           remain (rest seq-gen)
           digit-map {}]
      (if-let [ans (seq (filter #(= 5 (count (val %))) digit-map))]
        ans
        (recur (first remain)
               (rest remain)
               (update digit-map
                       (sort (common/num-to-digit-seq term))
                       #(vec (conj % term))))))))
