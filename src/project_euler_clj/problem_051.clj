;; Problem 51
;; <p>By replacing the 1<sup>st</sup> digit of the 2-digit number *3, it turns out that six of the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.</p>
;; <p>By replacing the 3<sup>rd</sup> and 4<sup>th</sup> digits of 56**3 with the same digit, this 5-digit number is the first example having seven primes among the ten generated numbers, yielding the family: 56003, 56113, 56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first member of this family, is the smallest prime with this property.</p>
;; <p>Find the smallest prime which, by replacing part of the number (not necessarily adjacent digits) with the same digit, is part of an eight prime value family.</p>


(ns project-euler-clj.problem-051
  (:require [project-euler-clj.common :as common])
  (:require [clojure.math.combinatorics :as combo]))

(defn- get-dig-seqs
  [pos-sel total-digits]
  (map (fn [digits] (replace (assoc (into {} (map vector pos-sel digits))
                                    (dec total-digits) (last digits))
                             (vec (range total-digits))))
       (common/product (concat (map (fn [pos] (if (= 0 pos)
                                                (range 1 10)
                                                (range 10)))
                                    pos-sel)
                               [(list 1 3 7 9)]))))
(get-dig-seqs [0 2] 4)

(defn replace-at
  [coll positions v]
  (reduce (fn [c pos]  (assoc c pos v))
              coll
              positions))
(replace-at [0 1 1] [1] :x)

(defn- partition-of-digits-except-last
  [dig-num]
  (apply concat
         (map (fn [x] [x (reverse x)])
              (combo/partitions (range (dec dig-num)) :min 2 :max 2))))

(defn gen-candidates
  [dig-num]
  (apply concat
         (map (fn [[var-digs replace-pos]] (map (fn [seq] (replace-at seq replace-pos :x))
                                                  (get-dig-seqs var-digs dig-num)))
              (partition-of-digits-except-last dig-num))))

(take 50 (gen-candidates 4))


(defn solve [dig-num min-len]
  (filter (fn [s] (<= min-len (count s)))
          (map (fn [num-seq] (filter (comp common/is-prime? common/digit-seq-to-num)
                                     (map #(replace {:x %} num-seq)
                                          (if (= :x (first num-seq))
                                            (range 1 10)
                                            (range 10)))))
               (gen-candidates dig-num))))

;; This yields the answer.
(solve 6 8)
