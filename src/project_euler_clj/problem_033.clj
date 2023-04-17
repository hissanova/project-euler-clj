;; Problem 33
;; The fraction 49/98 is a curious fraction, as an inexperienced mathematician in attempting to simplify it may incorrectly believe that 49/98 = 4/8, which is correct, is obtained by cancelling the 9s.

;; We shall consider fractions like, 30/50 = 3/5, to be trivial examples.

;; There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

;; If the product of these four fractions is given in its lowest common terms, find the value of the denominator.

(ns project-euler-clj.problem-033
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st])
  (:require [clojure.set :as sets])
  (:require [clojure.math.combinatorics :as combo]))


(def two-digits-nums (map (fn [x] {:num x :digits (vec (map :digit (:body (common/num-to-digits x))))})
                          (range 10 100)))


(defn remove-item-at [i coll]
  (concat (subvec coll 0 i)
          (subvec coll (inc i))))

(defn- reduce-digits
  [digits-vec-seq]
  (let [digits-maps (map set digits-vec-seq)
        intersection (apply sets/intersection digits-maps)
        union (apply sets/union digits-maps)]
    (if (or (= 0 (count intersection))
            (contains? union 0))
      nil
      (flatten (map #(remove-item-at (.indexOf % (nth (vec intersection) 0))
                                     %)
                    digits-vec-seq)))))

(def pairs-sharing-digits (filter :reduced
                                  (map (fn [[n1 n2]] {:pair [n1 n2]
                                                      :reduced (reduce-digits (map :digits [n1 n2]))})
                                       (combo/combinations two-digits-nums 2))))

(defn solve []
  (reduce * (map #(apply / (:reduced %)) (filter (fn [x] (= (apply / (map :num (:pair x)))
                                                            (apply / (:reduced x))))
                                                 pairs-sharing-digits))))
