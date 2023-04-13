;; Problem 31
;; In the United Kingdom the currency is made up of pound (£) and pence (p). There are eight coins in general circulation:

;; 1p, 2p, 5p, 10p, 20p, 50p, £1 (100p), and £2 (200p).
;; It is possible to make £2 in the following way:

;; 1×£1 + 1×50p + 2×20p + 1×5p + 1×2p + 3×1p
;; How many different ways can £2 be made using any number of coins?

(ns project-euler-clj.problem-031
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(def rev-range (fn [n] (reverse (range n))))

(defn get-max-coins
  [total-amount coin-value]
  (int (/ total-amount coin-value)))

(def coin-values [200 100 50 20 10 5 2 1])

(defn- enumerate-possible-configs-recur
  [total-amount coins coin-values]
  (if (= 1 (count coin-values))
    (let [coin-val (last coin-values)
          max-coins (get-max-coins total-amount coin-val)]
      (conj coins
            {coin-val max-coins}
            {:remain (mod total-amount coin-val)}))
    (let [current-coin (first coin-values)
          values (reverse (map-indexed vector (range 0 (inc total-amount) current-coin)))]
      (for [[coin-num current-value] values]
        (let [coin-nums  (conj coins {current-coin coin-num})]
          (if (= total-amount current-value)
            (conj coin-nums {:remain 0})
            (enumerate-possible-configs-recur (- total-amount current-value)
                                        coin-nums
                                        (rest coin-values))))))))

(defn enumerate-possible-configs
  [total-amount coins coin-values]
  (flatten (enumerate-possible-configs-recur total-amount coins coin-values)))

(defn solve
  []
  (count (enumerate-possible-configs 200 {} coin-values)))
