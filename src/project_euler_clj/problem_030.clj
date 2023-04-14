;; Problem 30
;; <p>Surprisingly there are only three numbers that can be written as the sum of fourth powers of their digits:</p>
;; <blockquote>1634 = 1<sup>4</sup> + 6<sup>4</sup> + 3<sup>4</sup> + 4<sup>4</sup><br />
;; 8208 = 8<sup>4</sup> + 2<sup>4</sup> + 0<sup>4</sup> + 8<sup>4</sup><br />
;; 9474 = 9<sup>4</sup> + 4<sup>4</sup> + 7<sup>4</sup> + 4<sup>4</sup></blockquote>
;; <p class="smaller">As 1 = 1<sup>4</sup> is not a sum it is not included.</p>
;; <p>The sum of these numbers is 1634 + 8208 + 9474 = 19316.</p>
;; <p>Find the sum of all the numbers that can be written as the sum of fifth powers of their digits.</p>

(ns project-euler-clj.problem-030
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))


(defn- get-max-num-of-digits
  [power]
  (first (reverse (take-while #(> (* % (Math/pow 9 power))
                                  (Math/pow 10 (dec %)))
                              (range 1 100)))))

(defn gen-n-digits-nums
  [n]
  (range (int (Math/pow 10 (dec n)))
         (int (Math/pow 10 n))))

(defn sum-of-digits-kth-pow
  [n k]
  (reduce + (map (fn [x] (int (Math/pow (:digit x) k)))
                 (:body (common/num-to-digits n)))))

(defn solve [kth-power]
  (reduce + (filter (fn [n] (= n (sum-of-digits-kth-pow n kth-power)))
                    (flatten (map gen-n-digits-nums
                                  (range 2 (inc (get-max-num-of-digits kth-power))))))))
