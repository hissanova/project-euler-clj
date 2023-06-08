;; Problem 68
;; <p>Consider the following "magic" 3-gon ring, filled with the numbers 1 to 6, and each line adding to nine.</p>
;; <div class="center">
;; <img src="resources/images/0068_1.png?1678992052" class="dark_img" alt=""><br></div>
;; <p>Working <b>clockwise</b>, and starting from the group of three with the numerically lowest external node (4,3,2 in this example), each solution can be described uniquely. For example, the above solution can be described by the set: 4,3,2; 6,2,1; 5,1,3.</p>
;; <p>It is possible to complete the ring with four different totals: 9, 10, 11, and 12. There are eight solutions in total.</p>
;; <div class="center">
;; <table width="400" cellspacing="0" cellpadding="0"><tr><td width="100"><b>Total</b></td><td width="300"><b>Solution Set</b></td>
;; </tr><tr><td>9</td><td>4,2,3; 5,3,1; 6,1,2</td>
;; </tr><tr><td>9</td><td>4,3,2; 6,2,1; 5,1,3</td>
;; </tr><tr><td>10</td><td>2,3,5; 4,5,1; 6,1,3</td>
;; </tr><tr><td>10</td><td>2,5,3; 6,3,1; 4,1,5</td>
;; </tr><tr><td>11</td><td>1,4,6; 3,6,2; 5,2,4</td>
;; </tr><tr><td>11</td><td>1,6,4; 5,4,2; 3,2,6</td>
;; </tr><tr><td>12</td><td>1,5,6; 2,6,4; 3,4,5</td>
;; </tr><tr><td>12</td><td>1,6,5; 3,5,4; 2,4,6</td>
;; </tr></table></div>
;; <p>By concatenating each group it is possible to form 9-digit strings; the maximum string for a 3-gon ring is 432621513.</p>
;; <p>Using the numbers 1 to 10, and depending on arrangements, it is possible to form 16- and 17-digit strings. What is the maximum <b>16-digit</b> string for a "magic" 5-gon ring?</p>
;; <div class="center">
;; <img src="resources/images/0068_2.png?1678992052" class="dark_img" alt=""><br></div>

(ns project-euler-clj.problem-068
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st])
  (:require [clojure.math.combinatorics :as combo]))


(common/factorial 10)

(defn permute-seq
  [sq]
  (concat (rest sq)
          [(first sq)]))

(permute-seq [1 2 3])

(defn min-first
  [c]
  (let [outer-min (apply min-key first c)
        min-pos (.indexOf c outer-min)]
        (nth (iterate permute-seq c) min-pos)))

(defn- concat-nextline
  [chains candidates]
  (filter empty
         (for [chain chains
               next-cand candidates]
           (let [tail (last chain)]
             (if (and (= 1 (count (clojure.set/intersection (set tail)
                                                            (set next-cand))))
                      (= (last tail)
                         (second next-cand)))
               (conj chain next-cand))))))

(defn gen-n-gons
  [n sum]
  (let [nums (set (range 1 (inc (* 2 n))))
        lines (filter #(= sum (apply + %))
                (map vec
                     (combo/permuted-combinations nums 3)))]
    (set (map min-first
              (apply concat
                     (for [l lines]
                       (let [others (filter #(not= l %) lines)]
                         (loop [k-th 1
                                chains [[l]] 
                                remain others]
                           (if (= k-th n)
                             (filter (fn [c] (and (= nums (set (flatten c)))
                                                  (= n (count (set (map first c))))
                                                  (= (last (last c))
                                                     (second (first c)))))
                                     chains)
                             (recur (inc k-th)
                                    (concat-nextline chains remain)
                                    (rest remain)))))))))))

(last (sort (map #((comp common/digit-seq-to-num flatten) %) (gen-n-gons 3 9))))

(defn take-only-non-nil
  [lazy-sq]
  (take-while not-empty
              (drop (count (take-while empty? lazy-sq))
                    lazy-sq)))

(take-only-non-nil (map #(if (and (< 3 %) (< % 10)) [%]) (range)))

(defn solve []
  (last
   (sort
    (map (fn [s] (common/digit-seq-to-num s))
         (filter #(= 16 (count %))
                 (map (fn [c] (flatten (map common/num-to-digit-seq
                                            (flatten c))))
                      (apply concat
                             (take-only-non-nil
                              (map (fn [sum] (vec (gen-n-gons 5 sum)))
                                   (range))))))))))
