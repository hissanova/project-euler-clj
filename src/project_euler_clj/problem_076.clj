;; Problem 76
;; <p>It is possible to write five as a sum in exactly six different ways:</p>
;; \begin{align}
;; &amp;4 + 1\\
;; &amp;3 + 2\\
;; &amp;3 + 1 + 1\\
;; &amp;2 + 2 + 1\\
;; &amp;2 + 1 + 1 + 1\\
;; &amp;1 + 1 + 1 + 1 + 1
;; \end{align}
;; <p>How many different ways can one hundred be written as a sum of at least two positive integers?</p>

(ns project-euler-clj.problem-076
  (:require [project-euler-clj.common :as common]))

(defn- partition-n-sub
  [n]
  (if (= n 1)
    [1]
    (apply concat
           (map (fn [m]  (let [n_ (- n m)]
                           (cons [n_ m]
                                 (if (not= m 1)
                                   (map (fn [k] (vec
                                                 (reverse
                                                  (sort
                                                   (cons n_ k)))))
                                          (partition-n-sub m))))))
                (range 1 n)))))
(count (partition-n-sub 5))

(defn memo-partition-n
  [n]
  ((memoize partition-n-sub) n))
(memo-partition-n 5)
(defn partition-n
  [n]
  ((comp vec set) (map (comp vec reverse sort)
                       (memo-partition-n n))))

(count (partition-n 5))

(defn convert-to-mat
  [partition-sq size]
  (let [sq (concat partition-sq (repeat (- size (count partition-sq))
                                        0))]
    (map (fn [n] (concat (repeat n 1)
                         (repeat (- size n) 0)))
         sq)))


(defn transpose-mat
  [mat]
  (apply map vector mat))

(transpose-mat (convert-to-mat [5 1] 6))

(defn transpose
  [partition-sq]
  (filter (fn [n] (> n 0))
          (map (fn [row] (reduce + row))
               (transpose-mat
                (convert-to-mat partition-sq
                                (reduce + partition-sq))))))

(transpose [5 1])

(defn partition-n2
  [n]
  (if (= n 1)
    [[1]]
    (let [left-columns (apply concat
                              (map (fn [m] (let [n_ (- n m)]
                                             (if (= 0 n_)
                                               [[n]]
                                               (map (fn [k] 
                                                      (vec (cons m k)))
                                                    (partition-n2 n_)))))
                                   (range n (int (Math/sqrt n)) -1)))]
      (set (concat left-columns
                   (mapv transpose left-columns))))))

(count (partition-n2 3))

(defn get-sum
  [n mx partition-num-map]
  (apply + (map :partitions
                (filter (fn [x]
                          (<= (:max x) mx))
                        (partition-num-map n)))))

(get-sum 3 2 {1 [{:max 1,:partitions 1}],
              2 [{:max 2 :partitions 1},
                 {:max 1 :partitions 1}],
              3 [{:max 3 :partitions 1},
                 {:max 2 :partitions 1},
                 {:max 1 :partitions 1}]})

(defn solve [target]
  (let [part (get (loop [n 3
                         partition-num-map {1 [{:max 1,:partitions 1}],
                                            2 [{:max 2 :partitions 1},
                                               {:max 1 :partitions 1}]}]
                    (if (= n (inc target))
                      partition-num-map
                      (recur (inc n)
                             (assoc partition-num-map
                                    n
                                    (concat [{:max n :partitions 1}]
                                            (mapv (fn [m] {:max (- n m)
                                                           :partitions (get-sum m
                                                                                (- n m)
                                                                              partition-num-map)})
                                                  (range 1 n)))))))
                  target)]
    (apply + (map :partitions part))))
