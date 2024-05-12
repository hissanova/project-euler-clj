;; Problem 78
;; <p>Let $p(n)$ represent the number of different ways in which $n$ coins can be separated into piles. For example, five coins can be separated into piles in exactly seven different ways, so $p(5)=7$.</p>
;; <div class="margin_left">
;; OOOOO<br>
;; OOOO   O<br>
;; OOO   OO<br>
;; OOO   O   O<br>
;; OO   OO   O<br>
;; OO   O   O   O<br>
;; O   O   O   O   O
;; </div>
;; <p>Find the least value of $n$ for which $p(n)$ is divisible by one million.</p>

(ns project-euler-clj.problem-078
  (:require [project-euler-clj.common :as common]))

;; (defn get-sum
;;   [n mx partition-num-map]
;;   ;;(println "Inside get-sum: " n mx)
;;   (if (= max 1)
;;     1
;;     (apply + (map :partitions
;;                   (filter (fn [x]
;;                                 (<= (:max x) mx))
;;                           (partition-num-map n))))))

(defn get-sum
  [n mx partition-num-map]
  ;;(println "Inside get-sum: " n mx partition-num-map)
  (if (< n 1)
    1
    (if (< n mx)
      (get-in partition-num-map [n n :total])
      (get-in partition-num-map [n mx :total]))))


(defn solve [div]
  (loop [n 3
         partition-num-map {1 {1 {:partitions 1 :total 1}},
                            2 {2 {:partitions 1 :total 2},
                               1 {:partitions 1 :total 1}}}]
    (let [last-partition (last (last (sort partition-num-map)))]
      (if (= 0 (mod (dec n) 100))
        (println (format "Now calculating %d" (dec n))))
      ;;(println "Loop:" n partition-num-map last-partition (get-in last-partition [(dec n) :total]))
      (if ;;(= div (dec n))
          (= 0 (mod (get-in last-partition [(dec n) :total])
                    div))
        [(dec n) last-partition]
        (recur (inc n)
               (assoc partition-num-map
                      n
                      (reduce (fn [mp mx] (let [remain (- n mx)
                                               par (get-sum remain
                                                            mx
                                                            partition-num-map)]
                                           ;;(println n mx mp par)
                                           (assoc mp
                                                  mx
                                                  {:partitions  (mod par div)
                                                   :total (mod (+ par (get-in mp [(dec mx) :total] 0))
                                                               div)})))
                              {1 {:partitions 1 :total 1}}
                              (range 1 (inc n)))))))))

;; div=100000->11224.
