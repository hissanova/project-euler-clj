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

(defn get-sum
  [n mx partition-num-map]
  ;;(println "Inside get-sum: " n mx)
  (apply + (map :partitions
                (filter (fn [x]
                          (<= (:max x) mx))
                        (partition-num-map n)))))

(defn solve [div]
  (loop [n 3
         partition-num-map {1 [{:max 1,:partitions 1}],
                            2 [{:max 2 :partitions 1},
                               {:max 1 :partitions 1}]}]
    (let [last-partition (last (last (sort partition-num-map)))]
      ;;(println partition-num-map)
      (if ;;(= limit (dec n))
          (= 0 (mod (apply + (map :partitions last-partition))
                    div))
        [(dec n) last-partition]
        (recur (inc n)
               (assoc partition-num-map
                      n
                      (concat [{:max n :partitions 1}]
                              (pmap (fn [m] {:max (- n m)
                                             :partitions  (mod (get-sum m
                                                                        (- n m)
                                                                        partition-num-map)
                                                               div)})
                                    (range 1 n)))))))))

