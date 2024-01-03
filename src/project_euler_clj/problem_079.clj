;; Problem 79

(ns project-euler-clj.problem-079
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(def triples (map (fn [s] (map identity s))
                  (st/split-lines (slurp "src/project_euler_clj/data/0079_keylog.txt"))))
(def total-nums (set (flatten triples)))
(def order (set (concat (map #(take 2 %) triples) (map #(take-last 2 %) triples))))
;;(def maximal-num  (clojure.set/difference total-nums (set (map first order))))
;;(def least-num  (clojure.set/difference total-nums (set (map last order))))
(defn solve []
  (loop [current-order order
         candidates total-nums
         code []]
    ;;(println current-order candidates code)
    (let [least-num  (last (clojure.set/difference candidates (set (map last current-order))))
          remain (clojure.set/difference candidates #{least-num})]
      (if (empty? candidates)
        (st/join code)
        (recur
         (set (filter #(not= least-num (first %)) current-order))
         remain
         (conj code least-num))))))

