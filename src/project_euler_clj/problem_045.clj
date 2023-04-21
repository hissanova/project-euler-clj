;; Problem 45
;; Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

;; Triangle	 	Tn=n(n+1)/2	 	1, 3, 6, 10, 15, ...
;; Pentagonal	 	Pn=n(3n−1)/2	 	1, 5, 12, 22, 35, ...
;; Hexagonal	 	Hn=n(2n−1)	 	1, 6, 15, 28, 45, ...
;; It can be verified that T285 = P165 = H143 = 40755.

;; Find the next triangle number that is also pentagonal and hexagonal.

(ns project-euler-clj.problem-045
  (:require [project-euler-clj.common :as common ])
  (:require [clojure.set :as sets])
  (:require [clojure.math.combinatorics :as combo]))


(take 10 (common/tri-seq))
(take 10 (common/penta-seq))
(take 10 (common/hexa-seq))
(defn take-le-element
  [sq n]
  (last (take-while #(>= n %)
                    sq)))

(defn solve
  []
  (take 3 (filter identity
                  (pmap #(and (= % (take-le-element (common/penta-seq) %))
                              (= % (take-le-element (common/hexa-seq) %))
                              %)
                        (common/tri-seq)))))

