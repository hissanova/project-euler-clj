;; Problem 48
;; <p>The series, 1<sup>1</sup> + 2<sup>2</sup> + 3<sup>3</sup> + ... + 10<sup>10</sup> = 10405071317.</p>
;; <p>Find the last ten digits of the series, 1<sup>1</sup> + 2<sup>2</sup> + 3<sup>3</sup> + ... + 1000<sup>1000</sup>.</p>

(ns project-euler-clj.problem-048
  (:require [project-euler-clj.common :as common]))

(defn multiply-mod
  [a b m]
  (rem (* a b) m))

(defn pow-mod
  [a b m]
  (last (take b (iterate #(multiply-mod % a m) a))))

(pow-mod 99 99  (bigint 1e10))

(defn solve
  []
  (rem (reduce + (map #(pow-mod % % (bigint 1e10)) (range 1 1001)))
       (bigint 1e10)))

