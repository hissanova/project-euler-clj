;; If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.

;; If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?


;; NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20 letters. The use of "and" when writing out numbers is in compliance with British usage.

(ns project-euler-clj.problem-17
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(def num-in-words ["one" "two" "three" ])
(def to-english (partial clojure.pprint/cl-format nil 
                         "~@(~@[~R~]~^ ~A.~)"))

(map to-english (range 1 31))
(def get-count #(count (clojure.string/replace (to-english %) #"[ -]" "")))

(get-count 21)
;; correct answer
(+ (* 9 (* 99 (count "and"))) (reduce + (map get-count (range 1 1001))))
