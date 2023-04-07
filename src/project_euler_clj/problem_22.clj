;; Problem 22
;; Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

;; For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list. So, COLIN would obtain a score of 938 × 53 = 49714.

;; What is the total of all the name scores in the file?

(ns project-euler-clj.problem-22
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(def source-file "./src/project_euler_clj/data/p022_names.txt")
(def name-list (st/split (st/replace  (slurp source-file) "\"" "") #","))

(defn convert-to-ints [capital-word]
  (map (comp #(- %1 64) int) capital-word))

(defn solve []
  (reduce + (map-indexed #(* (inc %1) %2)
                         (map #(reduce + (convert-to-ints %1))
                              (sort name-list)))))
