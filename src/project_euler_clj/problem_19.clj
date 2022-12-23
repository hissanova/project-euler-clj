;; Problem 19
;; You are given the following information, but you may prefer to do some research for yourself.

;; 1 Jan 1900 was a Monday.
;; Thirty days has September,
;; April, June and November.
;; All the rest have thirty-one,
;; Saving February alone,
;; Which has twenty-eight, rain or shine.
;; And on leap years, twenty-nine.
;; A leap year occurs on any year evenly divisible by 4, but not on a century unless it is divisible by 400.
;; How many Sundays fell on the first of the month during the twentieth century (1 Jan 1901 to 31 Dec 2000)?

(ns project-euler-clj.problem-19
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(defn xor
  [a b]
  (or (and (not a) b)
      (and a (not b))))

(defn leap-year?
  [year]
  (xor (common/divides? year 4)
       (xor (common/divides? year 100)
            (common/divides? year 400))))

(defn get-days-in-month
  [year month]
  (cond
    (= month 2) (if (leap-year? year)
                  29
                  28)
    (#{4 6 9 11} month) 30
    :else 31))

(defn get-days-in-year
  [year]
  (if (leap-year? year)
    366 365))

(defn get-total-days
  [year]
  (reduce (fn [acc y] (+ acc (get-days-in-year y)))
          0
          (range year)))

(defn get-past-days-in-year
  [year month]
  (reduce (fn [acc m] (+ acc (get-days-in-month year m)))
          0
          (range 1 month)))

(defn absolute-days
  [year month day]
  (let [days-in-month (get-days-in-month year month)]
    (when (or (< day 1) (< days-in-month day))
      (throw (Exception. (format "In %d/%d, there was only %d" year month days-in-month))))
    (+ (get-total-days year)
       (get-past-days-in-year year month)
       day)))

(defn get-weekday
  [date]
  (case (mod (- (absolute-days (:year date) (:month date) (:day date))
                (absolute-days 1900 1 1))
             7)
    0 :Llun
    1 :Mawrth
    2 :Mercher
    3 :Iau
    4 :Gwerner
    5 :Sadwrn
    6 :Sul))


(count (filter #(= :Llun %) (for [year (range 1901 2000)
                                  month (range 1 13)]
                              (get-weekday {:year year :month month :day 1}))))
