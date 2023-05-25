;; Problem 54
;; In the card game poker, a hand consists of five cards and are ranked, from lowest to highest, in the following way:

;; High Card: Highest value card.
;; One Pair: Two cards of the same value.
;; Two Pairs: Two different pairs.
;; Three of a Kind: Three cards of the same value.
;; Straight: All cards are consecutive values.
;; Flush: All cards of the same suit.
;; Full House: Three of a kind and a pair.
;; Four of a Kind: Four cards of the same value.
;; Straight Flush: All cards are consecutive values of same suit.
;; Royal Flush: Ten, Jack, Queen, King, Ace, in same suit.
;; The cards are valued in the order:
;; 2, 3, 4, 5, 6, 7, 8, 9, 10, Jack, Queen, King, Ace.

;; If two players have the same ranked hands then the rank made up of the highest value wins; for example, a pair of eights beats a pair of fives (see example 1 below). But if two ranks tie, for example, both players have a pair of queens, then highest cards in each hand are compared (see example 4 below); if the highest cards tie then the next highest cards are compared, and so on.

;; Consider the following five hands dealt to two players:

;; Hand	 	Player 1	 	Player 2	 	Winner
;; 1	 	5H 5C 6S 7S KD
;; Pair of Fives
;;  	2C 3S 8S 8D TD
;; Pair of Eights
;;  	Player 2
;; 2	 	5D 8C 9S JS AC
;; Highest card Ace
;;  	2C 5C 7D 8S QH
;; Highest card Queen
;;  	Player 1
;; 3	 	2D 9C AS AH AC
;; Three Aces
;;  	3D 6D 7D TD QD
;; Flush with Diamonds
;;  	Player 2
;; 4	 	4D 6S 9H QH QC
;; Pair of Queens
;; Highest card Nine
;;  	3D 6D 7H QD QS
;; Pair of Queens
;; Highest card Seven
;;  	Player 1
;; 5	 	2H 2D 4C 4D 4S
;; Full House
;; With Three Fours
;;  	3C 3D 3S 9S 9D
;; Full House
;; with Three Threes
;;  	Player 1
;; The file, poker.txt, contains one-thousand random hands dealt to two players. Each line of the file contains ten cards (separated by a single space): the first five are Player 1's cards and the last five are Player 2's cards. You can assume that all hands are valid (no invalid characters or repeated cards), each player's hand is in no specific order, and in each hand there is a clear winner.

;; How many hands does Player 1 win?

(ns project-euler-clj.problem-054
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(def games (map (fn [line] (partition 5
                                      (st/split line #" ")))
                (st/split-lines (slurp "./src/project_euler_clj/data/p054_poker.txt"))))

(def ranks [:none
            :one-pair
            :two-pairs
            :triple
            :straight
            :flush
            :full-house
            :quadruple
            :straight-flush
            :royal-flush])

(defn- get-numbers
  [hand]
  (map #(nth % 0) hand))

(defn- get-suits
  [hand]
  (map #(nth % 1) hand))

(defn- to-number
  [ch]
  (let [sym (str ch)]
    (case sym
      "T" 10
      "J" 11
      "Q" 12
      "K" 13
      "A" 14
      (Integer/parseInt sym))))

(to-number \T)
(map to-number (get-numbers (first (first games))))
(get-suits (first (first games)))

(defn reduce-hand
  [hand]
  [(reverse (sort (map to-number (get-numbers hand))))
   (set (get-suits hand))])

(defn count-entries
  [coll]
  (map #(count (filter #{%} coll)) (sort coll)))
(count-entries [1 2 3 4 5])
(map (fn [x] [x ((comp count-entries first reduce-hand) x)])
     (take 10 (apply concat games)))
(reduce-hand (first (first games)))

(defn get-k-v-seq
  [nums]
  (map vector
       (count-entries nums)
       (sort nums)))
(get-k-v-seq [1 2 3 2 1])

(defn group-by-num
  [hand]
  (let [red-hand (reduce-hand hand)
        nums (first red-hand)]
    (mapv (fn [[x sq]] [x (vec (reverse sq))])
          (vec (reverse (into (sorted-map)
                              (reduce (fn [m [k v]] (update m k #(if (some #{v} %) % (vec (conj % v)))))
                                      {}
                                      (get-k-v-seq nums))))))))

(let [nums (first (reduce-hand (first (apply concat games))))]
  (map vector (count-entries nums) nums))
(map group-by-num (take 10 (apply concat games)))

(defn how-many-pairs
  [hand]
  (count (filter #{2}
                 (count-entries (get-numbers hand)))))

(map #(how-many-pairs (first %))
     games)

(defn any-triple?
  [hand]
  (some #{3} (count-entries (get-numbers hand))))

(defn any-quadruple?
  [hand]
  (some #{4} (count-entries (get-numbers hand))))

(defn diff-seq
  [sq]
  (loop [current-term (first sq)
         next-term (second sq)
         remain-sq (drop 2 sq)
         diff-seq []]
    (if (nil? next-term)
      diff-seq
      (recur next-term
             (first remain-sq)
             (rest remain-sq)
             (conj diff-seq (- next-term current-term))))))

(defn straight?
  [hand]
  (every? #{1}
          (diff-seq (sort (map (comp to-number str)
                               (get-numbers hand))))))

(straight? (first (nth games 2)))

(defn flush?
  [hand]
  (= 1 (count (set (get-suits hand)))))

(filter #(flush? %)        
        (apply concat games))

(defn royal?
  [hand]  (= [10 11 12 13 14] (vec (sort (map #(to-number (str %))
                                              (get-numbers hand))))))

(royal? ["TD" "JD" "QD" "KD" "AD"])

(defn- rewrite-if-true
  [[hand old-rank] func new-rank]
  [hand (if (func hand)
          new-rank
          old-rank)])

(rewrite-if-true [["AH" "2H" "2D" "3S" "JC"] nil]
                 (fn [h] (= 1 (how-many-pairs h)))
                 :one-pair)

(defn get-rank
  [hand]
  (-> [hand :none]
      (rewrite-if-true (fn [h] (= 1 (/ (how-many-pairs h) 2)))
                       :one-pair)
      (rewrite-if-true (fn [h] (= 2 (/ (how-many-pairs h) 2)))
                       :two-pairs)
      (rewrite-if-true (fn [h] (any-triple? h))
                       :triple)
      (rewrite-if-true (fn [h] (straight? h))
                       :straight)
      (rewrite-if-true (fn [h] (flush? h))
                       :flush)
      (rewrite-if-true (fn [h] (and (= 1 (how-many-pairs h)) (any-triple? h)))
                       :full-house)
      (rewrite-if-true (fn [h] (any-quadruple? h))
                       :quadruple)
      (rewrite-if-true (fn [h] (and (straight? h) (flush? h)))
                       :straight-flush)
      (rewrite-if-true (fn [h] (and (royal? h) (flush? h)))
                       :royal-flush)
      ))

(filter #(= :royal-flush (nth (get-rank %) 1))
        (apply concat games))

(defn solve []
  (count
   (filter (fn [[[h1 r1] [h2 r2]]] (if (= r1 r2)
                                   (let [hands (map group-by-num [h1 h2])]
                                  (= hands (sort hands)))
                                (< r1 r2)))
         (map #(map (fn [h] [h
                             (.indexOf ranks
                                       (second (get-rank h)))])
                    %)
              games))))
