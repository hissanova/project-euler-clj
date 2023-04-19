;; Problem 37
;; The number 3797 has an interesting property. Being prime itself, it is possible to continuously remove digits from left to right, and remain prime at each stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797, 379, 37, and 3.

;; Find the sum of the only eleven primes that are both truncatable from left to right and right to left.

;; NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.

(ns project-euler-clj.problem-037
  (:require [project-euler-clj.common :as common]))

(def memo-primes-upto (memoize common/get-primes-upto))

;; (defn candidates
;;   [limit]
;;   (filter #(and (< 1 (count %))
;;                 (not-any? even? %)
;;                 (not-any? (fn [n] (= 5 n)) %)
;;                 (every? #{3 7} [(first %) (last %)]))
;;           (map common/num-to-digit-seq
;;                (memo-primes-upto limit))))

(defn is-divisible-by-11
  [digit-seq]
  (reduce (fn [acc [d p]] (+ acc (* d (int (Math/pow -1 p)))))
          0
          (map vector
               digit-seq
               (range (count digit-seq)))))

(defn candidates
  [max-num-digits]
  (if (> 2 max-num-digits)
    (throw (Exception. (format "Number of digits must be more than 1: %" max-num-digits)))
    (map common/digit-seq-to-num
         (filter #(and (not= 0 (mod (reduce + %) 3))
                       (not= 0 (is-divisible-by-11 %)))
                 (apply concat
                        (map (fn [n] (common/product (cons [3 7]
                                                           (concat (repeat n [1 3 7 9])
                                                                   [[3 7]]))))
                             (range (dec max-num-digits))))))))

(def head-tail-map {0 [3 3],1 [3 7],2 [7 3],3 [7 7]})
(def interior-map {0 1, 1 3, 2 7, 3 9})

(defn- inc-digits
  [digit-rep]
  (let [digits-num (digit-rep :digits-num)
        head-tail-num (digit-rep :head-tail)
        head-tail-nums (map head-tail-map (common/num-to-digit-seq head-tail-num 2))
        interior-num (digit-rep :interior)
        interior-nums (common/num-to-digit-seq interior-num 4)]
    (if (= 0 digits-num)
      (if (= 3 head-tail-num)
        (assoc (update digit-rep :digits-num inc)
               :head-tail 0)
        (update digit-rep :head-tail inc))
      (if (= interior-num (dec (int (Math/pow 4 digits-num))))
        (if (= 3 head-tail-num)
          (assoc (update digit-rep :digits-num inc)
                 :interior 0
                 :head-tail 0)
          (assoc (update digit-rep :head-tail inc)
                 :interior 0))
        (update digit-rep :interior inc)))))

(defn lazy-candidate-gen
  ([]
   (lazy-candidate-gen {:head-tail 0 :interior 0 :digits-num 0}))
  ([digit-seq]
   (lazy-seq (cons digit-seq (lazy-candidate-gen (inc-digits digit-seq))))))

(defn digits-rep-to-num-seq
  [digits-rep]
  (let [[head tail] (head-tail-map (digits-rep :head-tail))
        interior (map interior-map (common/num-to-digit-seq (digits-rep :interior) 4))]
    (if (= 0 (digits-rep :digits-num))
      (cons head (list tail))
      (cons head (conj (vec (concat (repeat (- (digits-rep :digits-num) (count interior)) 1)
                                    (vec interior)))
                       tail)))))

(defn gen-candidates
  []
  (filter #(and (not= 0 (mod (reduce + %) 3))
                (not= 0 (is-divisible-by-11 %)))
          (map digits-rep-to-num-seq (lazy-candidate-gen))))

(defn left-truncator
  ([s] (left-truncator s []))
  ([s acc] (if (= (count s) 0)
             acc
             (recur (vec (rest s))
                    (conj acc s)))))

(defn right-truncator
  ([s] (right-truncator s []))
  ([s acc] (if (= (count s) 0)
             acc
             (recur (vec (take (dec (count s)) s))
                    (conj acc s)))))

(defn gen-truncations
  [digit-seq]
  (map common/digit-seq-to-num (concat (left-truncator digit-seq)
                                       (rest (right-truncator digit-seq)))))

(defn is-truncatable?
  [digit-seq]
  (every? common/is-prime? (set (gen-truncations digit-seq))))

(defn solve []
  (take 11
        (filter is-truncatable?
                (map (fn [[i x]] (if (= 0 (mod i 1000))
                                   (do (println (format "%d evaluated" i))
                                       x)
                                   x))
                     (map-indexed vector (gen-candidates))))))

(defn left-truncate
  [candidate left-truncatables]
  (loop [d-seq candidate
         primes #{}]
    (if (or (= 0 (count d-seq))
            (contains? left-truncatables d-seq))
      (clojure.set/union left-truncatables primes)
      (if (not (common/is-prime? (common/digit-seq-to-num d-seq)))
        left-truncatables
        (recur (rest d-seq)
               (conj primes d-seq))))))

(defn right-truncate
  [candidate right-truncatables]
  (loop [d-seq  candidate
         primes #{}]
    (if (or (= 0 (count d-seq))
            (contains? right-truncatables d-seq))
      (clojure.set/union right-truncatables primes)
      (if (not (common/is-prime? (common/digit-seq-to-num d-seq)))
        right-truncatables
        (recur (take (dec (count d-seq)) d-seq)
               (conj primes d-seq))))))

(defn solve2
  []
  (reduce (fn [[left-truncatables
                right-truncatables
                truncatables]
               [index
                candidate]]
            (do (when (= (mod index 1000) 0)
                  (println (format "%d evaluated." index))
                  (println (sort-by vec left-truncatables))
                  (println (sort-by vec right-truncatables))
                  (println truncatables))
                (let [left-truncatables' (left-truncate candidate left-truncatables)
                      right-truncatables' (right-truncate candidate right-truncatables)]
                  [left-truncatables'
                   right-truncatables'
                   (if (and (> (count left-truncatables')
                               (count left-truncatables))
                            (> (count right-truncatables')
                               (count right-truncatables)))
                     (conj truncatables candidate)
                     truncatables)])))
          [#{} #{} []]
          (map-indexed vector (gen-candidates))))


(defn gen-left-truncatables
  ([] (gen-left-truncatables '(3 7)))
  ([left-truncatables]
   (cons left-truncatables
         (lazy-seq (gen-left-truncatables (filter #(common/is-prime? (common/digit-seq-to-num %))
                                                  (map flatten
                                                       (common/product [[1 2 3 5 7 9] left-truncatables]))))))))

(defn gen-right-truncatables
  ([] (gen-right-truncatables '(2 3 5 7)))
  ([right-truncatables]
   (cons right-truncatables
         (lazy-seq (gen-right-truncatables (filter #(common/is-prime? (common/digit-seq-to-num %))
                                                  (map flatten
                                                       (common/product [right-truncatables [1 3 7 9]]))))))))

(defn candidate-filter
  [d-seq]
  (not-any? #{2 5} d-seq))

(def left-truncatable-candidates  (apply concat (take-while #(< 0 (count (filter candidate-filter %)))
                                                            (rest (gen-left-truncatables)))))

(def right-truncatable-candidates (apply concat (take-while #(< 0 (count (filter candidate-filter %)))
                                                            (rest (gen-right-truncatables)))))


(defn solve3
  []
  (reduce + (map common/digit-seq-to-num (clojure.set/intersection (set left-truncatable-candidates)
                                                                   (set right-truncatable-candidates)))))
