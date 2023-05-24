;; Problem 60
;; The primes 3, 7, 109, and 673, are quite remarkable. By taking any two primes and concatenating them in any order the result will always be prime. For example, taking 7 and 109, both 7109 and 1097 are prime. The sum of these four primes, 792, represents the lowest sum for a set of four primes with this property.

;; Find the lowest sum for a set of five primes for which any two primes concatenate to produce another prime.
(ns project-euler-clj.problem-060
  (:require [project-euler-clj.common :as common])
  (:require [clojure.math.combinatorics :as combo]))


(defn concat-nums
  [& m]
  (common/digit-seq-to-num (flatten (concat (map common/num-to-digit-seq
                                                 m)))))

(map #(apply concat-nums %)
     (combo/permuted-combinations [12 32 89] 2))

(defn form-new-prime-tuple?
  [tuple prime]
  (every? common/is-prime?
          (map #(apply concat-nums %)
               (apply concat (map (fn [p] [[p prime] [prime p]])
                                  tuple)))))

(form-new-prime-tuple? [3 7 109] 673)

(defn form-new-tuples
  [p-tuples prime]
  (filter some?
          (pmap (fn [t] (when (form-new-prime-tuple? t prime)
                         (conj t prime)))
               p-tuples)))

(form-new-tuples [[2] [3] [5]] 7)



(defn solve
  [tuple-num]
  (let [init-gen (filter common/is-prime? (drop 1 (range)))]
    (loop [prime-gen (drop 2 init-gen)
           prime-tuple-list [[[(first init-gen)]]]
           current-prime (second init-gen)]
      (do (if (= 0 (rem (count (first prime-tuple-list))
                        100))
            (println "latest prime is: " (last (first prime-tuple-list))))
        (if (= tuple-num (count prime-tuple-list))
            prime-tuple-list
            (let [new-list (pmap #(form-new-tuples % current-prime)
                                prime-tuple-list)]
              (recur (rest prime-gen)
                     (map concat
                          (if (not-empty (last new-list))
                            (concat prime-tuple-list [[]])
                            prime-tuple-list)
                          (concat [[[current-prime]]] new-list))
                     (first prime-gen))))))))
