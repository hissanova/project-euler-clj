;; Problem 67
;; <p>By starting at the top of the triangle below and moving to adjacent numbers on the row below, the maximum total from top to bottom is 23.</p>
;; <p class="monospace center"><span class="red"><b>3</b></span><br><span class="red"><b>7</b></span> 4<br>
;; 2 <span class="red"><b>4</b></span> 6<br>
;; 8 5 <span class="red"><b>9</b></span> 3</p>
;; <p>That is, 3 + 7 + 4 + 9 = 23.</p>
;; <p>Find the maximum total from top to bottom in <a href="resources/documents/0067_triangle.txt">triangle.txt</a> (right click and 'Save Link/Target As...'), a 15K text file containing a triangle with one-hundred rows.</p>
;; <p class="smaller"><b>NOTE:</b> This is a much more difficult version of <a href="problem=18">Problem 18</a>. It is not possible to try every route to solve this problem, as there are 2<sup>99</sup> altogether! If you could check one trillion (10<sup>12</sup>) routes every second it would take over twenty billion years to check them all. There is an efficient algorithm to solve it. ;o)</p>

(ns project-euler-clj.problem-067
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st])
  (:require [project-euler-clj.problem-018 :as p018]))


(defn parse-triangle
  [triangle-str]
  (map #(map (fn [n] (Integer/parseInt n)) (st/split % #" ")) (st/split-lines triangle-str)))

(def triangle (parse-triangle (slurp "src/project_euler_clj/data/0067_triangle.txt")))

(defn get-max-path-naive
  [tri]
  (p018/get-max-seq (p018/paths-to-vals tri
                              (p018/get-loci (map p018/render-path
                                             (p018/path-seeds (dec (count tri))))))))
(map #(get-max-path-naive (take % triangle))
     (range 2 10))


(defn binomial-coeff
  [n k]
  (/ (common/factorial n) (* (common/factorial k)
                             (common/factorial (- n k)))))
(binomial-coeff 2 2)
(defn get-binomial-coeffs
  [n]
  (map #(binomial-coeff n %) (range (inc n))))

(get-binomial-coeffs 3)

(defn mean-with-binomial-weights
  [values]
  (let [len (dec (count values))
        normaliser (common/pow 2 len)]
    (float (/ (reduce + (map * values (get-binomial-coeffs len)))
              normaliser))))

(mean-with-binomial-weights [1 2 1])

(defn solve-wrongly []
  (loop [path [(first (first triangle))]
         last-index 0
         row (second triangle)
         remain (drop 2 triangle)]
    (let [cand1 [last-index (nth row last-index)]
          cand2 [(inc last-index) (nth row (inc last-index))]
          ]
      (println "SUM:" path)
      (let [[n1 n2] (map last [cand1 cand2])]
        (cond
          (= n1 n2) (println "same!!")
          (> n1 n2) (println n1)
          (< n1 n2) n2
          ))
      (if (empty? remain)
        path
        (let [[new-index increment] (max-key last cand1 cand2)]
          (recur (conj path increment)
                 new-index
                 (first remain)
                 (rest remain)))))))

;; The correct solution starts from here.

(defn duplicate-inbetweeners
  [sq]
  (let [len (count sq)]
    (cons (first sq)
          (concat (flatten (map (fn [x] (repeat 2 x))
                                (drop 1 (take (dec len) sq))))
                  [(last sq)]))))

(defn find-local-max-path
  [upper-row lower-row]
  (map #(apply max %)
       (map (fn [n [m1 m2]] [(+ m1 n) (+ m2 n)])
            lower-row (partition 2 (duplicate-inbetweeners upper-row)))))

(find-local-max-path [1 2 3] [1 2])

(defn solve []
  (let [rev-triangle (reverse triangle)]
    (loop [upper-row (first rev-triangle)
           lower-row (second rev-triangle)
           remain (drop 2 rev-triangle)]
     (if (empty? lower-row)
       (first upper-row)
       (recur (find-local-max-path upper-row lower-row)
              (first remain)
              (rest remain))))))
