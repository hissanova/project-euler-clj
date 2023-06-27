;; Problem 75
;; <p>It turns out that $\pu{12 cm}$ is the smallest length of wire that can be bent to form an integer sided right angle triangle in exactly one way, but there are many more examples.</p>
;; <ul style="list-style-type:none;">
;; <li>$\pu{\mathbf{12} \mathbf{cm}}$: $(3,4,5)$</li>
;; <li>$\pu{\mathbf{24} \mathbf{cm}}$: $(6,8,10)$</li>
;; <li>$\pu{\mathbf{30} \mathbf{cm}}$: $(5,12,13)$</li>
;; <li>$\pu{\mathbf{36} \mathbf{cm}}$: $(9,12,15)$</li>
;; <li>$\pu{\mathbf{40} \mathbf{cm}}$: $(8,15,17)$</li>
;; <li>$\pu{\mathbf{48} \mathbf{cm}}$: $(12,16,20)$</li></ul>
;; <p>In contrast, some lengths of wire, like $\pu{20 cm}$, cannot be bent to form an integer sided right angle triangle, and other lengths allow more than one solution to be found; for example, using $\pu{120 cm}$ it is possible to form exactly three different integer sided right angle triangles.</p>
;; <ul style="list-style-type:none;">
;; <li>$\pu{\mathbf{120} \mathbf{cm}}$: $(30,40,50)$, $(20,48,52)$, $(24,45,51)$</li></ul>

;; <p>Given that $L$ is the length of the wire, for how many values of $L \le 1\,500\,000$ can exactly one integer sided right angle triangle be formed?</p>


(ns project-euler-clj.problem-075
  (:require [project-euler-clj.common :as common]))

(defn coprime?
  [m n]
  (let [f (/ m n)]
    (= m (numerator f))))


(defn get-coprimes-upto
  ([n]
   (filter #(coprime? % n) (range 1 n)))
  ([n condition]
   (take-while #(condition %) (get-coprimes-upto n))))

(get-coprimes-upto 50000 (fn [x] (> 90000 (+ x 50000))))

(defn triple
  [m n]
  (let [m-sqrd (* m m)
        n-sqrd (* n n)]
    [(Math/abs (- m-sqrd n-sqrd))
     (* 2 (* m n))
     (+ m-sqrd n-sqrd)]))


(map (fn [n] (map (fn [m] [[m n] (triple m n) (* 2 (* n (+ m n)))])
                  (get-coprimes-upto n (fn [x] (> limit (+ x n))))))
     (range 1 limit))

(let [s 12
      t [3 4 5]]
  (take 10 (iterate (fn [[ss tt]] [(+ ss s)
                                   (map + t tt)])
                    [s t])))

(defn solve [sum-limit]
  (let [gen-limit (int (Math/sqrt (inc (/ sum-limit 2))))]
    (loop [m 1
           ns (get-coprimes-upto m)
           triples {}]
      (if (= gen-limit m)
        triples
        (recur (inc m)
               (get-coprimes-upto (inc m))
               (reduce (fn [triples-map [sum t]]
                         (update triples-map
                                 sum
                                 (fnil #(clojure.set/union % #{(sort t)})
                                       #{})))
                       triples
                       (apply concat
                              (map (fn [n] (let [triple (triple m n)
                                                 s (reduce + triple)]
                                             (take-while (fn [x] (>= sum-limit (first x)))
                                                         (iterate (fn [[ss tt]] [(+ s ss)
                                                                                 (map + triple tt)])
                                                                  [s triple]))))
                                   ns))))))))
