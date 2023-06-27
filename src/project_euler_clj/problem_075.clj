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

(defn triple
  [m n]
  (let [m-sqrd (* m m)
        n-sqrd (* n n)]
    [(Math/abs (- m-sqrd n-sqrd))
     (* 2 (* m n))
     (+ m-sqrd n-sqrd)]))


(defn get-triples-upto-sum-limit
  [m n sum-limit]
  (let [dt (triple m n)
        ds (reduce + dt)]
    (take-while (fn [x] (>= sum-limit (first x)))
                (iterate (fn [[s t]] [(+ s ds)
                                      (map + t dt)])
                         [ds dt]))))

(defn calculate-sum-to-triples-map
  [sum-limit]
  (let [gen-limit (int (Math/sqrt (inc (/ sum-limit 2))))]
    (loop [m 1
           ns (get-coprimes-upto m)
           triple-map {}]
      (if (= gen-limit m)
        triple-map
        (recur (inc m)
               (get-coprimes-upto (inc m))
               (reduce (fn [t-map [sum t]]
                         (update t-map
                                 sum
                                 (fnil #(clojure.set/union % #{(sort t)})
                                       #{})))
                       triple-map
                       (apply concat
                              (map #(get-triples-upto-sum-limit m % sum-limit)
                                   ns))))))))

(defn solve
  [sum-limit]
  (count
   (filter (fn [x] (= 1 (count (second x))))
           (into (sorted-map)
                 (calculate-sum-to-triples-map sum-limit)))))
