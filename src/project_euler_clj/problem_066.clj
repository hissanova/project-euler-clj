;; Problem 66
;; <p>Consider quadratic Diophantine equations of the form:
;; $$x^2 - Dy^2 = 1$$</p>
;; <p>For example, when $D=13$, the minimal solution in $x$ is $649^2 - 13 \times 180^2 = 1$.</p>
;; <p>It can be assumed that there are no solutions in positive integers when $D$ is square.</p>
;; <p>By finding minimal solutions in $x$ for $D = \{2, 3, 5, 6, 7\}$, we obtain the following:</p>
;; \begin{align}
;; 3^2 - 2 \times 2^2 &amp;= 1\\
;; 2^2 - 3 \times 1^2 &amp;= 1\\
;; {\color{red}{\mathbf 9}}^2 - 5 \times 4^2 &amp;= 1\\
;; 5^2 - 6 \times 2^2 &amp;= 1\\
;; 8^2 - 7 \times 3^2 &amp;= 1
;; \end{align}
;; <p>Hence, by considering minimal solutions in $x$ for $D \le 7$, the largest $x$ is obtained when $D=5$.</p>
;; <p>Find the value of $D \le 1000$ in minimal solutions of $x$ for which the largest value of $x$ is obtained.</p>

(ns project-euler-clj.problem-066
  (:require [project-euler-clj.common :as common]))

(defn equation
  [x D y]
  (= 1 (- (* x x) (* D (* y y)))))

(equation 3 2 2)

(defn find-1st-solution
  [D]
  (let [sqrt (Math/sqrt D)]
    (first (filter some?
                   (for [x (filter #(>= % (int sqrt)) (range))
                         y (range (max 1 (int (/ (- x 1) sqrt)))
                                  (inc (Math/ceil (/ x sqrt))))]
                     (if (equation x D y)
                       [x D y]))))))
(find-1st-solution 10)

(defn is-square?
  [n]
  (every? even? (map :exp (common/prime-factor n))))

(defn is-square?2
  [n]
  (= n (first (filter #(>= % n) (common/square-seq)))))


(take 5 (common/square-seq))
(is-square?2 144)

(defn is-square?3
  [n]
  (if (= n (common/pow (bigint (Math/sqrt n)) 2))
    n))

(defn find-1st-solution2
  [D]
  (let [sqrt (Math/sqrt D)
        num-gen (drop 1 (range))]
    (first (filter empty
                   (pmap (fn [y]
                          (if-let [y-sqrd-dec (is-square?3 (inc (* D (* y y))))]
                            [(int (Math/sqrt y-sqrd-dec)) D y]))
                        num-gen)))))

(find-1st-solution2 61)
(is-square? 9)


(defn find-1st-solution3
  [D]
  (let [cnt-frc (common/cont-frac-of-sqrt-n D)]
    (first (filter (fn [triple] (apply equation triple))
                   (map (fn [[p q]] [p D q])
                        (common/lazy-continued-fraction
                         (map bigint (flatten (cons (first cnt-frc)
                                                    (repeat (rest cnt-frc)))))))))))
(find-1st-solution3 61)

(defn solve
  [D-max]
  (map #(find-1st-solution %)
       (filter (comp not is-square?3) (range 2 D-max))))

(defn solve2
  [D-max]
  (map #(find-1st-solution3 %)
       (filter (comp not is-square?3) (range 2 D-max))))

