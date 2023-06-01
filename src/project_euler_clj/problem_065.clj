;; Problem 65

;; <p>The square root of 2 can be written as an infinite continued fraction.</p>
;; <p>$\sqrt{2} = 1 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2 + ...}}}}$</p>
;; <p>The infinite continued fraction can be written, $\sqrt{2} = [1; (2)]$, $(2)$ indicates that 2 repeats <i>ad infinitum</i>. In a similar way, $\sqrt{23} = [4; (1, 3, 1, 8)]$.</p>
;; <p>It turns out that the sequence of partial values of continued fractions for square roots provide the best rational approximations. Let us consider the convergents for $\sqrt{2}$.</p>
;; <p>$\begin{align}
;; &amp;1 + \dfrac{1}{2} = \dfrac{3}{2} \\
;; &amp;1 + \dfrac{1}{2 + \dfrac{1}{2}} = \dfrac{7}{5}\\
;; &amp;1 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2}}} = \dfrac{17}{12}\\
;; &amp;1 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2 + \dfrac{1}{2}}}} = \dfrac{41}{29}
;; \end{align}$</p>
;; <p>Hence the sequence of the first ten convergents for $\sqrt{2}$ are:</p>
;; <p>$1, \dfrac{3}{2}, \dfrac{7}{5}, \dfrac{17}{12}, \dfrac{41}{29}, \dfrac{99}{70}, \dfrac{239}{169}, \dfrac{577}{408}, \dfrac{1393}{985}, \dfrac{3363}{2378}, ...$</p>
;; <p>What is most surprising is that the important mathematical constant,<br />$e = [2; 1, 2, 1, 1, 4, 1, 1, 6, 1, ... , 1, 2k, 1, ...]$.</p>
;; <p>The first ten terms in the sequence of convergents for <i>e</i> are:</p>
;; <p>$2, 3, \dfrac{8}{3}, \dfrac{11}{4}, \dfrac{19}{7}, \dfrac{87}{32}, \dfrac{106}{39}, \dfrac{193}{71}, \dfrac{1264}{465}, \dfrac{1457}{536}, ...$</p>
;; <p>The sum of digits in the numerator of the 10<sup>th</sup> convergent is $1 + 4 + 5 + 7 = 17$.</p>
;; <p>Find the sum of digits in the numerator of the 100<sup>th</sup> convergent of the continued fraction for $e$.</p>

(ns project-euler-clj.problem-065
  (:require [project-euler-clj.common :as common]))

(defn lazy-continued-fraction
  ([lazy-sq] (let [a0 (first lazy-sq)
                   a1 (second lazy-sq)]
               (lazy-continued-fraction [a0 1]
                                        [(inc (* a0 a1)) a1]
                                        (drop 2 lazy-sq))))
  ([frac1 frac2 lazy-sq] (let [a (first lazy-sq)
                               num1 (first frac1)
                               num2 (first frac2)
                               den1 (second frac1)
                               den2 (second frac2)
                               ]
                           (cons frac1 (lazy-seq (lazy-continued-fraction frac2
                                                                         [(+ (* a num2) num1)
                                                                          (+ (* a den2) den1)]
                                                                         (rest lazy-sq)))))))

(defn napier-cfrac-coeff-gen
  ([] (napier-cfrac-coeff-gen 1N))
  ([k] 
   (cons (if (= 1 k)
           2N
           (if (= 0 (rem k 3))
             (* 2 (/ k 3))
             1N))
         (lazy-seq (napier-cfrac-coeff-gen (inc k))))))

(defn solve
  []
  (reduce +
          (common/num-to-digit-seq
           (first
            (last
             (take 100
                   (lazy-continued-fraction
                    (napier-cfrac-coeff-gen))))))))
