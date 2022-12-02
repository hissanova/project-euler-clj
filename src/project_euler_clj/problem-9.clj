;; <p>A Pythagorean triplet is a set of three natural numbers, <var>a</var> &lt; <var>b</var> &lt; <var>c</var>, for which,</p>
;; <div class="center"> <var>a</var><sup>2</sup> + <var>b</var><sup>2</sup> = <var>c</var><sup>2</sup></div>
;; <p>For example, 3<sup>2</sup> + 4<sup>2</sup> = 9 + 16 = 25 = 5<sup>2</sup>.</p>
;; <p>There exists exactly one Pythagorean triplet for which <var>a</var> + <var>b</var> + <var>c</var> = 1000.<br />Find the product <var>abc</var>.</p>

(def const 1000)
(defn get-domain
  []
  (for [a (range 1 333)
        b (range a 499)]
    [a b (- 1000 (+ a b))]))

(defn pythagorean-triple?
  [a b c]
  (= (+ (* a a) (* b b)) (* c c)))

;; The answer is
(map #(reduce * %) (filter #(apply pythagorean-triple? %) (get-domain)))

