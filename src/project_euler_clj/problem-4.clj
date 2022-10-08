;; A palindromic number reads the same both ways. The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
;; Find the largest palindrome made from the product of two 3-digit numbers.

(defn n-digit-nums
  [n]
  (range (dec (int (Math/pow 10 n)))
         (int (dec (Math/pow 10 (dec n))))
         -1))
(n-digit-nums 2)
(defn perm-two-vecs
  [vec1 vec2]
  (for [x vec1
        y vec2]
    (* x y)))

(perm-two-vecs (n-digit-nums 2) (n-digit-nums 2))

(defn palindrome?
  [n]
  (= n (Integer/parseInt (apply str (reverse (str n))))))

(int (apply str (reverse (str 19))))
(Integer/parseInt "21")
(palindrome? 101)

(defn get-n-digits-palindrome
  [n]
  (filter palindrome? (perm-two-vecs (n-digit-nums n)
                                     (n-digit-nums n))))

(reverse (sort (get-n-digits-palindrome 3)))
