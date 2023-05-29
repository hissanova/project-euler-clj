;; Problem 59
;; <p>Each character on a computer is assigned a unique code and the preferred standard is ASCII (American Standard Code for Information Interchange). For example, uppercase A = 65, asterisk (*) = 42, and lowercase k = 107.</p>
;; <p>A modern encryption method is to take a text file, convert the bytes to ASCII, then XOR each byte with a given value, taken from a secret key. The advantage with the XOR function is that using the same encryption key on the cipher text, restores the plain text; for example, 65 XOR 42 = 107, then 107 XOR 42 = 65.</p>
;; <p>For unbreakable encryption, the key is the same length as the plain text message, and the key is made up of random bytes. The user would keep the encrypted message and the encryption key in different locations, and without both "halves", it is impossible to decrypt the message.</p>
;; <p>Unfortunately, this method is impractical for most users, so the modified method is to use a password as a key. If the password is shorter than the message, which is likely, the key is repeated cyclically throughout the message. The balance for this method is using a sufficiently long password key for security, but short enough to be memorable.</p>
;; <p>Your task has been made easy, as the encryption key consists of three lower case characters. Using <a href="resources/documents/0059_cipher.txt">0059_cipher.txt</a> (right click and 'Save Link/Target As...'), a file containing the encrypted ASCII codes, and the knowledge that the plain text must contain common English words, decrypt the message and find the sum of the ASCII values in the original text.</p>

(ns project-euler-clj.problem-059
  (:require [project-euler-clj.common :as common])
  (:require [clojure.string :as st]))

(def source-file "./src/project_euler_clj/data/0059_cipher.txt")
(def ciphered-text (map #(Integer/parseInt %)
                    (st/split (slurp source-file) #",")))

(defn decipher
  [dec-key text]
  (map (fn [byte-ar] (map (comp char bit-xor) byte-ar dec-key))
       (partition (count dec-key) text)))

(def key-candidates (common/product (repeat 3 (range (int \a) (inc (int \z))))))

(filter (fn [[k [original match]]] (= original match))
          (map (fn [[k s]] [k [s (re-find #"[\p{Alpha} ]+" s)]])
               (map (fn [k] [k (st/join (take 10
                                              (flatten (decipher k ciphered-text))))])
                    key-candidates)))

(def the-key '(101 120 112))

(st/join (flatten (decipher the-key ciphered-text)))

(defn solve
  []
  (reduce + (map int (flatten (decipher the-key ciphered-text)))))
