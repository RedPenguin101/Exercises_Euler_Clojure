(ns euler.core
  (:gen-class))

;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?

(defn divisible-by?
  [number divisor]
  (zero? (mod number divisor)))

; if divisor is greater than sqrt of the number
; return the number if it's not 1
; if the number is divisible by the divisor,
; return the divisor consed with recur with the factored out divisor
; else recur with an increased divisor

(defn factors-starting-at [divisor number]
  (cond
    (> divisor (Math/sqrt number)) (if (= number 1) [] [number])
    (divisible-by? number divisor)
    (cons divisor (factors-starting-at divisor (/ number divisor)))
    :else (recur (inc divisor) number)))

(defn prime-factors [number]
  (factors-starting-at 2 number))

;; 4. Largest Palindrome
;; The largest palindrome from the product of 2 digit numbers is 9009 (91x99)
;; what is the largest from 3 digit numbers?

(defn is-palindromic? [number]
  (let [fwd (str number)
        rev (apply str (reverse fwd))]
    (= fwd rev)))

(defn largest-palindrome [number]
  (let [numbers (range 1 (inc number))]
    (apply max (filter is-palindromic? (for [n1 numbers n2 numbers] (* n1 n2))))))
 

;; 2520 is the smallest number that can be divided by each of the numbers from 1-10 without remainder (evenly divisible). What is the smallest number that is evenly divisible by 1-20?

(defn smallest-divisible-by-first
  ([divisor-range] (smallest-divisible-by-first 1 divisor-range))
  ([number divisor-range]
   (let [numbers (reverse (range 1 (inc divisor-range)))]
     (if (every? #(divisible-by? number %) numbers)
       number
       (recur (inc number) divisor-range)))))
