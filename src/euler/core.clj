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

;; Problem 6: The sum of the squares of the first 10 natural numbers is
;;           1^2 + 2^2 + ... + 10^2 = 385
;; The sum of the square of the first 10 numbers is
;;           (1 + 2 + ... + 10)^2 = 55^2 = 3025
;; Hence the difference between the two is 3025-385 = 2640
;; find the difference for the the first 100 numbers

(defn ** [n x] (reduce * (repeat x n)))

(defn square [x] (** x 2))

(defn sum-of-squares
  [number]
  (reduce #(+ %1 (square %2)) (range 1 (inc number))))

(defn square-of-sums
  [number]
  (square (reduce + (range (inc number)))))

;; Problem 7: 10001st Prime

(defn add-if-prime [n primes]
  (if (some #(divisible-by? n %) primes)
    primes
    (conj primes n)))

(defn primes-to-length
  ([length] (primes-to-length [2] 3 (dec length)))
  ([primes start desired-length]
   (if (> (count primes) desired-length)
     primes
     (recur (add-if-prime start primes) (+ 2 start) desired-length))))

;; A pythagorean triplet is a set of three natural numbers a<b<c
;; such that a^2 + b^2 = c^2
;; like 3 4 5
;; there is one triplet such that a+b+c=1000. Find the product abc

(defn pythag [a b]
  (Math/sqrt (+ (square a) (square b))))

(defn py-sum-to [a]
  #(= a (+ %1 %2 (pythag %1 %2))))

(def py-sum-to-1000 (py-sum-to 1000.0))

(defn find-pyth-trip-with-sum
  [n]
  (for [a (range 1 (/ (- n 3) 3))
        b (range 1 (/ (- n a) 2))
        :when ((py-sum-to (double n)) a b)]
    [(double a) (double b) (pythag a b)]))

;; Problem 10: Find the sum of all primes below 2000000
;; Note, modification of previous is waaay to slow. Used sieve of Eratosthenes

(defn primes-to-size
  ([max-size] (primes-to-size [2] 3 max-size))
  ([primes start max-size]
   (if (> (last primes) max-size)
     (butlast primes)
     (recur (add-if-prime start primes) (+ 2 start) max-size))))

(def not-divisible-by? (complement divisible-by?))

(defn prime-sieve
  ([max-size] (prime-sieve [] (range 2 (inc max-size)) (int (Math/sqrt max-size))))
  ([primes integer-list stop-at]
   (if (> (first integer-list) stop-at)
     (concat primes integer-list)
     (let [[next-prime & ints-to-test] integer-list
           new-primes (conj primes next-prime)]
       (recur new-primes (doall (filter #(not-divisible-by? % next-prime) ints-to-test)) stop-at)))))
