(ns euler.core
  (:gen-class))


;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?

;; strategy: starting at 2, run through all primes and divide out, until remainder is prime

(defn prime-factors
  [number]
  0)

(defn all-primes-below
  [number]
  (range 2 number))

(defn sieve
  [number-list factor]
  (filter #(not= (mod % factor) 0) number-list))

(all-primes-below 10)