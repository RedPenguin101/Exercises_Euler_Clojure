; If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
; The sum of these multiples is 23.
; Find the sum of all the multiples of 3 or 5 below 1000.

; when given a divisor returns a function which checks if a number is divisible by that divisor
(defn divisible-by
  [divisor]
  #(= (mod % divisor) 0))

(defn mult-3-5
  [below-this]
  (reduce + 
    (filter 
      #(or ((divisible-by 3) %) ((divisible-by 5) %))
      (range below-this))))

(mult-3-5 1000)