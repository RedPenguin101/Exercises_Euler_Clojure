; Each new term in the Fibonacci sequence is generated 
; by adding the previous two terms. By starting with 
; 1 and 2, the first 10 terms will be:
; 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

; By considering the terms in the Fibonacci sequence 
; whose values do not exceed four million, find the sum 
; of the even-valued terms.

(defn fib-below
  [below-this]
  (loop [fib-numbers [1 1]]
    (if (> (last fib-numbers) below-this)
      fib-numbers
      (recur 
        (conj 
          fib-numbers 
          (reduce + (take-last 2 fib-numbers)))))))

(defn sum-even-fib
  [below-this]
  (reduce + (filter even? (fib-below 4000000))))
