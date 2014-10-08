(ns fibonacci.example)

; Fibonacci sequence:
; U[n] = U[n - 1] + U[n - 2]
; U[0] = 1
; U[1] = 1

; helper function that uses a vector to accumulate the values calculated so far
; Example:
;   (fibs 6) -> 8
;
;   acc looks like:
;   [1 1]
;   [1 1 2]
;   [1 1 2 3]
;   [1 1 2 3 5]
;   [1 1 2 3 5 8] -> then it returns (last acc) -> 8
(defn fibs-helper [n acc]
  (if (<= n 2)
    (last acc)
    (fibs-helper (dec n)
                 (conj acc
                       (+ (last acc)
                          (->> acc
                               reverse
                               second))))))

; returns the nth element in the fibonacci sequence
(defn fibs [n]
  (fibs-helper n [1 1]))

(fibs 6)


