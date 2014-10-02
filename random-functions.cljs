(ns random-functions
  (:require [clojure.string :as string]))

(defn factorial [n]
  (->> n (+ 1) range rest (apply *)))

(factorial 10)

(defn derive_helper [f h x]
  (/ (- (f (+ h x)) (f x)) h))

(defn derive [f h]
  (partial derive_helper f h))

(def square #(* % %))
((derive square 0.00000001) 3)

(defn newton [f xn i]
  "Applies newton's method to find the 0 of a function.
  It takes a function (that takes one Number argument and returns a Number), a first guess and the max number of iterations."
  (if (> i 0)
    (newton f (- xn (/ (f xn) ((derive f 0.00000000000001) xn))) (- i 1))
    xn))

(newton square 5 100)

(factorial 170)

(defn permutations [n]
  "Returns all permutations of numbers smaller or equal to n."
  (if (> n 1)
    (conj
      (reduce
       (fn [acc val]
         (conj
          (conj acc val)
          (conj val n)))
       []
       (permutations (- n 1)))
      (list n))
    (list (list n))))

(permutations 3)


;; helper functions for Luhn's algorithm
(defn map-every-nth [f n coll]
  (map-indexed #(if (zero? (mod (inc %1) n)) (f %2) %2) coll))

(defn rev [f]
  ""
  (fn [a b] (f b a)))

(defn luhn [n]
  "This is an algorithm used to validate ID numbers like credit card numbers.
   This is mainly used to check for errors in entries"
  (->> n
       str
       ((rev string/split) #"")
       rest
       (map js/parseInt)
       (map-every-nth #(* % 2) 2)
       (map
         #(if (> % 9)
           (->> %
             str
             ((rev string/split) #"")
             rest
             (map js/parseInt)
             (reduce +))
           %))
       (reduce +)
       ((rev mod) 10)
       (= 0)
       ))

(luhn 79927398713)
