(ns random-functions
  (:require [clojure.string :as string]))

(defn factorial [n]
  (->> n (+ 1) range rest (apply *)))

(factorial 10)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn derive_helper [f h x]
  (/ (- (f (+ h x)) (f x)) h))

(defn derive [f h]
  (partial derive_helper f h))

(def square #(* % %))
((derive square 0.00000001) 3)

(defn newton [f xn i]
  "Applies newton's method to find the 0 of a function.
  It takes a function (that takes one Number argument and returns a Number), a
  first guess and the max number of iterations."
  (if (> i 0)
    (newton f (- xn (/ (f xn) ((derive f 0.00000000000001) xn))) (- i 1))
    xn))

(newton square 5 100)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; I'm going to re-write this in a better way (more clojure-ish way)

#_(defn permutations [n]
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

#_(permutations 3)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; helper functions for Luhn's algorithm
(defn rev [f]
  "This is used to reverse the arguments"
  (fn [a b] (f b a)))

(defn luhn [n]
  "This is an algorithm used to validate ID numbers like credit card numbers.
   This is mainly used to check for errors in entries"
  (->> n
       str
       ((rev string/split) #"")
       rest
       (map js/parseInt)
       (map-indexed
        #(if (zero? (mod (inc %1) 2))
          (* %2 2)
         %2))
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
       zero?
       ))

(luhn 79927398713)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; I had a conversation with a friend about how to check if an array contained
; ordered elements. isOrdered1 was my solution, and isOrdered2 was his.

; My solution
(defn isOrdered1 [coll]
  (reduce (fn [acc val]
            (and acc
                 (<= (get val 0) (get val 1))))
          true
          (map vector
               coll
               (rest coll))))

; His solution
(defn isOrdered2 [coll]
  (every? true?
          (map #(<= %1 %2)
               coll
               (rest coll))))

; Once we shared our solution, we had a long discussion about which one was
; better and why. His solution is fundamentally better because it assumes
; independance between each element. Independance betwen each elements means that
; the elements in the collection aren't dependent on each other and their order
; doesn't matter. This notion of independence of elements in a collection
; is very important because it comes with huge benefits. The first one is that
; since you're able to use map, your function is going to work on infinite
; streams by default, no need to change anything. Also, you gain the ability to
; parallelize the function with a parallelized version of map.
; He realized that once he removed the dependency between the elements of the
; he could use map and avoid reduce. Map makes for a much faster and scalable
; solution. What's great is that map returns a lazy-seq, so every? is going to
; stop as soon as the array is found to not be ordered.
; Reduce imposes an order to the elements in the vector making them
; dependent for no reason. This comes with the cost that reduce won't stop as soon
; as we found that the array wasn't ordered, it can't and shouldn't by its nature.

; So the lesson here is that if you want to go through a collection and explore
; each elements individually, you should use map. If you're doing something that
; requires an accumulator of values, use reduce. If you can't decide, use map.
; Finally, if you're writing a function that forces a dependency between the
; elements of the collection, try to find a way to remove that dependency and then
; use map.

; In the end the best way of doing it is this
(defn isOrdered3 [coll]
  (apply <= coll))


(assert (isOrdered1 [1 2 3 4]))
(assert (isOrdered2 [1 2 3 4]))
(assert (isOrdered3 [1 2 3 4]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Fibonnacci sequence: 1 1 2 3 5 8 13 21
; first version of fibs
(defn fibs [n]
  (if (<= n 2)
    1
    (+ (fibs (dec n)) (fibs (- n 2)))))

(assert (= (fibs 6) 8))

; second version with accumulator
(defn fibs2_helper [n acc]
  (if (<= n 1)
    acc
    (fibs2_helper (dec n) ())))

(defn fibs2 [n]
  (fibs2_helper n [1 1]))
(last [1 2 3 4])
(fibs2 7)
