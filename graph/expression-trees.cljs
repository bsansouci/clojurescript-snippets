(ns expression-tree)

; Let's re-invent math!

; Let's start by defining a binary tree for operations
(def tree {
 :left {:value 1}
 :right {:value 1}
 :value +})

; Now define a way to travese it and apply a function on each node
(defn depth-first-evaluation [t f]
  (if
   (= (:left t) (:right t) nil) (:value t)
   ((f t) (depth-first-evaluation (:left t) f)
          (depth-first-evaluation (:right t) f))))

; Now we can evaluate our expression tree
(defn evaluate [t]
  (depth-first-evaluation t #(:value %)))

(def tree2 {:left tree
            :right tree
            :value *})

(evaluate tree2)

; Cool, now let's make a way to compare two operations
(defn is-equal [t1 t2]
  (cond
   (and (nil? t1) (nil? t2)) true
   (not= (:value t1) (:value t2)) false
   :else (and (is-equal (:right t1) (:right t2))
              (is-equal (:left t1) (:left t2)))))

(is-equal tree2 tree2)

(is-equal tree2 tree)

(defn plus [exp1 exp2]
  {:left exp1
   :right exp2
   :value +})

(def two {:value 2})

(evaluate (plus two two))

; pretty cool but we could do better. We don't want to redefine all the functions to do math. So why not use
; what we know about higher order functions to generate function.

(defn expressionify [operator]
  (fn [exp1 exp2]
    {:left exp1
      :right exp2
      :value operator}))

; now we can go back to the old definition of + and definie our own
(def ++ (expressionify +))

(evaluate (++ two two))

; Now let's forget everything and change the name of the function evaluate to toString. Convince yourself
; that we're doing all the operations when you want them to happen, and toString is simply a way to format
; the data properly.
(def toString evaluate)

; we still have our good old addition
(+ 1 1)

; and we have our own addition
(toString (++ two two))


; One last change that I just thought of. In clojure we get the freedom to have functions that work on an
; undefined number of arguments, so we shouldn't limit ourselves to 2.
(def tree3 {:children [{:value 1} {:value 2}]
            :value +})

; Simple reimplementation of depth-first-search from above
(defn depth-first-evaluation2 [t]
  (cond (or (nil? (:children t)) (empty? (:children t))) (:value t)
        :else (apply (:value t) (map depth-first-evaluation2 (:children t)))))

; We rename our old function
(def toStringOLD toString)

(defn toString [t]
  (depth-first-evaluation2 t))

(toString tree3)

(def expressionifyOLD expressionify)
(defn expressionify [operator]
  (fn [& args]
    {:children args
     :value operator}))

; Let's not forget that we redefined + to be something else, so we have to use the old definition
(def +++ (expressionify +))

; We need to define what our objects are, so we can delegate to ++ if the given arguments are our objects
; or to old+ if not
(defn is-our-objects [n]
  (not (nil? (:value n))))

; So this works
(toString (+++ tree3 tree3 tree3 tree3))

; Now let's try something interesting
(def one {:value 1})

(defn increment [v]
  (if (is-our-objects v) (+++ v {:value 1})
    (+ v 1)))

(toString (increment one))

(toString (apply +++ (take 5 (repeat one))))

(defn sequence-from-deep [n f1 f2]
  (lazy-cat [n] (sequence-from (f1 n) (f2 f1) f2)))

(defn sequence-from [n f]
  (lazy-cat [n] (sequence-from (f n) f)))

; Now we can define natural numbers like so
(def nats (sequence-from 1 increment))

; Or like so
(def nats (sequence-from one increment))

(apply +++ (take 10 nats))

; Naive implementation of equality check that performs a breadth first search
; on a tree
(defn is-equal-helper [col1 col2]
  (cond
   (and (empty? col1) (empty? col2)) :true
   (or (empty? col1) (empty? col2)) :false
   (not (every? identity (map #(= (:value %1) (:value %2)) col1 col2))) :false
   :else (is-equal-helper
          (filter identity (flatten (map :children col1)))
          (filter identity (flatten (map :children col2))))))

(defn is-equal [t1 t2]
  (is-equal-helper [t1] [t2]))

; Now the problem is that expression trees aren't necessarily similar, but
; represent math that will give the same result. So we need a way to reduce
; an expression tree such that two trees yielding the same result will be
; represented the same way.