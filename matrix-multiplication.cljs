(ns matrix-mult.example)

(defn transpose [coll]
  (into []
        (apply (partial map vector)
               coll)))

(defn map-diagonal
  ([coll] (map-indexed #(nth %2 %1) coll))
  ([coll f] (map f (map-diagonal coll))))

(defn mmult [a b]
  (cond (empty? a) nil
        (not= (count (first a)) (count b)) nil
        :else (map (fn [x1] (map (fn [x2] (apply + (map * x1 x2))) a))
                   (transpose b))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mmult [[1 2] [3 4]] [[1 2] [3 4]])