(defn pipe [data & args]
  (cond (= (count args) 0) data
    :else (let [f (first args)
                r (rest args)]
            (apply pipe (concat (list (f data)) r)))))

(def data [0 1 2 3 4 5 6 7 8 9])
(pipe data
      (partial map inc)
      (partial filter (fn [x] (zero? (mod x 2))))
      (partial map #(* % 2)))