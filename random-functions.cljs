(ns random-functions
  (:require [clojure.string :as string]))

(defn factorial [n]
  (->> n (+ 1) range rest (apply *)))

(factorial 4)
