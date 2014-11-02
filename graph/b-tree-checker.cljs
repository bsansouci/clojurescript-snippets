(def node2-1-l [{:val 4} {:val 5}])
(def node2-1-r [{:val 7} {:val 8}])
(def node2-2-l node2-1-r)
(def node2-2-r [{:val 10} {:val 11}])
(def node2-3-l node2-2-r)
(def node2-3-r [{:val 13} {:val 14}])

(def node1-l [{:val 1} {:val 2}])
(def node1-r [{:left node2-1-l
                 :right node2-1-r
                 :val 7}
                {:left node2-2-l
                 :right node2-2-r
                 :val 9}
                {:left node2-3-l
                 :right node2-3-r
                 :val 12}])

(def node2-l node1-r)
(def node2-r [{:val 16} {:val 17}])

(def node3-l node2-r)
(def node3-r [{:val 19} {:val 20}])

(def b-tree [{:left node1-l
             :right node1-r
             :val 3}
            {:left node2-l
             :right node2-r
             :val 15}
            {:left node3-l
             :right node3-r
             :val 18}])


; At every level we cal this function that will check that the current node
; and its children are properly ordered
(defn b-tree-check-level [n]
  (let [cur (:val n)]
    (every? identity (map #(and (<= (:val %1) cur)
                                (>= (:val %2) cur))
                          (:left n)
                          (:right n)))))

(defn b-tree-check [t]
  (and (every? identity (map b-tree-check-level t))
       (every? identity (map b-tree-check (concat (filter #(:left %) t)
                                                  (filter #(:right %) t))))))

(b-tree-check b-tree)
