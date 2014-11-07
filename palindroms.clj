(defn group-by-palindroms [coll]
  (vals (group-by #(sort (list (hash %)
                               (hash (clojure.string/reverse %)))) coll)))

(group-by-palindroms ["abbbc", "abcd", "dcba", "abbbbc", "cbba", "cbbba", "abc", "cba"])