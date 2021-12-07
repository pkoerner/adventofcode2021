;; brute force is good enough (takes a few secs)
;; more clever solutions are likely to be found if you have to time to think about
(def input (read-string (str \[ (slurp "input07") \])))
(def choices (for [x (range 0 (inc (reduce max input)))]
               [x (reduce + (map (fn [y] (Math/abs (- x y))) input))]))
(sort-by second choices)


(defn gaussian-sum [n]
  (/ (* n (inc n)) 2))

(def choices2 (for [x (range 0 (inc (reduce max input)))]
               [x (reduce + (map (fn [y] (gaussian-sum (Math/abs (- x y)))) input))]))
(sort-by second choices2)
