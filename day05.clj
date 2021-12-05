(defn extract-data [s]
  (map read-string (rest (first (re-seq #"([0-9]+),([0-9]+) -> ([0-9]+),([0-9]+)" s)))))

(def input (map extract-data (clojure.string/split-lines (slurp "input05"))))

(def vertical-horizontal-points (for [[a b c d] (filter (fn [[a b c d]] (or (= a c) (= b d))) input)
                                      x (range (if (< a c) a c) (inc (if (< a c) c a)))
                                      y (range (if (< b d) b d) (inc (if (< b d) d b)))]
                                  [x y]))

(count (remove (fn [[_ y]] (= y 1)) (frequencies vertical-horizontal-points)))

(def diagonal-points 
  (mapcat (fn [[a b c d]]
            (let [f1 (if (< a c) + -)
                  f2 (if (< b d) + -)
                  signed-len (- a c)
                  len (Math/abs signed-len)]
              (for [n (range (inc len))]
                [(f1 a n) (f2 b n)])))
          (remove (fn [[a b c d]] (or (= a c) (= b d))) input)))

(count (remove (fn [[_ y]] (= y 1)) (frequencies (concat diagonal-points vertical-horizontal-points))))
