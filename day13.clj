(def input (partition-by empty? (clojure.string/split-lines (slurp "input13"))))
(def coords (set (map (comp (partial zipmap [:x :y]) #(map read-string (clojure.string/split %  #"\,"))) (first input))))
(def folds (map (comp (fn [[x y]] [(keyword x) (read-string y)]) rest first #(re-seq #"fold along (.)=([0-9]+)" %)) (nth input 2)))

(defn fold [coords [axis v]]
  (set (map (fn [x] (if (= axis :x)
                      (update x :x #(if (< % v) % (- v (- % v))))
                      (update x :y #(if (< % v) % (- v (- % v))))))
            coords)))


(count (fold coords (first folds)))

(defn print! [coords]
  (doseq [y (range (inc (reduce max (map :y coords))))]
    (doseq [x (range (inc (reduce max (map :x coords))))]
      (if (coords {:x x, :y y})
        (print \#)
        (print \.)))
    (println)))
    
(print! (reduce fold coords folds)) 
