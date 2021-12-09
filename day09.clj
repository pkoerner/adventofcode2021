(def input (map (fn [x] (map int x)) (clojure.string/split-lines (slurp "input09")))) 
(defn extend-input [in]
  (let [n (count (first in))]
    (concat [(repeat (+ n 2) 99)] 
            (map (fn [line] (concat [99] line [99])) in)
            [(repeat (+ n 2) 99)])))

(def coords-input (map-indexed (fn [y xs] (map-indexed (fn [x v] {:x x :y y :v v}) xs)) (extend-input input)))
(def low-x (set (mapcat (fn [xs] (map second (filter (fn [[a b c]] (and (< (:v b) (:v a)) (< (:v b) (:v c)))) (partition 3 1 xs)))) coords-input)))
(def low-y (set (mapcat (fn [xs] (map second (filter (fn [[a b c]] (and (< (:v b) (:v a)) (< (:v b) (:v c)))) (partition 3 1 xs)))) (apply map vector coords-input))))
(def lows (clojure.set/intersection low-x low-y))
(reduce + (map (comp inc (fn [x] (- x (int \0))) :v) lows))



(defn graphify [coords-input]
  (let [graph-horizontal (mapcat (comp (partial map (partial sort-by :v)) (partial filter (fn [[x y]] (and (< (:v x) (int \9)) (< (:v y) (int \9))))) (partial partition 2 1)) coords-input)
        graph-vertical   (mapcat (comp (partial map (partial sort-by :v)) (partial filter (fn [[x y]] (and (< (:v x) (int \9)) (< (:v y) (int \9))))) (partial partition 2 1)) (apply map vector coords-input))]
    (into {} (map (fn [[x y]] [x (set (map second y))]) (group-by first (concat graph-horizontal graph-vertical))))))

(sort-by :v '({:x 1, :y 4, :v 56} {:x 1, :y 5, :v 55}))

(defn extract-basin [g pos]
  (loop [basin []
         [p & q :as qq] [pos]]
    (if (empty? qq)
      basin
      (recur (conj basin p) (concat q (get g p))))))
(apply * (map (fn [x] (count (map (comp #(- % (int \0)) :v) x))) (take 3 (reverse (sort-by count (map set (map (partial extract-basin (graphify coords-input)) lows)))))))
