(def input (into {} (apply concat (map-indexed (fn [idx xs] (map-indexed (fn [idx2 y] [{:x idx2, :y idx} (read-string (str y))]) xs)) (clojure.string/split-lines (slurp "input11"))))))

(def size 10)

(defn neighbours [coords]
  (for [x (range (max 0 (dec (:x coords)))
                 (min size (+ 2 (:x coords))))
        y (range (max 0 (dec (:y coords)))
                 (min size (+ 2 (:y coords))))
        :when (or (not= x (:x coords)) (not= y (:y coords)))]
    {:x x, :y y}))

(defn increase [grid]
  (loop [grid grid
         acts (keys (:grid grid))]
    (if (seq acts)
      (let [grid' (update-in grid [:grid (first acts)] inc)]
        (recur grid' (if (= (get-in grid' [:grid (first acts)]) 10) 
                       (do #_(println :flash (first acts)) (concat acts (neighbours (first acts))))
                       (rest acts))))
      grid)))

(defn reset-to-zero [{:keys [grid flashes]}]
  {:grid (into {} (map (fn [[x y]] [x (if (<= 10 y) 0 y)]) grid))
   :flashes (+ flashes (count (filter #(<= 10 %) (vals grid))))})

(def step (comp #_(fn [x] (pprint x) x) reset-to-zero increase))

(defn pprint [{:keys [flashes grid]}]
  (println :flashes flashes)
  (doseq [y (range size)]
    (doseq [x (range size)]
      (print (get grid {:x x :y y})))
    (newline)))

(map :flashes (take 101 (iterate step {:grid input, :flashes 0})))

(loop [x {:grid input, :flashes 0}
       n 0]
  (if (= #{0} (set (vals (:grid x))))
    n
    (recur (step x) (inc n))))

