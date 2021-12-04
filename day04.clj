(def input 
  (let [[draws _ & t] (clojure.string/split-lines (slurp "input04"))]
    {:draws (read-string (str \[ draws \]))
     :boards (map butlast (partition 6 (map (fn [line] (read-string (str "[" line "]"))) t)))}))

(defn rows-and-col-sets [idx board]
  (let [transposed (apply map vector board)]
    (for [v (concat transposed board)]
      [(set v) idx])))

(def transformed-boards (into {} (apply concat (map-indexed (fn [idx board] (rows-and-col-sets idx board)) (:boards input)))))

(defn call-number [draws tboards]
  (loop [draws draws
         drawn []]
    (if-let [winner (some (fn [x] (clojure.set/subset? x (set drawn))) (keys tboards))]
      [(first (filter (fn [x] (clojure.set/subset? x (set drawn))) (keys tboards))) drawn]
      (recur (rest draws) (conj drawn (first draws))))))

(let [[winner drawn] (call-number (:draws input) transformed-boards)
      board-idx (transformed-boards winner)
      board (nth (:boards input) board-idx)
      unmarked (clojure.set/difference (set (apply concat board)) (set drawn)) ] 
  (* (reduce + unmarked) (last drawn)))


;; eh

(defn call-number2 [draws tboards]
  (loop [draws draws
         drawn []
         boards (set (range (count (:boards input))))
         lboards boards]
    (prn :boards boards :drawn drawn :lboards lboards)
    (if (empty? boards)
      [(first lboards) drawn]
      (recur (rest draws)
             (conj drawn (first draws))
             (clojure.set/difference boards (set (map tboards (filter (fn [x] (clojure.set/subset? x (set drawn))) (keys tboards)))))
             boards))))

(let [[board-idx drawn] (call-number2 (:draws input) transformed-boards)
      board (nth (:boards input) board-idx)
      unmarked (clojure.set/difference (set (apply concat board)) (set (butlast drawn))) ] 
  (* (reduce + unmarked) (last (butlast drawn))))
