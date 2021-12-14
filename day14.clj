(def input (clojure.string/split-lines (slurp "input14")))

(def start (first input))
(def rules (into {} (map (fn [x] [(take 2 x) (last x)]) (drop 2 input))))

(defn step [cs]
  (mapcat (fn [x] (if (contains? rules x)
                    [(first x) (get rules x)]
                    [(first x)]))
          (partition 2 1 (concat cs [\Z]))))

(let [fs (frequencies (nth (iterate step start) 10))]
  (- (reduce max (vals fs)) (reduce min (vals fs))))

(defn stepp [fs]
  (apply merge-with +
         (for [[[x y] n] fs]
           (if (contains? rules [x y])
             (let [c (get rules [x y])]
               {[x c] n, [c y] n})
             {[x y] n}))))

(def start2 (frequencies (partition 2 1 (concat start [\Z]))))  

(defn summarize [fs]
  (apply merge-with +
         (for [[[x y] n] fs]
           {x n})))

(let [freqs (reverse (sort-by second (dissoc (summarize (nth (iterate stepp start2) 40)) \Z)))]
  (- (second (first freqs)) (second (last freqs))))
