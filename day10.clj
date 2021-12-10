(def input (clojure.string/split-lines (slurp "input10")))

(def matching {\( \), \[ \], \{, \}, \< \>})

(defn find-syntax-err [line]
  (loop [[c :as line] line
         stack ()]
    (if (seq line)
      (if (get (set (keys matching)) c) 
        (recur (rest line) (conj stack c)) ;; opening character
        (if (= (get matching (first stack)) c) ;; closing charcter
          (recur (rest line) (rest stack))
          c))
      (map matching stack))))

(def scores {\) 3, \] 57, \} 1197, \> 25137})
(reduce + (map scores (remove seq? (keep find-syntax-err input))))


(def scores2 {\) 1, \] 2, \} 3, \> 4})
(let [scores (sort (map (fn [x] (reduce (fn [a c] (+ (* a 5) (get scores2 c))) 0 x)) (filter seq? (keep find-syntax-err input))))
      len (count scores)]
  (nth scores (quot len 2)))
