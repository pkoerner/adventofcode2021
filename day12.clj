(def input (into {} (map (fn [[k v]] [k (map second v)]) (group-by first (mapcat (fn [[x y]] [[x y] [y x]]) (map #(clojure.string/split % #"-") (clojure.string/split-lines (slurp "input12"))))))))

(defn is-small-cave? [s]
  (or (= (clojure.string/lower-case s) s)))

(defn search [g]
  (loop [[p :as q] [(list "start")]
         done #{}]
    (if (seq q)
      (if (= (first p) "end")
        (recur (rest q) (conj done p))
        (recur (into (rest q) (map #(cons % p) (remove (set (filter is-small-cave? p)) (get g (first p))))) done))
      done)))

(count (search input))

(defn small-cave-visited? [p]
  (get (set (vals (frequencies (filter is-small-cave? p)))) 2))

(defn search' [g]
  (loop [[p :as q] [(list "start")]
         done #{}]
    (if (seq q)
      (if (= (first p) "end")
        (recur (rest q) (conj done p))
        (recur (into (rest q) (map #(cons % p) (remove #(or (= "start" %) (and (is-small-cave? %) ((set p) %) (small-cave-visited? p))) (get g (first p))))) done))
      done)))

(count (search' input))
