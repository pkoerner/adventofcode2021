(def input (read-string (str \[ (slurp "input06") \])))

(defn next-gen [x]
  (if (zero? x)
    [6 8]
    [(dec x)]))

(count (last (take 81 (iterate (partial mapcat next-gen) input))))

;; part 2
(def fishmap-start (frequencies input))

(defn next-gen-fast [[state n]]
  (if (zero? state)
    {6 n 8 n}
    {(dec state) n}))

(defn entire-next-gen [m]
  (apply merge-with + (map next-gen-fast m)))

(reduce + (vals (last (take 257 (iterate entire-next-gen fishmap-start)))))
