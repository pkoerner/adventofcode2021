
(def input (read-string (str \[ (slurp "input01") \])))

(defn find-increases [c]
  (filter (partial apply <) (partition 2 1 c)))
;;  part 1
(count (find-increases input))

;; part 2
(count (find-increases (map (partial apply +) (partition 3 1 input)))) 
