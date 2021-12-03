(def input (clojure.string/split-lines (slurp "input03")))

(def bits (map (comp key (partial apply max-key val) frequencies) (apply map vector input)))

(defn bits->dec 
  ([bits] (bits->dec bits 0 1))
  ([bits acc power]
   (if (seq bits)
     (recur (rest bits)
            (+ acc (if (= \1 (first bits)) power 0))
            (* 2 power))
     acc)))

(* (bits->dec (reverse bits)) 
   (bits->dec (map {\0 \1, \1 \0} (reverse bits))))

(defn calc-val [bitlist f pos]
  (if (= 1 (count bitlist))
    (first bitlist)
    (let [freqs (frequencies (map #(nth % pos) bitlist))]
      (if (f (get freqs \0 0) (get freqs \1 0))
          (recur (remove (fn [bits] (= \0 (nth bits pos))) bitlist) f (inc pos))
          (recur (remove (fn [bits] (= \1 (nth bits pos))) bitlist) f (inc pos))))))

(* (bits->dec (reverse (calc-val input <= 0)))
   (bits->dec (reverse (calc-val input > 0))))
