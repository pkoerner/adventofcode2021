(use 'clojure.repl)
(def input (map (fn [line] (map #(clojure.string/split (clojure.string/trim %) #" ") (clojure.string/split line #"\|"))) (clojure.string/split-lines (slurp "input08"))))
(count (filter (comp #{2 4 7 3} count) (mapcat second input)))

(defn only [x]
  (assert (= (count x) 1))
  (first x))

(defn label [segments]
  (let [one (only (filter #(= (count %) 2) segments))
        seven (only (filter #(= (count %) 3) segments))
        segment-a (only (clojure.set/difference (set seven) (set one)))
        four (only (filter #(= (count %) 4) segments))
        eight (only (filter #(= (count %) 7) segments))
        six (only (filter #(and (not (clojure.set/subset? (set one) (set %))) (= (count %) 6)) segments))
        segment-c (only (clojure.set/difference (set one) (set six)))
        segment-f (only (clojure.set/intersection (set one) (set six)))
        five (only (filter #(and (not (contains? (set %) segment-c)) (= (count %) 5)) segments))
        two-or-three (remove #(or (not (contains? (set %) segment-c)) (not= (count %) 5)) segments)
        segment-b (only (clojure.set/difference (set five) (apply clojure.set/union (map set two-or-three))))
        zero-or-nine (remove #(or (not (clojure.set/subset? (set one) (set %))) (not= (count %) 6)) segments)
        three (only (filter #((set %) segment-f) two-or-three))
        two (only (remove #((set %) segment-f) two-or-three))
        segment-e (only (clojure.set/difference (set two) (set three)))
        zero (only (filter #(contains? % segment-e) (map set zero-or-nine)))
        segment-g (only (clojure.set/difference (set zero) #{segment-a segment-b segment-c segment-e segment-f}))
        segment-d (only (clojure.set/difference (set eight) #{segment-a segment-b segment-c segment-e segment-f segment-g}))]
  {segment-a :a
   segment-b :b
   segment-c :c
   segment-d :d
   segment-e :e
   segment-f :f
   segment-g :g}))

(def segments->digit {#{:a :b :c :e :f :g} 0
                      #{:c :f} 1
                      #{:a :c :d :e :g} 2
                      #{:a :c :d :f :g} 3
                      #{:b :c :d :f} 4
                      #{:a :b :d :f :g} 5
                      #{:a :b :d :e :f :g} 6
                      #{:a :c :f} 7
                      #{:a :b :c :d :e :f :g} 8
                      #{:a :b :c :d :f :g} 9})

(defn digits->num [[a b c d]]
  (+ (* 1000 a) (* 100 b) (* 10 c) d))

(reduce + (map (fn [[segments numbers]] (digits->num (map (comp segments->digit set (partial map (label segments)) set) numbers))) input ))
