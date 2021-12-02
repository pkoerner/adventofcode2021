(def input (partition 2 (read-string (str \[ (slurp "input02") \]))))

(defmulti process-cmd (fn [state cmd] (first cmd)))
(defmethod process-cmd 'forward [state [_ n]]
  (update state :horizontal + n))
(defmethod process-cmd 'up [state [_ n]]
  (update state :vertical - n))
(defmethod process-cmd 'down [state [_ n]]
  (update state :vertical + n))

(apply * (vals (reduce process-cmd {:horizontal 0, :vertical 0} input)))

(defmulti process-cmd2 (fn [state cmd] (first cmd)))
(defmethod process-cmd2 'forward [state [_ n]]
  (-> state
    (update :horizontal + n)
    (update :vertical + (* (:aim state) n))))
(defmethod process-cmd2 'up [state [_ n]]
  (update state :aim - n))
(defmethod process-cmd2 'down [state [_ n]]
  (update state :aim + n))

(apply * (vals (select-keys (reduce process-cmd2 {:horizontal 0, :vertical 0, :aim 0} input) [:horizontal :vertical])))
