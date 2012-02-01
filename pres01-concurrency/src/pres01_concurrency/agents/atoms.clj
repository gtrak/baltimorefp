(ns pres01-concurrency.agents.atoms)

(def accounts (atom []))

(defn log-emitter
  [i]
  (let [message (str "Log message from " i)]
   (fn []
     [(System/currentTimeMillis) message])))

(defn slow-conj [coll e]
  (Thread/sleep 1)
  (conj coll e))

(defn append-log
  [log-emitter]
  (swap! accounts slow-conj (log-emitter)))

(defn unique-timestamps
  []
  (count (sort (set (map first @accounts)))))

(defn average-logs-per-milli
  []
  (let [milli-histogram (reduce conj {} (for [entry (group-by first @accounts)]
                                          [(first entry) (count (second entry))]))
        millis-represented (count (keys milli-histogram))]
    (float (/ (reduce + 0 (vals milli-histogram)) millis-represented))))

(time
 (let [emitter (log-emitter -1)]
   (dotimes [i 1000]
     (append-log emitter))))

(def log-count-synchronous
  (count @accounts))

(def unique-timestamps-synchronous
  (unique-timestamps))

(def average-logs-per-milli-synchronous
  (average-logs-per-milli))

(reset! accounts [])

(time
 (let [emitters (vec (map log-emitter (range 4)))
       threads (vec (for [i (range 4)]
                  (Thread. (fn [] (dotimes [j 250]
                                    (append-log (emitters i)))))))]
   (dotimes [i 4]
     (.start (threads i)))
   (dotimes [i 4]
     (.join (threads i)))))

(def log-count-asynchronous
  (count @accounts))

(def unique-timestamps-asynchronous
  (unique-timestamps))

(def average-logs-per-milli-asynchronous
  (average-logs-per-milli))

(println "Synchronous log count: " log-count-synchronous)
(println "Asynchronous log count: " log-count-asynchronous)
(println "Synchronous unique timestamps: " unique-timestamps-synchronous)
(println "Asynchronous unique timestamps: " unique-timestamps-asynchronous)
(println "Synchronous average logs per millisecond: " average-logs-per-milli-synchronous)
(println "Asynchronous average logs per millisecond: " average-logs-per-milli-asynchronous)
