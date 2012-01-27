(ns pres01-concurrency.refs.refs)

(def accounts [(ref {:name :alice :balance 500})
               (ref {:name :bob :balance 300})
               (ref {:name :carol :balance 1000})
               (ref {:name :dave :balance 10000})])

(defn current-total
  []
  (reduce + (map (comp :balance deref) accounts)))

(def starting-total
  (current-total))

(defn deplete
  [account amount]
  (let [start (:balance account)]
    (conj account [:balance (- start amount)])))

(defn supplement
  [account amount]
  (let [start (:balance account)]
    (conj account [:balance (+ start amount)])))

(defn take-from
  [victim-name receiver-name]
  (let [name-index (zipmap (map (comp :name deref) accounts) (range))
        victim (accounts (name-index victim-name))
        receiver (accounts (name-index receiver-name))
        vbal (:balance @victim)
        rbal (:balance @receiver)
        amt (rand-int vbal)]
    (dosync
     (alter receiver supplement amt)
     (alter victim deplete amt))))

(defn rand-account-name
  []
  (:name @(accounts (rand-int (count accounts)))))

(defn rand-other-account-name
  [name]
  (let [others (vec (disj (set (map (comp :name deref) accounts)) name))]
    (others (rand-int (count others)))))

(time (dotimes [i 100000]
   (let [recipient (rand-account-name)]
     (take-from (rand-other-account-name recipient) recipient))))

(def synchronous-total
  (current-total))

(dotimes [i 4]
  (.start (Thread. (fn [] (dotimes [i 25000]
                            (let [recipient (rand-account-name)]
                              (take-from (rand-other-account-name recipient) recipient)))))))

(def asynchronous-total
  (current-total))

(println "Starting total: " starting-total)
(println "Total after 10000 sync transactions: " synchronous-total)
(println "Total after 10000 async transactions: " asynchronous-total)
