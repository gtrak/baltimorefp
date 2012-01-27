(ns pres01-concurrency.refs.refs)

(def accounts [(ref {:name :alice :balance 500})
               (ref {:name :bob :balance 300})
               (ref {:name :carol :balance 1000})
               (ref {:name :dave :balance 10000})])

(defn current-total
  []
  (reduce + (map (comp :balance deref) accounts)))

(defn negative-balances
  []
  (count (filter (comp (partial > 0) :balance deref) accounts)))

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
  (dosync (let [name-index (zipmap (map (comp :name deref) accounts) (range))
                victim (accounts (name-index victim-name))
                receiver (accounts (name-index receiver-name))
                vbal (:balance @victim)
                rbal (:balance @receiver)
                amt (rand-int vbal)]
            (Thread/sleep 3)
            (alter receiver supplement amt)
            (Thread/sleep 3)
            (alter victim deplete amt))))

(defn rand-account-name
  []
  (:name @(accounts (rand-int (count accounts)))))

(defn rand-other-account-name
  [name]
  (let [others (vec (disj (set (map (comp :name deref) accounts)) name))]
    (others (rand-int (count others)))))

(def starting-total
  (current-total))

(def starting-negative-balances
  (negative-balances))

(time (dotimes [i 100]
   (let [recipient (rand-account-name)]
     (take-from (rand-other-account-name recipient) recipient))))

(def synchronous-total
  (current-total))

(def synchronous-negative-account-balances
  (negative-balances))

(let [threads (vec (for [i (range 4)]
                     (Thread. (fn []
                                (dotimes [j 25]
                                  (let [recipient (rand-account-name)]
                                    (take-from (rand-other-account-name recipient) recipient)))))))]
  (dotimes [i 4]
    (.start (threads i)))
  (dotimes [i 4]
    (.join (threads i))))

(def asynchronous-total
  (current-total))

(def asynchronous-negative-account-balances
  (negative-balances))

(println "Starting total: " starting-total)
(println "Starting negative balances: " starting-negative-balances)
(println "Total after 100 sync transactions: " synchronous-total)
(println "Accounts with a negative balance after synch: " synchronous-negative-account-balances)
(println "Total after 100 async transactions: " asynchronous-total)
(println "Accounts with a negative balance after async: " asynchronous-negative-account-balances)
