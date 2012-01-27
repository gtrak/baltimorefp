(ns pres01-concurrency.refs.ohno)

(def ^"[Lclojure.lang.PersistentHashMap;"
  accounts (into-array [{:name :alice :balance 500}
                        {:name :bob :balance 300}
                        {:name :carol :balance 1000}
                        {:name :dave :balance 10000}]))

(defn current-total
  []
  (reduce + (map :balance accounts)))

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
  (let [name-index (zipmap (map :name accounts) (range))
        victim (get accounts (name-index victim-name))
        receiver (get accounts (name-index receiver-name))
        vbal (:balance victim)
        rbal (:balance receiver)
        amt (rand-int vbal)]
    (aset accounts (name-index receiver-name) (supplement receiver amt))
    (aset accounts (name-index victim-name) (deplete victim amt))))-

(defn rand-account-name
  []
  (:name (get accounts (rand-int (count accounts)))))

(defn rand-other-account-name
  [name]
  (let [others (vec (disj (set (map :name accounts)) name))]
    (others (rand-int (count others)))))

(time (dotimes [i 10000]
   (let [recipient (rand-account-name)]
     (take-from (rand-other-account-name recipient) recipient))))

(def synchronous-total
  (current-total))

(dotimes [i 4]
  (.start (Thread. (fn [] (dotimes [i 2500]
                            (let [recipient (rand-account-name)]
                              (take-from (rand-other-account-name recipient) recipient)))))))

(def asynchronous-total
  (current-total))

(println "Starting total: " starting-total)
(println "Total after 10000 sync transactions: " synchronous-total)
(println "Total after 10000 async transactions: " asynchronous-total)
