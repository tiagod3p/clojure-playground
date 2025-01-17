;; future behavior with exceptions (there is a way to handle it using structured concurrency from java 21)
(defn get-a
  []
  (Thread/sleep 5000)
  "a")

(defn get-b
  []
  (Thread/sleep 1000)
  (throw (Exception. "my exception message"))
  "b")

(with-out-str
  (time
   (try
     (let [a (get-a)
           b (get-b)]
       [a b])
     (catch Exception _
       nil))))

(with-out-str
  (time
   (let [a (future (get-a))
         b (future (get-b))]
     (try
       [@a @b]
       (catch Exception _ nil)))))

;; ----------------------------------------------------

;;testing lazy seq

(->> '(1 2 3)
     (map (fn [a] (println a) a))
     (map (fn [a] (println a) a)))

(->> [1 2 3]
     (map (fn [a] (println a) a))
     (map (fn [a] (println a) a)))

(macroexpand-1 '(->> [1 2 3 4]
                     (filter odd?)
                     (map inc)))

(macroexpand-1 '(map inc (filter odd? [1 2 3 4])))

;; transducers

(def xf (comp (map inc)
              (filter #(<= 3 %))))

(def a [1 2 3 4 5 6])

(transduce xf + a)

(into [] xf a)

(sequence xf a)

;; watcher using atoms and promise
(import '[java.util.concurrent ConcurrentHashMap])

(def this (atom {}))

(defn start!
  []
  (swap! this assoc :state (new ConcurrentHashMap)))

(defn stop!
  []
  (swap! this dissoc :state))

(defn notify!
  [this
   id]
  (let [state (:state @this)]
    (when-let [p (.get state id)]
      (deliver p {:status :ok})
      (.remove state id))))

(defn get-promise-value!
  [this
   id
   timeout-ms]
  (let [state (:state @this)
        p     (promise)]
    (.put state id p)
    (or (deref p timeout-ms nil)
        (do
          (.remove state id)
          {:status :error}))))

(def id (random-uuid))

(start!)

this

(.start (Thread. (fn []
                   (println (get-promise-value! this id 8000))
                   )))

(.start (Thread. (fn []
                   (notify! this id)
                   )))
