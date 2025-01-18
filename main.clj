
;; twos-difference exercise
(defn get-next-number
  [number coll]
  (let [next-number (+ number 2)]
    (some->> coll
             (some #{next-number})
             (conj [number]))))

(defn  twos-difference
  [coll]
  (->> coll
       sort
       (map #(get-next-number % coll))
       (remove nil?)))

(twos-difference [4 2 3 1])
;; ------------------------------------

(defn lucasnum [n]
  (loop [n n
         a 2M
         b 1M]
    (cond
      (= n 0) a
      (= n 1) b
      (pos? n) (recur (dec n) b (+' a b))
      :else (recur (inc n) (-' b a) a))))

(lucasnum 100)

(defn lucasnum-pos [a b]
  (lazy-seq (cons a (lucasnum-pos b (+' a b)))))

(defn lucasnum-2 [n]
  (when (pos? n)
    (nth (lucasnum-pos 2 1) n)))

(lucasnum-2 100)

(time (lucasnum 100))

(time (lucasnum-2 100))

;; almost the same performance

;; -------------------------------------------------

(defn num-str->digits
  [num-str]
  (Character/digit num-str 10))

(defn order-weight [strng]
  (->> #" "
       (clojure.string/split strng)
       sort
       (sort #(< (reduce + (map num-str->digits %1))
                 (reduce + (map num-str->digits %2))))
       (clojure.string/join " ")))

(order-weight "103 123 4444 99 2000")

(order-weight "2000 10003 1234000 44444444 9999 11 11 22 123")

;; -------------------------------------------------

(defn spin-words [strng]
  (clojure.string/replace strng #"\b\w{5,}\b"  clojure.string/reverse))

;; -------------------------------------------------

(defn clean-string [s]
  (->> s
       (iterate #(clojure.string/replace % #"(?:^|[^#])#" ""))
       (drop-while #(clojure.string/includes? % "#"))
       first))

(clean-string "abc#d##c")

(clean-string "abc####d##c#")

;; -------------------------------------------------
