(ns overtone-fun.util)

(defn take-rand [coll n]
  (take n (shuffle coll) ))

(defn p-val
  [[p v]]
  (when (= 1 p) v))


(defn infinite
  "return an infinite list of repeated evaluations of a function that returns
   the value of @a"
  [a]
  (->> (repeatedly (fn [] (lazy-seq @a)))
       (apply concat)))

(defn extend-vec
  "append 0 to v such that it has length n"
  [v n]
  (->>
   (concat v (repeat 0))
   (take n)
   (vec)))
