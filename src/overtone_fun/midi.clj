(ns overtone-fun.midi
  (:use [overtone.live]))

(comment
  (connected-midi-receivers)
  (connected-midi-devices)

  (event-debug-on)

  (event-debug-off)


  )

(defn first-kv
  "Find first element in coll where element's :k is v"
  [coll k v]
  (let [fn #(= v (get % k)) ]
    (->> coll
        (filter fn)
        (first))))

(def launch-out
  (midi-find-connected-receiver #"Launchpad"))




(on-event
 [:midi :note-on]
 (fn [e]
   (println (:note e)))
 ::lanchpad-input-handler)

(def w 8)
(def h 8)

(def tiles (atom (repeat (* w h) 0)))

(defn tile->midinote
  [n]
  (+ n (* 8 (int (/ n 8)))))

(defn show
  [tiles]
  (doseq [[v n] (map vector tiles (range))]
    (let [m (tile->midinote n)]
      (if (> v 0)
        (midi-note-on launch-out m v)
        (midi-note-off launch-out m)))))

(defn bar
  []
  (let [t (now)]
    (show (repeatedly (* w h) #(rand-int 128)))
    (apply-at (+ 500 t) #'bar)))
