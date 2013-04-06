(ns overtone-fun.exp1
  (:use [overtone.live]
        [overtone.inst.synth])
  (:require [overtone-fun.util :as util])
  (:import [java.lang Math]))

(defonce m (metronome 120))

(def melody (flatten (repeat (map #(+ 40 %) [1 3 6 2]))))

(defn play-notes
  [notes now]
  (let [n (first notes)]
    (at (m now)
        (util/beep n))
    (augment n 0 now)
    (augment n 1 now)
;;    (augment n 2 now)
    (augment n 3 now)
    (augment n 4 now)
;;    (augment n 5 now)
    (augment n 6 now)
    (augment n 7 now))
  (when (not (empty? (rest notes)))
    (let
[next (inc now)]
      (apply-at (m next) #'play-notes [(rest notes) next]))))

(def augmentation [12 7 5 4 3 2 1])

(defn augment [n i b]
  (let [a (apply + (take (inc i) augmentation))
        n (+ n a)
        d (apply +
                 (for [j (range (inc i))]
                   (/ 1 (Math/pow 2 (inc j)))))
        b2 (+ b d)]
    (println b2)
    (at (m b2) (util/beep n))))
