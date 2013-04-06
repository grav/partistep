(ns overtone-fun.exp1
  (:use [overtone.live]
        [overtone.inst.synth])
  (:require [overtone-fun.util :as util]))

(defonce m (metronome 160))

(def melody (flatten (repeat (map #(+ 56 %) [1 4 1 8]))))

(defn play-notes
  [notes now]
  (util/beep (first notes))
  (when (not (empty? (rest notes)))
    (let [next (+ 0.5 now)]
      (apply-at (m next) #'play-notes [(rest notes) next]))))
