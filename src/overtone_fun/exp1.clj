(ns overtone-fun.exp1
  (:use [overtone.live]
        [overtone.inst.synth])
  (:require [overtone-fun.util :as util])
  (:import [java.lang Math]))

(defonce m (metronome 120))

(def melody (flatten (repeat (map #(+ 55 %) [1 3 6 2]))))

(defn play-notes
  [notes now]
  (let [n (first notes)]
    (at (m now)
        (util/beep-partial n 1)))
  (when (not (empty? (rest notes)))
    (let [next (inc now)]
      (apply-at (m next) #'play-notes [(rest notes) next]))))


#_(defn augment [n i b]
  (let [a (apply + (take (inc i) augmentation))
        n (+ n a)
        d (apply +
                 (for [j (range (inc i))]
                   (/ 1 (Math/pow 2 (inc j)))))
        b2 (+ b d)]
    (println b2)
    (at (m b2) (util/beep n))))

(defcgen bell-partials
  "Bell partial generator"
  [freq {:default 440 :doc "The fundamental frequency for the partials"}
   dur  {:default 1.0 :doc "Duration multiplier. Length of longest partial will
                            be dur seconds"}
   partials {:default [0.5 1 2 4] :doc "sequence of frequencies which are
                                        multiples of freq"}]
  "Generates a series of progressively shorter and quieter enveloped sine waves
  for each of the partials specified. The length of the envolope is proportional
  to dur and the fundamental frequency is specified with freq."
  (:ar
   (apply +
          (map
           (fn [partial proportion]
             (let [env      (env-gen (perc 0.01 (* dur proportion)))
                   vol      (/ proportion 2)
                   overtone (* partial freq)]
               (* env vol (sin-osc overtone))))
           partials ;; current partial
           (iterate #(/ % 2) 1.0)  ;; proportions (1.0  0.5 0.25)  etc
           ))))

(bell-partials)
(util/beep-partial)
