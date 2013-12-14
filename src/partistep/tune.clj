(ns overtone-fun.tune
  (:require [overtone.live :as live]))

(definst foo
  [freq 200]
  (live/saw freq))
(stop)
(foo 200)

;; just tuning (not equal/well-tempered)
;; from http://en.wikipedia.org/wiki/Just_intonation#Twelve_tone_scale
;; todo - figure out system
(def tones
  [[1 1] ;; c
   [9 8] ;; d
   [81 64] ;; e
   [4 3] ;; f
   [3 2] ;; g
   [27 16] ;; a
   [243 128]]) ;; b


(defn play
  [n]
  (let [o (int (/ n 7))
        n (mod n 7)
        [num den] (nth tones n)
        freq (* (Math/pow 2 o) 200 (/  num den))]
    (println "Freq:" (int freq) "Octave:" o)
    (ctl foo :freq freq)))

(defn play-inc
  [n]
  (let [t (now)]
    (at t (play n))
    (apply-at (+ 600 t) #'play-inc [(inc n)])))

#_(play-inc 0)
