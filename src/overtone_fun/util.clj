(ns overtone-fun.util
    (:use [overtone.live]
          [overtone.inst.synth]
;          [clojure.core.async]
          ))

(defn take-rand [coll n]
  (take n (shuffle coll) ))

(defn p-val
  [[p v]]
  (when (= 1 p) v))

(definst beep-partial
  [note 60 p1 1 p2 0 p3 0 p4 0 p5 0 p6 0 p7 0 p8 0]
  (*
   (let [f (midicps note)

         partials (->> [p1 p2 p3 p4 p5 p6 p7 p8]
                       ;; todo - change range to something more interesting
                       (map vector (range 1 9)))
         oscs (for [[p v] partials]
                (* v (sin-osc (* f p))))]
        (apply + oscs))
   (env-gen (perc 0.01 0.35) :action FREE)))

(defn foo
  []
  (let [t (now)]
    (at t (beep-partial))
    (apply-at (+ 1000 t) #'foo [])))

(beep-partial 60 1 1 0 1)
