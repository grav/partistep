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
                       (map vector (range 1 9 0.5)))
         oscs (for [[p v] partials]
                (* v (sin-osc (* f p))))]
        (apply + oscs))
   (env-gen (perc 0.01 0.35) :action FREE)))

(defn foo
  [notes partials]
  (let [t (now)
        args (cons (first notes) (first partials))]
    (apply beep-partial args)
    (apply-at (+ 500 t) #'foo [(rest notes) (rest partials)])))



;; ==== Using atoms, we can dynamically set the params

;; atoms with defaults

(def notes (atom [60 62 65 67]))

(def partials (atom [[1 0 0.1 0]
                     [1 0.3 0 0]
                     [1 0 0.5 0]
                     [1 0 0 0.2]]))

;; either set the notes directly
(reset! notes [60 62 65 67])
;; or apply a function on existing notes
(swap! notes reverse)

;; wow - polyrhythm
(reset! partials [[1 0 0.1 1]
                  [1 0.3 0.4 0]
                  [0 1.0 0.5 0]])

;; here, we just deref our atoms
(defn make-notes
  [] @notes)

(defn make-partials
  [] @partials)

;; but we can also do other stuff like randomness
(defn make-partials
  []
  [(map #(* % 0.5 (rand)) (repeat 4 1))])

;; === Let's have some sound
; (foo (apply concat (repeatedly make-notes)) (apply concat (repeatedly make-partials)))

;; === Let's not
; (stop)
