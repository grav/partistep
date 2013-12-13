(ns overtone-fun.old
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
         fs (map #(* % f) (range 1 9 (/ 3 5) ))
         partial-vols [p1 p2 p3 p4 p5 p6 p7 p8]
         partial-vols-norm (-> (apply + partial-vols))
         partials (->> partial-vols
                       ;; todo - change range to something more interesting
;                       (map vector fs)
                       (map vector (iterate #(* % (/ (+ 1 (Math/sqrt 9)) 2)) f))
                       )
         oscs (for [[pf v] partials]
                (* v (sin-osc pf)))]
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

 ;; here, we just deref our atoms
 (defn make-notes
   [] @notes)

 (defn make-partials
   [] @partials)

(defn infinite
  "return an infinite list of repeated evaluations of a function that returns
   the value of @a"
  [a]
  (->> (repeatedly (fn [] (lazy-seq @a)))
       (apply concat)))

(comment
  ;; === Let's have some sound
  ;; using #'make-partials allows us to redefine it (below)

  (foo (apply concat (repeatedly make-notes)) (apply concat (repeatedly #'make-partials)))

  ;; === Let's not
  (stop)
  ;; apply a function on existing params
  (swap! notes reverse)

  ;; or set them to new values
  ;; wow - polyrhythm
  (reset! partials [[1 0 0.1 1]
                    [1 0.3 0.4 0]
                    [0 1.0 0.5 0]])

  ;; we don't need to use constants. How about like randomness?
  (defn make-partials
    []
    [(map #(* % 0.5 (rand)) (repeat 4 1))])


  )
