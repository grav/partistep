(ns overtone-fun.synth
    (:use [overtone.live]
          [overtone.inst.synth]))

(definst beep-partial
  [note 60 p1 1 p2 0 p3 0 p4 0 p5 0 p6 0 p7 0 p8 0]
  (*
   (let [add-half #(+ % (* % (/ 1 2))) ;; these must be defined in the definst macro
         double #(* % 2)
         f (midicps note)
         fs (map #(* % f) (range 1 9 (/ 3 5) ))
         partial-vols [p1 p2 p3 p4 p5 p6 p7 p8]
         norm (-> (apply + partial-vols)
                  (/ 8))
         partials (->> (* partial-vols norm)
                       (map vector (iterate double f))
                       )
         oscs (for [[pf v] partials]
                (* v (sin-osc pf)))]
     (apply + oscs))
   (env-gen (perc 0.01 0.35) :action FREE)))
