(ns overtone-fun.util
    (:use [overtone.live]
          [overtone.inst.synth]))

(definst beep [note 60]
  (* (sin-osc (midicps note))
     (env-gen (perc 0.05 0.15) :action FREE)))
