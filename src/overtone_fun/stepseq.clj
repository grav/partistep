(ns overtone-fun.stepseq
  (:require [overtone.live :as overtone :refer :all]
            [overtone-fun.launchpad :as l]
            [overtone-fun.synth :as s]
            [overtone-fun.util :as u]
            [overtone.music.pitch :as p]))

;; ==============
;;   SEQUENCER
;; ==============
;; This is a step sequencer that can step through
;; melody and the partials.
;; The number of melody steps and partial steps
;; does not have to be equal. This can be used
;; for making polyrhythmic melodies.


(def partials (atom nil))

(def melody (atom nil))

(def mode (atom nil))

(def val->note [:i :ii :iii :iv :v :vi :vii :i])

(defn tile-conf-nocache [s]
  (-> (for [i (take 8 (iterate dec 8))]
        (map #(if (= i %) l/green2 0) s))
      (flatten)))


(def partial-vol-tbl
  {0.0 :silent
   0.1 :very-low
   0.3 :low
   0.5 :medium
   1.0 :high})

(def partial-vol-list
  [:silent :very-low :low :medium :high])

(defn next-partial-vol [vol]
  (let [idx (->> (map vector partial-vol-list (range))
                (filter #(= (first %) vol))
                (first)
                (second))
        next-idx (-> (inc idx)
                     (mod (count partial-vol-list)))]
    (get partial-vol-list next-idx)))

(defn partial-vol
  [vol]
  (let [ks (filter #(>= vol %) (keys partial-vol-tbl))
        k (last (sort ks))]
    (get partial-vol-tbl k)))

(def partial-col
  {:silent 0
   :very-low l/green1
   :low (bit-or l/green1 l/red1)
   :medium (bit-or l/green2 l/red1)
   :high l/red2})

(defn tile-conf-partials [ps]
  (->> (let [ps (concat ps
                        (repeat (- 8 (count ps))
                                [0 0 0 0 ]))]
         (for [i [3 2 1 0]]
           (for [partials ps]
             (get partials i))))
       (concat (repeat 4 (repeat 8 0)))
       (flatten)
       (map partial-vol)
       (map partial-col)))

(def tile-conf (memoize tile-conf-nocache))

(partial-vol 1)


(defn midinote->step
  [m]
  (let [t (l/midinote->tile m)]
    {:step  (mod t 8)
     :val  (- 8 (int (/ t 8)))}))


(defn update-and-show
  [new-conf]
  (l/show (tile-conf @melody) new-conf))


;; returns new sequence that has step p marked
(defn mark-conf
  [conf p]
  (let [first-row (vec (first (partition 8 conf)))
        value-marked (bit-or l/red2 (get first-row p))
        first-row-marked (assoc first-row p value-marked)]
    (-> (cons first-row-marked
              (vec (drop 8 conf)))
        (flatten))))

(defn mod-get
  [coll p]
  (get coll (mod p (count coll))))

(defn conf-now
  [mode]
  (case mode
    :partials (tile-conf-partials @partials)
    :melody (tile-conf @melody)))

(defn p-now
  [mode p]
  (case mode
    :partials (mod p (count @partials))
    :melody (mod p (count @melody))))

(defn handle-event-note
  [e]
  (let [{:keys [step val]} (midinote->step (:note e))]
    (let [existing-val (get @melody step)
          new-val (if (= val existing-val)
                    0
                    val)
          new-melody (assoc @melody step new-val)]
      (l/show (tile-conf @melody)
              (tile-conf new-melody))
      (reset! melody new-melody))))

(defn midinote->partial
  [m]
  (let [t (l/midinote->tile m)
        p {:step  (mod t 8)
           :partial  (- 7 (int (/ t 8)))}]
    (when (< (:partial p) 4)
      p)))


(defn handle-event-partial
  [e]
  (when-let [{:keys [step partial]} (midinote->partial (:note e))]
    (let [old-partials @partials
          next-vol (->> (get-in @partials [step partial])
                        (partial-vol)
                        (next-partial-vol)
                        ((clojure.set/map-invert partial-vol-tbl)))
          new-partials (assoc-in old-partials [step partial] next-vol)]
      (l/show (tile-conf-partials old-partials)
              (tile-conf-partials new-partials))
      (reset! partials new-partials))))

(on-event
 [:midi :note-on]
 (fn [e]
   (case @mode
     :partials (handle-event-partial e)
     :melody (handle-event-note e)))
  ::launchpad-input-handler)

(defn player
  [t ns ps p old-conf]
  (let [conf (conf-now @mode)
        marked-conf (mark-conf conf (p-now @mode p))
        n (first ns)]
    ;; update status
    (l/show old-conf marked-conf)
    (when (not (zero? n))
      ;; play tone
      (let [partials (first ps)
            degree (get val->note (dec n))
            note (-> (first (p/degrees->pitches [degree] :minor :F3))
                     (+ (* 12 (int (/ n 8))))) ;; octave
            ]
        (apply s/beep-partial (cons note partials))))
    (let [t' (+ t 150)]
      (apply-by t' #'player [t' (rest ns) (rest ps) (inc p) marked-conf]))))

(defn change-mode
  [mode]
  (case mode
    :melody :partials
    :partials :melody))

(do
    (reset! melody [0 0 0 0 0 0 0 0])
    (reset! partials [[1 0 0.2 0]
                      [1 0.2 0.0 1.0]
;                      [1 0.3 0.5 0]
;                      [1 0.3 0.5 0]
                      [1 0.3 0.5 0]
                      ])
    (reset! mode :melody)
    (l/reset))

;; We can use the function below for random partials
;; by supplying
;;  (apply concat (repeatedly #'random-partials))
;; as partials-argument to the player fn
;; The random partials aren't visualized, though.
(defn random-partials
    []
    [(map #(* % 0.5 (rand)) (repeat 4 1))])

(comment

  (player (now)
          (u/infinite melody) ; melody
          (u/infinite partials) ; partials
          0 ; pointer :-/
          (conf-now @mode) ; old conf
          )

  (stop)

  ;; change mode
  (swap! mode change-mode)

  ;; fun stuff
  (swap! melody reverse)


  )
