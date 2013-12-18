(ns partistep.stepseq
  (:require [overtone.live :as overtone :refer :all]
            [overtone.music.pitch :as p]
            [partistep.launchpad :as l]
            [partistep.synth :as s]
            [partistep.util :as u]))

(comment
  ;; ==============
  ;;   PartiStep
  ;; ==============
  ;; This is a step sequencer that can sequence both
  ;; melody and the partials.
  ;; The number of melody steps and partial steps
  ;; can be changed individually, which can lead
  ;; to some weird polyrhythmic patterns.

  ;; To use, connect launchpad (if you have one),
  ;; start 'lein repl', and connect from emacs. Then compile this namespace.

  ;; -- with launchpad
  ;; The step sequencer can be started with the lowest right-arrow on the launchpad
  ;; and stopped with the right-arrow above.
  ;; You can switch mode between melody and partials using the top-most right-arrow.

  ;; -- without launchpad
  ;; If you don't have a launchpad,
  ;; you can play the sequencer by evaluating expressions.
  ;; Here are a few examples:

  ;; start playing
  (play-now)

  ;; or don't
  (stop)

  ;; atoms for sequencing:
  @melody
  @partials

  ;; active steps
  @melody-steps
  @partial-steps

  ;; alter the melody
  (swap! melody reverse)

  ;; set the partials
  (reset! partials [[1.0 1.0]
                    [0.5 0.2 0.6]
                    [0.1 0.5 0.3]
                    [1.0 1.0]
                    [0.5 0.2 0.6]
                    [0.1 0.5 0.3]
                    [1.0 1.0]
                    [0.5 0.2 0.6]])

  ;; play some of the steps
  (reset! melody-steps '(0 1 2 3 4))

  (reset! partial-steps '(0 1 2)))


;;; ================

(def partial-steps (atom [0 1 2 3 4 5 6 7]))

(def melody-steps (atom [0 1 2 3 4 5 6 7]))

(def partials (atom nil))

(def max-partials 8)

(def melody (atom nil))

(def mode (atom nil))

(def val->note [:i :ii :iii :iv :v :vi :vii :i])

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

(defn tile-conf-nocache [s]
  (-> (for [i (take 8 (iterate dec 8))]
        (map #(if (= i %) l/green2 0) s))
      (flatten)))

(def tile-conf (memoize tile-conf-nocache))

(defn tile-conf-partials [ps]
  (->> (let [ps (concat ps
                        (repeat (- max-partials (count ps))
                                (repeat max-partials 0)))]
         (for [i (take max-partials (iterate dec (dec max-partials)))]
           (for [partials (u/extend-vec ps max-partials)]
             (get partials i 0))))
       (flatten)
       (map partial-vol)
       (map partial-col)))

(defn midinote->step
  [m]
  (let [t (l/midinote->tile m)]
    {:step  (mod t 8)
     :val  (- 8 (int (/ t 8)))}))


;; returns new sequence that has step p marked
(defn mark-conf
  [conf p]
  (let [first-row (vec (first (partition 8 conf)))
        value-marked l/red3
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
          new-melody (assoc (vec @melody) step new-val)]
      (l/show (tile-conf new-melody) [])
      (reset! melody new-melody))))

(defn midinote->partial
  [m]
  (let [t (l/midinote->tile m)
        p {:step  (mod t 8)
           :partial  (- 7 (int (/ t 8)))}]
    (when (and
           (< (:step p) (count @partials))
           (< (:partial p) max-partials))
      p)))

(defn dprn [& e]
  (do (print e)
      e))

(defn handle-event-partial
  [e]
  (when-let [{:keys [step partial]} (midinote->partial (:note e))]
    (let [old-partials @partials
          ps (-> (get @partials step)
                 (u/extend-vec max-partials))
          next-vol (->> (get ps partial)
                        (partial-vol)
                        (next-partial-vol)
                        ((clojure.set/map-invert partial-vol-tbl)))
          new-ps (assoc ps partial next-vol)
          new-partials (assoc old-partials step new-ps)]
      (l/show (tile-conf-partials new-partials) [])
      (reset! partials new-partials))))

(defn change-mode
  [mode]
  (case mode
    :melody :partials
    :partials :melody))

(declare play-now)

(defn handle-right-arrow
  [{note :note}]
  (case note
    8 (swap! mode change-mode)
    104 (stop)
    120 (play-now)
    :else nil))

(defn handle-event-cc
  [e]
  (when (l/is-upper-btn? e)
    (let [steps (case @mode
                  :melody melody-steps
                  :partials partial-steps)
          n (-> (:data1 e)
                (- 104))]

      (reset! steps
              (take (inc n) [0 1 2 3 4 5 6 7 8])))))

(on-event
 [:midi :control-change]
 #'handle-event-cc
 ::launchpad-cc-handler)

(on-event
 [:midi :note-on]
 (fn [{note :note :as e}]
   (cond
    (l/is-tile? note) (case @mode
                     :partials (handle-event-partial e)
                     :melody (handle-event-note e))
    (l/is-right-arrow? note) (handle-right-arrow e)))
 ::launchpad-input-handler)

(count @(atom '(1 2 3)))

(defn player
  [t ns ps m-steps p-steps]
  (let [conf (conf-now @mode)
        p  (-> (if (= @mode :melody)
                 m-steps
                 p-steps)
               (first))
        marked-conf (mark-conf conf p)
        n-steps (->
                 (case @mode
                   :melody  @melody-steps
                   :partials @partial-steps)
                 (count))
        yellow (bit-or l/green3 l/red2)
        upper-conf  (concat
                     (repeat n-steps yellow)
                     (repeat (- 8 n-steps) 0))
        n (first ns)]
    ;; update status
    (l/show marked-conf upper-conf)
    (when (not (zero? n))
      ;; play tone
      (let [partials (first ps)
            degree (get val->note (dec n))
            note (-> (first (p/degrees->pitches [degree] :minor :F3))
                     (+ (* 12 (int (/ n 8))))) ;; octave
            ]
        (apply s/beep-partial (cons note partials))))
    (let [t' (+ t 200)]
      (apply-by t' #'player [t' (rest ns) (rest ps) (rest m-steps) (rest p-steps)]))))

(do
  (reset! melody (repeatedly 8 #(int (* 9 (rand)))))
  (reset! partials (vec (repeat 8 [1])))
  (reset! mode :melody)
  (l/reset)
  (l/show (conf-now @mode) (repeat 8 0)))

;; We can use the function below for random partials
;; by supplying
;;  (apply concat (repeatedly #'random-partials))
;; as partials-argument to the player fn
;; The random partials aren't visualized, though.
(defn random-partials
    []
    [(map #(* % 0.5 (rand)) (repeat max-partials 1))])

(defn get-from
  [l-ref]
  (fn [s]
    (get (vec @l-ref) s)))

(defn play-now
  []
  (player (+ 300 (now))
          (u/infinite (fn [] (lazy-seq (map (get-from melody) @melody-steps)))) ; melody
          (u/infinite (fn [] (lazy-seq (map (get-from partials) @partial-steps)))) ; partials
          (u/infinite (fn [] (lazy-seq @melody-steps)))
          (u/infinite (fn [] (lazy-seq @partial-steps)))
          ))
