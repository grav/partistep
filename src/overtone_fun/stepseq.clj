(ns overtone-fun.stepseq
  (:require [overtone.live :as overtone :refer :all]
            [overtone-fun.launchpad :as l]
            [overtone-fun.util :as u]))


(def val->note [60 62 64 65 67 69 71 72])

(defn tile-conf [s]
  (-> (for [i (take 8 (iterate dec 8))]
        (map #(if (= i %) l/green2 0) s))
      (flatten)))

(defn midinote->step
  [m]
  (let [t (l/midinote->tile m)]
    {:step  (mod t 8)
     :val  (- 8 (int (/ t 8)))}))


(def my-sequence (atom (vec (repeat 8 0))))

(defn update-and-show
  [new-conf]
  (l/show (tile-conf @my-sequence) new-conf))


;; returns new sequence that has step p marked
(defn mark-conf
  [conf p]
  (let [last-row (vec (last (partition 8 conf)))
        value-marked (bit-or l/red2 (get last-row p))
        last-row-marked (assoc last-row p value-marked)]
    (-> (conj (vec (take (* 8 7) conf))
              last-row-marked)
        (flatten))))

(defn player
  [t ns p old-conf]
  (let [conf (tile-conf @my-sequence)
        marked-conf (mark-conf conf p)]
    (l/show old-conf marked-conf)
    (when-let [n (get @ns p)]
      (when (not (zero? n))
        (let [note (get val->note (dec n))]
          (at t
              (u/beep-partial note)))))
    (let [t' (+ t 200)
          steps (count @ns)]
      (apply-at t' #'player [t' ns (mod (inc p) steps) marked-conf]))))

(player (now) my-sequence 0 (tile-conf @my-sequence))
(stop)

(do
  (reset! my-sequence (repeat 8 0))
  (l/reset))

(on-event
 [:midi :note-on]
 (fn [e]
   (let [{:keys [step val]} (midinote->step (:note e))]
     (let [existing-val (get @my-sequence step)
           new-val (if (= val existing-val)
                     0
                     val)]
       (swap! my-sequence #(assoc % step new-val))
       (let [new-conf (tile-conf @my-sequence)]
         (update-and-show new-conf)))))
 ::lanchpad-input-handler)
