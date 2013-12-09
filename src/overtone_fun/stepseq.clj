(ns overtone-fun.stepseq
  (:require [overtone.live :as overtone :refer :all]
            [overtone-fun.launchpad :as l]
            [overtone-fun.util :as u]
            [overtone.music.pitch :as p]))


(def val->note [:i :ii :iii :iv :v :vi :vii :i])

(defn tile-conf-nocache [s]
  (-> (for [i (take 8 (iterate dec 8))]
        (map #(if (= i %) l/green2 0) s))
      (flatten)))

(def tile-conf (memoize tile-conf-nocache))

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

(defn mod-get
  [coll p]
  (get coll (mod p (count coll))))

(defn player
  [t ns ps p old-conf]
  (let [conf (tile-conf @my-sequence)
        marked-conf (mark-conf conf p)
        n (first ns)]
    ;; update status
    (l/show old-conf marked-conf)
    (when (not (zero? n))
      ;; play tone
      (let [_ (println (dec n))
            partials (first ps)
            degree (get val->note (dec n))
            note (-> (first (p/degrees->pitches [degree] :minor :F3))
                     (+ (* 12 (int (/ n 8))))) ;; octave
            ]
        (apply u/beep-partial (cons note partials))))
    (let [t' (+ t 150)]
      (apply-by t' #'player [t' (rest ns) (rest ps) (mod (inc p) 8) marked-conf]))))

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

(def partials (atom [[1 0 0.1 0]
                     [1 0.3 0 0]
                     [1 0 0.5 0]
                     [1 0 0 0.2]]))

(reset! partials [[1 0 0.2 0]
                  [1 0.2 0.0 1.0]
                  [1 0.3 0.5 0]
                  ])

(defn infinite
  [a]
  (->> (repeatedly (fn [] (lazy-seq @a)))
       (apply concat )))



(comment
  (do
    (reset! my-sequence (vec (repeat 8 0)))
    (l/reset))

  (player (now)
          (infinite my-sequence)
          (infinite partials)
          0
          (tile-conf @my-sequence))
  (stop))

(p/note :C4)

(p/resolve-scale :major)
