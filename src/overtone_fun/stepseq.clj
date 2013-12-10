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

(l/show (tile-conf @my-sequence) (tile-conf-partials []))

(partial-vol 1)

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
  (let [conf (tile-conf-partials @partials)
        marked-conf (mark-conf conf (mod p (count @partials)))
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
        (apply u/beep-partial (cons note partials))))
    (let [t' (+ t 150)]
      (apply-by t' #'player [t' (rest ns) (rest ps) (inc p) marked-conf]))))

;; disabled for now
#_(on-event
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

(defn midinote->partial
  [m]
  (let [t (l/midinote->tile m)
        p {:step  (mod t 8)
           :partial  (- 7 (int (/ t 8)))}]
    (when (< (:partial p) 4)
      p)))

(on-event
 [:midi :note-on]
 (fn [e]
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
 ::launchpad-input-handler)

(def partials (atom [[1 0 0.1 0]
                     [1 0.3 0 0]
                     [1 0 0.5 0]
                     [1 0 0 0.2]]))

(reset! partials [[1 0 0.2 0]
                  [1 0.2 0.0 1.0]
                  [1 0.3 0.5 0]
                  [1 0.3 0.5 0]
                  [1 0.3 0.5 0]])

(defn infinite
  [a]
  (->> (repeatedly (fn [] (lazy-seq @a)))
       (apply concat )))

(reset! partials (take 3 @partials))

(reset! my-sequence [1 2 3 4 5 6 7 8])

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
