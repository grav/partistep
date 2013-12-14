(ns partistep.launchpad
  (:require [overtone.midi :as midi])
  (:require [overtone.live :as overtone :refer :all :exclude [buffer]])
  (:import [javax.sound.midi ShortMessage]))


(comment
  ;; debugging

  (midi-connected-receivers)
  (midi-connected-devices)

  (event-debug-on)

  (event-debug-off))


(defonce launch-out
  ;; seems to be necessary in 0.9.0
  (midi-find-connected-receiver #"Launchpad"))

;; dims of launchpad
(def W 8)
(def H 8)

(defn tile->midinote
  "Given an index of a square pad,
   return the corresponding midi note."
  [n]
  (+ n (* W (int (/ n H)))))

(defn midinote->tile
  [m]
  (let [width-multiplier   (int (/ m 2 W))]
    (+ (mod m W)
       (* W width-multiplier))))




(defn msg [a b c]
  "Create javax.sound.midi.ShortMessage
   a: channel
   b: data1
   c: data2"
  (let [m (ShortMessage.)]
    (.setMessage m a b c)
    m))

(defn send-to-launchpad
  "Send array of bytes interpreted as [channel data1 data2]
   to the launchpad"
  [bytes]
  (let [m (apply msg bytes)]
    (.send (:receiver launch-out) m -1)))

;; the byte codes for the two buffers
(def buff-code {1 0x31
                0 0x34})

;; state of the buffer
(def buff (atom 1))

(defn other-buffer
  "inverse buffer"
  [current-buffer]
  (if (= current-buffer 1)
    0
    1))

(defn display-buffer-bytes
  "given a buffer,
   generate the bytes for setting the buffer as the displayed one"
  [buffer]
  [0xb0 0x0 (get buff-code buffer)])

(defn show
  "display tile config on launchpad. tiles is an array of velocities.
   the mapping from vel to color can be found in the Launchpad Programmer's Reference"
  [old-tiles tiles]
  (swap! buff other-buffer)
  (send-to-launchpad (display-buffer-bytes @buff) )
  (doseq [[v1 v2 n] (map vector old-tiles tiles (range))]
    (when (not= v1 v2)
      (let [m (tile->midinote n)]
        (if (> v2 0)
          (midi-note-on launch-out m v2)
          (midi-note-off launch-out m)))))
  (send-to-launchpad (display-buffer-bytes (other-buffer @buff))))


(defn disco
  "Repeatedly display random colors
   on the launchpad. Uses double buffering."
  []
  (let [t (now)]
    (show (repeatedly (* W H) #(bit-and
                                ;; Launchpad Programmer’s Reference pg. 3:
                                ;; leaves only color bits intact
                                ;; Important regarding dbl buffering
                                2r0110011 ;
                                (rand-int 128))))
    (apply-at (+ 200 t) #'disco)))

;; some useful messages
(def reset-msg [0xb0 0x0 0x0])

(def dblbuff-off [0xb0 0x0 0x30])

;; some velocity values
(def green1 2r0010000)
(def green2 2r0100000)
(def green3 2r0110000)

(def red1 2r0000001)
(def red2 2r0000010)
(def red3 2r0000011)

(defn reset []
  (send-to-launchpad reset-msg))

(comment
  ;; clear all
  (send-to-launchpad reset-msg)
  (disco)
  ;; no more disco
  (do (stop)
      (send-to-launchpad dblbuff-off)
      (send-to-launchpad reset))
  ;; use color codes
  (midi-note-on launch-out 6 green1)
  ;; combine colors with bit-wise op
  (midi-note-on launch-out 6 (bit-or red2 green1)))

;; search freesound for short clicks
(def results (->> (freesound-search "click")
                  (filter (fn [r] (< (:duration r) 0.2)))))


(defn click
  "play the n'th click sound (from freesound)"
  [n]
  (let [snd (-> results
                (nth n)
                (:id)
                (freesound-path)
                (sample))]
    (snd)))

(def notes (atom (vec (repeat 8 nil))))

(def mylist [1 2 3])

(defn show-step
  [step]
  (midi-note-on launch-out (+ 16 step) red2))



(defn player
  [t ns p]
  (at t
      (println "tick")
      (reset)
      (when-let [n (get @ns p)]
        (println n)
        (click n)
        (midi-note-on launch-out n green1)
        (show-step p)))
  (let [t' (+ t 200)
        steps (count @ns)]
    (apply-at t' #'player [t' ns (mod (inc p) steps)])))


(comment


  (stop)

  (reset)

  (defonce mystuff (chan 8))

  (player (now) mystuff)

  (doseq [n [1 2 3 4 5]]
    (go (>! mystuff {:note 34}))))


(defn record-note [note]
  (reset)
  (click note)
  (midi-note-on launch-out note green3)
  (show-step (count @notes))
  (swap! notes #(conj % note)))

(comment
  (swap! notes (fn [m] (assoc m 0 nil)))

  (reset! notes [1 3 4 1 2 3 6 5])

  (player (now) notes 0)

  (count @notes)

  (stop)

  (reset! notes [])
)
