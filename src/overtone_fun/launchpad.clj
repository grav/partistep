(ns overtone-fun.launchpad
  (:require [overtone.midi :as midi])
  (:require [overtone.live :as overtone :refer :all :exclude [buffer]])
  (:require [clojure.core.async :as a :refer :all])
  (:import [javax.sound.midi ShortMessage]))


(comment
  (connected-midi-receivers)
  (connected-midi-devices)

  (event-debug-on)

  (event-debug-off)


  )


(def launch-out
  ;; seems to be necessary in 0.9.0
  (midi-find-connected-receiver #"Launchpad"))


;; Play around with core.async
(let [input (chan)]
  ;; monitor stuff
  (on-event
   [:midi :note-on]
   (fn [e]
     (go (>! input  e)))
   ::lanchpad-input-handler)

  (go (while true
        (when-let [e (<! input)]
          (println e)))))






;; dims of launchpad
(def W 8)
(def H 8)

(defn tile->midinote
  "Given an index of a square pad,
   return the corresponding midi note."
  [n]
  (+ n (* W (int (/ n H)))))

(defn msg [a b c]
  (let [m (ShortMessage.)]
    (.setMessage m a b c)
    m))

(defn send-to-launchpad
  [bytes]
  (let [m (apply msg bytes)]
    (.send (:receiver launch-out) m -1)))

(def buff-code {1 0x31
                0 0x34})

(def buff (atom 1))

(defn other-buffer
  [current-buffer]
  (if (= current-buffer 1)
    0
    1))

(defn display-buffer
  [buffer]
  [0xb0 0x0 (get buff-code buffer)])

(defn show
  "display tile config on launchpad"
  [tiles]
  (swap! buff other-buffer)
  (send-to-launchpad (display-buffer @buff) )
  (doseq [[v n] (map vector tiles (range))]
    (let [m (tile->midinote n)]
      (if (> v 0)
        (midi-note-on launch-out m v)
        (midi-note-off launch-out m))))
  (send-to-launchpad (display-buffer (other-buffer @buff))))


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
(def reset [0xb0 0x0 0x0])

(def dblbuff-off [0xb0 0x0 0x30])

;; some velocity values
(def green1 2r0010000)
(def green2 2r0100000)
(def green3 2r0110000)

(def red1 2r0000001)
(def red2 2r0000010)
(def red3 2r0000011)

(comment
  ;; clear all
  (send-to-launchpad reset)

  (disco)
  ;; no more disco
  (do (stop)
      (send-to-launchpad dblbuff-off)
      (send-to-launchpad reset))
  ;; use color codes
  (midi-note-on launch-out 6 green1)
  ;; combine colors with bit-wise op
  (midi-note-on launch-out 6 (bit-or red2 green1)))
