(ns overtone-fun.launchpad
  (:require [overtone.midi :as midi])
  (:require [overtone.live :as overtone :refer :all :exclude [buffer]])
  (:require [clojure.core.async :as a])
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


;; monitor stuff
(on-event
 [:midi :note-on]
 (fn [e]
   (println (:note e)))
 ::lanchpad-input-handler)

;; dims of launchpad
(def w 8)
(def h 8)

(def tiles (atom (repeat (* w h) 0)))

(defn tile->midinote
  "Given an index of a square pad,
   return the corresponding midi note."
  [n]
  (+ n (* 8 (int (/ n 8)))))

(defn show
  "display tile config on launchpad"
  [tiles]
  (doseq [[v n] (map vector tiles (range))]
    (let [m (tile->midinote n)]
      (if (> v 0)
        (midi-note-on launch-out m v)
        (midi-note-off launch-out m)))))

(defn msg [a b c]
  (let [m (ShortMessage.)]
    (.setMessage m a b c)
    m))

(defn send-to-launchpad
  [bytes]
  (let [m (apply msg bytes)]
    (.send (:receiver launch-out) m -1)))

(defn bar
  "Repeatedly display random colors
   on the launchpad. Uses double buffering."
  ([]
     (bar true))
  ([buff1?]
     (let [t (now)]
       ;; select certain buffer
       (send-to-launchpad [0xb0 0x0 (if buff1? 0x31 0x34)])
       (show (repeatedly (* w h) #(bit-and
                                   ;; Launchpad Programmer’s Reference pg. 3:
                                   ;; leaves only color bits intact
                                   ;; Important regarding dbl buffering
                                   2r0110011 ;
                                   (rand-int 128))))
       (send-to-launchpad [0xb0 0x0 (if buff1? 0x34 0x31)])
       ;; start again, but with other buffer
       (apply-at (+ 200 t) #'bar [(not buff1?)]))))

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
  ;; disco
  (bar)
  ;; no more disco
  (do (stop)
      (send-to-launchpad dblbuff-off)
      (send-to-launchpad reset))
  ;; use color codes
  (midi-note-on launch-out 6 green1)
  ;; combine colors with bit-wise op
  (midi-note-on launch-out 6 (bit-or red2 green1)))
