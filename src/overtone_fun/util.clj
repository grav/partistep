(ns overtone-fun.util
    (:use [overtone.live]
          [overtone.inst.synth]))

(defn take-rand [coll n]
  (take n (shuffle coll) ))

#_(def default-buf (buffer 8))
#_(buffer-write! default-buf 0 [17 17 17 17])

(int (first (buffer-read def-buf 0 1)))

#_(definst beep [note 60 partials-buffer def-buf]
  (*
   (let [partials (buffer-read)
         f (midicps note)
         oscs (for [p partials]
                (sin-osc (* f p)))]
     (apply + oscs))
   (env-gen (perc 0.01 0.35) :action FREE)))

#_(beep)

(defn idxs->binvec [idxs]
  (let [idxs (set (sort idxs))
        size (+ 1 (last idxs))]
   (map (fn [i] (if (contains? idxs i) 1 0)) (range size))))

(defn binvec->idxs [b]
  (->>
   (map (fn [i] (when (= 1 (nth b i)) i)) (range (count b)))
   (filter (comp not nil?))))

(defn partials->buf [partials]
  (let [size (last (sort partials))
        buf-vec (idxs->binvec partials)
        buf (buffer (count buf-vec))]
    (dosync
     (buffer-write! buf 0 buf-vec)
     buf)))


(defn buf->partials [buf]
  (->> (buffer-read buf)
       (map int)
       (binvec->idxs)))
