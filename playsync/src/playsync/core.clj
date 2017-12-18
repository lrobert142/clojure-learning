(ns playsync.core
  (:require [clojure.core.async
             :as a
             :refer [>! <! >!! <!! go chan buffer close! thread
                     alts! alts!! timeout sliding-buffer dropping-buffer]]))

; Creates a channel with the default buffer limit (1)
; Meaning only one item can sit in the buffer before other threads will
; wait until it is free again
(def echo-chan (chan))
(defn invoke-echo-chan
  "Allows the example to run with blocking the REPL"
  []
  (go (println (<! echo-chan)))
  (>!! echo-chan "ketchup"))
; >!! & <!! are used on the main thread to say, "Hey, this needs to block because I
; need to push this to a new thread! I don't want anything going wrong and killing
; this thread because it's the main one, so wait until I'm certain everything is good!"
; >! & <! are used in other threads to say, "Hey, I'd like to complete these tasks ASAP,
; so if I'm waiting for any time, just put something else on this thread to keep the
; work going. I'm not the main, so if it breaks just gimme another one!"

; Creates a channel with a buffer limit of 2
(def echo-buffer (chan 2))
; To call: (>!! echo-buffer "ketchup")
; NOTE: After 2 calls the buffer will be full and any extra calls
; will result in the thread 'freezing' until the buffer is empty

; Creates a 'sliding buffer' that will remove the oldest items from the
; buffer to put in new ones
(def echo-sliding-buffer (chan (sliding-buffer 2)))
; To call: (>!! echo-sliding-buffer "ketchup")

; Creates a 'dropping buffer' that will remove the newest items from the
; buffer to put in newer ones
(def echo-dropping-buffer (chan (dropping-buffer 2)))
; To call: (>!! echo-dropping-buffer "ketchup")

(defn hot-dog-machine-v2
  [hot-dog-count]
  (let [in (chan)
        out (chan)]
    (go (loop [hc hot-dog-count]
          (if (> hc 0)
            (let [input (<! in)]
              (if (= 3 input)
                 (do (>! out "hot dog")
                     (recur (dec hc)))
                 (do (>! out "wilted lettuce")
                     (recur hc))))
            (do (close! in)
                 (close! out)))))
    [in out]))
; To call:
;(let [[in out] (hot-dog-machine-v2 2)]
;  (>!! in "pocket lint")
;  (println (<!! out))
;
;  (>!! in 3)
;  (println (<!! out))
;
;  (>!! in 3)
;  (println (<!! out))
;
;  (>!! in 3)
;  (<!! out))

;Piping
;(let [c1 (chan)
;      c2 (chan)
;      c3 (chan)]
;  (go (>! c2 (clojure.string/upper-case (<! c1))))
;  (go (>! c3 (clojure.string/reverse (<! c2))))
;  (go (println (<! c3)))
;  (>!! c1 "redrum"))

(defn append-to-file
  "Write a string to the end of a file"
  [filename s]
  (spit filename s :append true))

(defn format-quote
  "Delineate the beginning and end of a quote because it's convenient"
  [quote]
  (str "=== BEGIN QUOTE ===\n" quote "=== END QUOTE ===\n\n"))

(defn random-quote
  "Retrieve a random quote and format it"
  []
  (format-quote (slurp "http://www.braveclojure.com/random-quote")))

(defn snag-quotes
  [filename num-quotes]
  (let [c (chan)]
    (go (while true (append-to-file filename (<! c))))
    (dotimes [n num-quotes] (go (>! c (random-quote))))))
; let c = new channel
; Using an infinite loop, process each item in the queue by taking the first
; available item from the channel (which will also remove it from the channel,
; making the loop wait until something else becomes available)
; Then, we put items in the queue by looping "num-quotes" times


(defn upper-caser
  "Converts input to UPPERCASE and returns a new channel with this value in its buffer"
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/upper-case (<! in)))))
    out))

(defn reverser
  "Reverses input and returns a new channel with this value in its buffer"
  [in]
  (let [out (chan)]
    (go (while true (>! out (clojure.string/reverse (<! in)))))
    out))

(defn printer
  "Prints the input value from a channel"
  [in]
  (go (while true (println (<! in)))))

;Initial channel
(def in-chan (chan))
;Channel used to uppercase input from the in-chan channel
(def upper-caser-out (upper-caser in-chan))
;Channel to reverse input from the upper-caser-out channel
(def reverser-out (reverser upper-caser-out))
;Channel to print input from the reverser-out channel
(printer reverser-out)
;To call: (>!! in-chan "redrum")
