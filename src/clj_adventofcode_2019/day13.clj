(ns clj-adventofcode-2019.day13
  (:gen-class)
  (:require
   [clojure.java.io :as io]
   [clojure.math.numeric-tower :as math]
   [clojure.string :as str]
   [clj-adventofcode-2019.intcode :as intcode]
   ))

(def the-arcade-program (intcode/load-program-from-resource "day13_input.txt"))

(def arcade-program-result (intcode/run (intcode/initialize-and-run the-arcade-program)))

(def program-output (:output arcade-program-result))

(def program-output-instructions (partition 3 program-output))

(def part1-number-ofblock-tiles
  (count (filter (fn [[_ _ z]] (= 2 z)) program-output-instructions)))


; ---- part2 ----

(def tiles {
            0 " "
            1 "█"
            2 "0"
            3 "▀"
            4 "o"
            })

(defn draw-row [row]
  (->>
   row
   (map (fn [[x _ v]] [x (get tiles v)]))
   (sort-by first)
   (map second)
   (apply str)
   )
  )

(defn draw-output [output]
  (let [instructions (partition 3 output)
        score (last (first (filter (fn [[x y _]] (and (= x -1) (= y 0))) instructions)))
        rows (sort-by 
              (fn [[k _]] k) 
              (group-by second 
                        (filter (fn [[x _ _]] (> x -1)) instructions)))]
    (into [(str "Score: " score)] (map (fn [[_ v]] (draw-row v)) rows))
    ))


(comment
  (draw-output program-output)
  )

; --- game loop ---
; Memory address 0 represents the number of quarters that have been inserted; set it to 2 to play for free. :D

(def patched-arcade-program (assoc the-arcade-program 0 2))

(def initial-state (intcode/initialize-and-run patched-arcade-program))

(defn merge-outputs [prev-state next-state]
  (let [
        prev-output (partition 3 (:output prev-state))
        next-output (partition 3 (:output next-state))
        prev-output-map (into {} (map (fn [[k v t]] [[k v] t]) prev-output))
        merged-output-map (into prev-output-map (map (fn [[k v t]] [[k v] t]) next-output))
        merged-output (apply vector (flatten (map (fn [[[x y] t]] [x y t]) merged-output-map)))]
    (assoc next-state :output merged-output)))

(comment
  (merge-outputs {:output [1 2 3 2 2 3 1 1 0]} {:output [1 2 4 2 2 4]})
  ;; => {:output [1 2 4 2 2 4 1 1 0]}
  )

(defn calculate-move [state]
  (let [output-instructions (partition 3 (:output state))
        paddle-col (ffirst (filter (fn [[_ _ t]] (= t 3)) output-instructions))
        ball-col (ffirst (filter (fn [[_ _ t]] (= t 4)) output-instructions))]
    (cond
      (< paddle-col ball-col) 1
      (> paddle-col ball-col) -1
      (= paddle-col ball-col) 0)))

(defn game-loop [state]
  (let [output (:output state)
        screen (clojure.string/join "\n" (draw-output output))
        status (:status state)]
    (print (str (char 27) "[2J")) ; clear screen
    (print (str (char 27) "[;H")) ; move cursor to the top left corner of the screen
    (println screen)
    (if (not= status :exited)
      (recur (merge-outputs state (intcode/run (assoc state
                                 :input [(calculate-move state)] ; (read-string (read-line))FIXME
                                 :output []))))
      (println " -- GAME OVER --")
      )))

(defn -main
  "Play"
  [& args]
  (game-loop initial-state))