(ns clj-adventofcode-2019.day11
  (:require
   [clj-adventofcode-2019.intcode :as intcode]))


(def 
  intcode-program 
  (intcode/load-program-from-resource "day11_input.txt"))

(defn change-direction [current-direction turn]
  (get {[:up :left] :left
        [:up :right] :right
        [:left :left] :down
        [:left :right] :up
        [:right :right] :down
        [:right :left] :up
        [:down :left] :right
        [:down :right] :left} [current-direction turn]
))

(comment
  ;examples
  (change-direction (change-direction :up :left) :left)
  ;; => :down
  (= (change-direction (change-direction :down :left) :right)
     (change-direction (change-direction :down :right) :left))
  ;; => true

)

(defn new-pos [current-pos direction]
  (case direction
    :left [(dec (first current-pos)) (second current-pos)]
    :right [(inc (first current-pos)) (second current-pos)]
    :up [(first current-pos) (inc (second current-pos))]
    :down [(first current-pos) (dec (second current-pos))]
    )
  )

(comment
  ;examples
  (new-pos [0 0] :left)
  ;; => [-1 0]
  (new-pos (new-pos [0 0] :down) :right)
  ;; => [1 -1]
  )

(defn int-to-turn [i]
  (if (= i 0) :left :right))

(def initial-state
  {:current-pos [0 0]
   :panels {[0 0] 0}
   :direction :up
   :program-state (intcode/initialize-program intcode-program)
   }  
  )


(defn do-painting [state]
  (let [current-color (get (:panels state) (:current-pos state) 0)
        next-program-state (intcode/run (assoc (:program-state state) :input [current-color] :output []))
        paint-color (first (:output next-program-state))
        turn (int-to-turn (second (:output next-program-state)))

        next-direction (change-direction (:direction state) turn)
        next-panels (assoc (:panels state) (:current-pos state) paint-color)
        next-pos (new-pos (:current-pos state) next-direction)
        is-exit? (= (:status next-program-state) :exited)

        next-state (assoc state
                          :current-pos next-pos
                          :panels next-panels
                          :direction next-direction
                          :program-state next-program-state)]
    (if is-exit? next-state (recur next-state)))
  
  
  )

(def part1-run-result (do-painting initial-state))

(def part1-solution-number-of-painted-panels (count (:panels part1-run-result)))

(def starting-on-white-initial-state
  {:current-pos [0 0]
   :panels {[0 0] 1}
   :direction :up
   :program-state (intcode/initialize-program intcode-program)}
  )

(def part2-run-result (do-painting starting-on-white-initial-state))

(:panels part2-run-result)

(defn add-missing-values [m]
  (for [i (range 0 45)] [i (get m i 0)]))


(defn get-row [row-number panels]
  (->>
   panels
   (filter (fn [[[_ r] _]] (= r row-number)))
   (map (fn [[[c _] v]] [c v]))
   (into {})
   (add-missing-values)
   (map (fn [[_ v]] (get {0 " " 1 "*" } v)))
   (apply str)
   ))


(comment
  (get-row -1 (:panels part2-run-result))
  ;(add-missing-values {5 1})
  )

(for [row (range 0 -6 -1)]
  (get-row row (:panels part2-run-result)))
;; => ("   ** **** *    **** ****  **  *  * ***      "
;;     "    * *    *    *    *    *  * *  * *  *     "
;;     "    * ***  *    ***  ***  *    **** *  *     "
;;     "    * *    *    *    *    * ** *  * ***      "
;;     " *  * *    *    *    *    *  * *  * *        "
;;     "  **  **** **** **** *     *** *  * *        ")


