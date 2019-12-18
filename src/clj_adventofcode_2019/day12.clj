(ns clj-adventofcode-2019.day12
  (:require
   [clojure.java.io :as io]
   [clojure.math.numeric-tower :as math]
   [clojure.string :as str]))


(defn parse-line [index line]
  [index { :pos
          (into {}
                (map (fn [[k v]] [(keyword k) (read-string v)])
                     (map
                      (fn [v] (str/split v #"="))
                      (str/split (str/replace (str/replace line "<" "") ">" "") #", "))))
          :vel {:x 0 :y 0 :z 0}
          }])  

(def puzzle-input (slurp (io/resource "day12_input.txt")))

(def initial-input
  (into {}
        (map-indexed parse-line (str/split-lines puzzle-input))))


; initial-input
; <x=-1, y=0, z=2>
; <x=2, y=-10, z=-7>
; <x=4, y=-8, z=8>
; <x=3, y=5, z=-1>
(def sample-moon-positions-1
  {0 {:pos {:x -1 :y 0 :z 2}
      :vel {:x 0  :y 0 :z 0}}
   1 {:pos {:x 2 :y -10 :z -7}
      :vel {:x 0  :y 0 :z 0}}
   2 {:pos {:x 4 :y -8 :z 8}
      :vel {:x 0  :y 0 :z 0}}
   3 {:pos {:x 3 :y 5 :z -1}
      :vel {:x 0  :y 0 :z 0}}})

(defn adjust-all-axis [a b]
  (into {} (for [axis [:x :y :z]]
             [axis (+ (get a axis) (get b axis))])
  ))

(comment
 (adjust-all-axis {:x 0 :y 0 :z 10} {:x 5 :y -2 :z 0})
  ;; => {:x 5, :y -2, :z 10}
 )

(defn calc-vel [moon-index moons-data]
  (let [other-moons-data (filter (fn [[i _]] (not= i moon-index)) moons-data)
        other-moons-positions (map (fn [[_ data]] (:pos data)) other-moons-data)
        current-moon-pos (:pos  (get moons-data moon-index))
        current-velocity (:vel (get moons-data moon-index))
        velocity-changes-grouped (group-by first (for [axis [:x :y :z]
                                                       other-moon-pos other-moons-positions]
                                                   (let [o (get other-moon-pos axis)
                                                         c (get current-moon-pos axis)]
                                                     (cond
                                                       (= o c) [axis 0]
                                                       (> o c) [axis 1]
                                                       (< o c) [axis -1]))))

        velocity-changes-summed
        (into {} 
              (map 
               (fn [[axis v]] [axis (reduce + (map second v))])
               velocity-changes-grouped))
        new-velocity (adjust-all-axis current-velocity velocity-changes-summed)
        ]
    new-velocity))

(comment
  (calc-vel 3 sample-moon-positions-1)
  )

(defn step [moons-data]
  (into {}
        (let [indexes (keys moons-data)]
          (for [index indexes]
            (let [new-velocity (calc-vel index moons-data)
                  new-position (adjust-all-axis (:pos (get moons-data index)) new-velocity)]
              [index {:pos new-position :vel new-velocity}])))))

(comment
  (step sample-moon-positions-1)
  ;; => {0 {:pos {:x 2, :y -1, :z 1}, :vel {:x 3, :y -1, :z -1}},
  ;;     1 {:pos {:x 3, :y -7, :z -4}, :vel {:x 1, :y 3, :z 3}},
  ;;     2 {:pos {:x 1, :y -7, :z 5}, :vel {:x -3, :y 1, :z -3}},
  ;;     3 {:pos {:x 2, :y 2, :z 0}, :vel {:x -1, :y -3, :z 1}}}

  (last (take 11 (iterate step sample-moon-positions-1)))
  ;; => {0 {:pos {:x 2, :y 1, :z -3}, :vel {:x -3, :y -2, :z 1}},
  ;;     1 {:pos {:x 1, :y -8, :z 0}, :vel {:x -1, :y 1, :z 3}},
  ;;     2 {:pos {:x 3, :y -6, :z 1}, :vel {:x 3, :y 2, :z -3}},
  ;;     3 {:pos {:x 2, :y 0, :z 4}, :vel {:x 1, :y -1, :z -1}}}
  )


(defn energy [pos]
  (reduce + (map (fn [[_ v]] (math/abs v)) pos)))


(defn total-energy [moon-data]
  (* (energy (:pos moon-data))
     (energy (:vel moon-data))))


(comment
  (energy {:x 5 :y 1 :z -5})
  ;; => 1
  
  (total-energy
   (get
    (last
     (take 11 (iterate step sample-moon-positions-1))) 0))
  ;; => 36
  (map (fn [[_ moon-data]] (total-energy moon-data))
       (last (take 11 (iterate step sample-moon-positions-1))))
  ;; => (36 45 80 18)
  (reduce +
          (map (fn [[_ moon-data]] (total-energy moon-data))
               (last (take 11 (iterate step sample-moon-positions-1)))))
  ;; => 179
)

(step initial-input)
;; => Execution error (NullPointerException) at clj-adventofcode-2019.day11/adjust-all-axis$iter$fn$fn (form-init7063701141190084591.clj:40).
;;    null

;; => {0 {:x 3, :y 3, :z 0}, 1 {:x 4, :y -16, :z 2}, 2 {:x -10, :y -6, :z 5}, 3 {:x -3, :y 0, :z -13}}


; part1 solution
(reduce +
        (map (fn [[_ moon-data]] (total-energy moon-data))
             (last (take 1001 (iterate step initial-input)))))
;; => 12351

(defn does-history-really-repeat-itself [initial]
  (loop [tries 0
         input initial
         ]
    (if (and (> tries 0) (= input initial)) {:tries tries :input input}
        (recur (inc tries) (step input)))
  ))
;; => #'clj-adventofcode-2019.day11/does-history-really-repeat-itself

;; => #'clj-adventofcode-2019.day11/Does-history-really-repeat-itself

(does-history-really-repeat-itself sample-moon-positions-1)
;; => {:tries 2772,
;;     :input
;;     {0 {:pos {:x -1, :y 0, :z 2}, :vel {:x 0, :y 0, :z 0}},
;;      1 {:pos {:x 2, :y -10, :z -7}, :vel {:x 0, :y 0, :z 0}},
;;      2 {:pos {:x 4, :y -8, :z 8}, :vel {:x 0, :y 0, :z 0}},
;;      3 {:pos {:x 3, :y 5, :z -1}, :vel {:x 0, :y 0, :z 0}}}}


;;(does-history-really-repeat-itself initial-input)

(defn get-axis [state axis]
  (map (fn [[i v]] [i {:pos (get (:pos v) axis)
                    :vel (get (:vel v) axis)}]) state)
  )
(get-axis initial-input :x)

(defn does-history-really-repeat-itself-axis [initial axis]
  (loop [tries 0
         input initial]
    (if (and (> tries 0)
             (= (get-axis input axis) (get-axis initial axis))) 
      {:tries tries :input input}
      (recur (inc tries) (step input)))))


(does-history-really-repeat-itself-axis initial-input :x)
;; => {:tries 22958,
;;     :input
;;     {0 {:pos {:x 3, :y 36, :z 582}, :vel {:x 0, :y 18, :z 6}},
;;      1 {:pos {:x 4, :y -247, :z -85}, :vel {:x 0, :y -10, :z -1}},
;;      2 {:pos {:x -10, :y 557, :z -300}, :vel {:x 0, :y -19, :z -2}},
;;      3 {:pos {:x -3, :y -365, :z -203}, :vel {:x 0, :y 11, :z -3}}}}

(does-history-really-repeat-itself-axis initial-input :y)
;; => {:tries 286332,
;;     :input
;;     {0 {:pos {:x 4, :y 3, :z 137}, :vel {:x 9, :y 0, :z -3}},
;;      1 {:pos {:x -2, :y -16, :z 156}, :vel {:x -4, :y 0, :z -59}},
;;      2 {:pos {:x -18, :y -6, :z -308}, :vel {:x -15, :y 0, :z 37}},
;;      3 {:pos {:x 10, :y 0, :z 9}, :vel {:x 10, :y 0, :z 25}}}}

(does-history-really-repeat-itself-axis initial-input :z)
;; => {:tries 231614,
;;     :input
;;     {0 {:pos {:x -85, :y 477, :z 0}, :vel {:x 12, :y 1, :z 0}},
;;      1 {:pos {:x -10, :y -659, :z 2}, :vel {:x -5, :y 33, :z 0}},
;;      2 {:pos {:x -5, :y 220, :z 5}, :vel {:x 8, :y -26, :z 0}},
;;      3 {:pos {:x 94, :y -57, :z -13}, :vel {:x -15, :y -8, :z 0}}}}

(defn gcd
  [a b]
  (if (zero? b)
    a
    (recur b, (mod a b))))


(defn lcm
  [a b]
  (/ (* a b) (gcd a b)))

(defn lcmv [& v] (reduce lcm v))

; part2 solution
(lcmv 231614 286332 22958)
;; => 380635029877596
