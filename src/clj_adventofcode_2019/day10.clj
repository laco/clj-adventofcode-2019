(ns clj-adventofcode-2019.day10
  (:require [clojure.java.io :as io]
            [clojure.math.numeric-tower :as math]))


(def asteroid-map (slurp (io/resource "day10_input.txt")))

(defn asteroid-map-to-coordinates [asteroids]
  (let [lines (map-indexed vector (clojure.string/split-lines asteroids))]
    (map (fn [v] [(first v) (second v)])
         (filter (fn [v] (nth v 2))
                 (apply concat
                        (map (fn [line-vec]
                               (let [line-index (first line-vec)
                                     line-str (second line-vec)
                                     ]
                                 (map
                                   (fn [col-vector]
                                     (let [col-index (first col-vector)
                                           char (second col-vector)
                                           has-asteroid? (= char \#)
                                           ]
                                       [col-index line-index has-asteroid?])
                                     )
                                   (map-indexed vector line-str))
                                 ))
                             lines)
                        )))
    ))

(def sample-map-1 ".#..#\n.....\n#####\n....#\n...##")

(asteroid-map-to-coordinates sample-map-1)

; IDEA: Vectors u and v are in same direction
; if their unit norm are equal ie vectors are scalar multiple of each other.

(first (asteroid-map-to-coordinates sample-map-1))

(defn vector-between [[x1 y1] [x2 y2]]
  [(- x2 x1) (- y2 y1)]
  )

(first (asteroid-map-to-coordinates sample-map-1))
(second (asteroid-map-to-coordinates sample-map-1))

(vector-between (first (asteroid-map-to-coordinates sample-map-1))
                (second (asteroid-map-to-coordinates sample-map-1)))

(defn length-of-vector [[x y]] (bigdec (math/sqrt (+ (math/expt x 2) (math/expt y 2)))))


(defn norm-vector [[x y]]
  (with-precision 6
    (let [length (length-of-vector [x y])]
      [(/ x length) (/ y length)])))

(defn count-of-unique-norm-vectors [selected-point all-points]
  (let [other-points (filter (fn [p] (not= p selected-point)) all-points)
        vectors (map (partial vector-between selected-point) other-points)
        norm-vectors (map norm-vector vectors)]
    (count (set norm-vectors))))


(defn collect-all-unique-norm-vector-counts [all-points]
  (into {}
        (map (fn [selected-point]
               [selected-point (count-of-unique-norm-vectors selected-point all-points)])
             all-points)))

(defn find-best-point [all-points]
  (let [norm-vector-counts (collect-all-unique-norm-vector-counts all-points)]
    (apply max-key val norm-vector-counts)
    ))


(assert (= (find-best-point (asteroid-map-to-coordinates sample-map-1))
           [[3 4] 8]))


(def sample-map-2 "......#.#.\n#..#.#....\n..#######.\n.#.#.###..\n.#..#.....\n..#....#.#\n#..#....#.\n.##.#..###\n##...#..#.\n.#....####")
(assert (= (find-best-point (asteroid-map-to-coordinates sample-map-2))
           [[5 8] 33]))


(def sample-map-3 "#.#...#.#.\n.###....#.\n.#....#...\n##.#.#.#.#\n....#.#.#.\n.##..###.#\n..#...##..\n..##....##\n......#...\n.####.###.")
(assert (= (find-best-point (asteroid-map-to-coordinates sample-map-3))
           [[1 2] 35]))


(def sample-map-4 ".#..#..###\n####.###.#\n....###.#.\n..###.##.#\n##.##.#.#.\n....###..#\n..#.#..#.#\n#..#.#.###\n.##...##.#\n.....#.#..")
(assert (= (find-best-point (asteroid-map-to-coordinates sample-map-4))
           [[6 3] 41]))


(def sample-map-5 ".#..##.###...#######\n##.############..##.\n.#.######.########.#\n.###.#######.####.#.\n#####.##.#.##.###.##\n..#####..#.#########\n####################\n#.####....###.#.#.##\n##.#################\n#####.##.###..####..\n..######..##.#######\n####.##.####...##..#\n.#####..#.######.###\n##...#.##########...\n#.##########.#######\n.####.#.###.###.#.##\n....##.##.###..#####\n.#.#.###########.###\n#.#.#.#####.####.###\n###.##.####.##.#..##")
(assert (= (find-best-point (asteroid-map-to-coordinates sample-map-5))
           [[11 13] 210]))

(def part1-solution (find-best-point (asteroid-map-to-coordinates asteroid-map)))


; --- part2 ----

(def station-point (first (find-best-point (asteroid-map-to-coordinates asteroid-map))))

(defn vector-radius [[x y]] (Math/atan2 x y))

(def pi (vector-radius [0 -1]))

(defn calc-point-properties [station-point other-point]
  (let [vector (vector-between station-point other-point)
        inv-vector (map (partial * -1) vector)
        length (length-of-vector vector)
        radius (vector-radius vector)
        inv-vector-radius (vector-radius inv-vector)
        norm (norm-vector vector)]
    {:point  other-point
     :vector vector
     :length length
     :radius radius
     :inv-vec-radius inv-vector-radius
     :clockwise (if (< (first vector) 0)
                  (+ pi (math/abs radius))
                  (math/abs inv-vector-radius)
                  )
     :norm   norm}))


(defn order-visible-points-clockwise [selected-point all-points]
  (let [other-points (filter (fn [p] (not= p selected-point)) all-points)
        points-with-props (map (partial calc-point-properties selected-point) other-points)
        keep-just-the-closest (fn [m] (for [[_ v] m] (first (sort-by :length v))))
        grouped-points-with-props (group-by :norm points-with-props)]
    (sort-by :clockwise (keep-just-the-closest grouped-points-with-props))))


(def part2-200th-point 
  (nth (order-visible-points-clockwise station-point 
                                       (asteroid-map-to-coordinates asteroid-map)) 
       199))
