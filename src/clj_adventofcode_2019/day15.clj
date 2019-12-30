(ns clj-adventofcode-2019.day15
  (:require
   [clojure.set]
   [clj-adventofcode-2019.intcode :as intcode]
   ))

(def robot-control-program (intcode/load-program-from-resource "day15_input.txt"))

(def directions {:north 1
                 :south 2
                 :west 3
                 :east 4})

(def status-codes {0 :wall
                   1 :moved
                   2 :moved-on-target
                   -1 :unknown})

(defn calc-new-pos [last-pos previous-direction]
  (case previous-direction
    :north [(first last-pos) (inc (second last-pos))]
    :south [(first last-pos) (dec (second last-pos))]
    :west [(dec (first last-pos)) (second last-pos)]
    :east [(inc (first last-pos)) (second last-pos)]))

(defn four-neighbours [coord]
  (for [d (keys directions)]
    (calc-new-pos coord d)))

(defn calc-direction [next-pos current-pos]
  (let [v [(- (first next-pos) (first current-pos))
           (- (second next-pos) (second current-pos))]]
    (case v
      [1 0] :east
      [-1 0] :west
      [0 1] :north
      [0 -1] :south
      :else (throw (AssertionError. (str "Wrong next-pos" next-pos current-pos))))))


(defn add-coordinate-to-area [area coordinate status]
  (let [additional-coordinates (if (or (= status :moved)
                                       (= status :moved-on-target)
                                       )
                                 (into [] (for [n (four-neighbours coordinate) :when (not (contains? area n))]
                                            [n :unknown]))
                                 [])]
    (into area (conj additional-coordinates [coordinate status]))
    )  
)

(comment
  (add-coordinate-to-area {} [0 0] :moved)
  ;; => {[0 1] :unknown, [0 -1] :unknown, [-1 0] :unknown, [1 0] :unknown, [0 0] :moved}

  (add-coordinate-to-area {[0 1] :unknown
                           [0 -1] :unknown
                           [-1 0] :unknown
                           [1 0] :unknown
                           [0 0] :moved}
                          [0 1] :wall)

  ;; => {[0 1] :wall, [0 -1] :unknown, [-1 0] :unknown, [1 0] :unknown, [0 0] :moved}
  )

(defn is-spot-reachable [spot]
  (let [[_ status] spot]
    (or (= status :moved)
        (= status :moved-on-target))
    ))

(defn only-reachable-spots-of [area]
  (filter is-spot-reachable area)
  )

(comment
  (only-reachable-spots-of {[0 1] :wall, [0 -1] :unknown, [-1 0] :unknown, [1 0] :unknown, [0 0] :moved})
;; => ([[0 0] :moved])
  )



(def infinity Integer/MAX_VALUE)
;; => #'clj-adventofcode-2019.day15/infinity


(defn shortest-path [coordinates source destination]
  (let
   [get-min-dist (fn [dist-map] (ffirst (sort-by (fn [[_ v]] (:dist v)) dist-map)))
    get-neighbours (fn [coordinate-set c] (clojure.set/intersection
                                           (set (four-neighbours c))
                                           coordinate-set))
    calc-dist-prev (fn [dist-map u n]
                     (let [new-dist (inc (:dist (get dist-map u)))
                           old-dist (:dist (get dist-map n))]
                       (if (< new-dist old-dist) {:dist new-dist :prev u}
                           (get dist-map n))))
    dijkstra-dist-map (loop [vertex-set (set coordinates)
                             dist-map (assoc (into {} (for [c coordinates]
                                                        [c {:dist infinity :prev :unknown}]))
                                             source {:dist 0 :prev :unknown})]
                        (if (empty? vertex-set)
                          dist-map
                          (let [u (get-min-dist (filter (fn [[k _]] (contains? vertex-set k)) dist-map))
                                new-vertex-set (disj vertex-set u)
                                neighbours (get-neighbours new-vertex-set u)
                                new-dist-map (into dist-map (for [n neighbours] [n (calc-dist-prev dist-map u n)]))]
                            ;(println (str u new-vertex-set neighbours new-dist-map))
                            (recur new-vertex-set new-dist-map))))

    build-path (fn [dist-map source destination path]
                 (if (= source destination) path
                     (recur dist-map source (:prev (get dist-map destination)) (conj path destination))))]
    (build-path dijkstra-dist-map source destination '())
    
    ))


(comment
  (shortest-path [[0 0]
                  [-1 0] [-2 0] [-2 1] [-2 2] [-2 3] [-1 3]
                  [0 1] [0 2] [0 3]
                  ]
                 [0 0]
                 [0 3])
  ;; => ([0 1] [0 2] [0 3])

  
  )



(defn distance-between [c1 c2]
  (+ (Math/abs (- (first c1) (first c2)))
     (Math/abs (- (second c1) (second c2)))))

(comment
  (distance-between [10 10] [5 5])
  ;; => 10
  (distance-between [1 0] [2 0])
  ;; => 1
  (distance-between [1 0] [2 1])
  ;; => 2
  )

(defn nearest-unknown-position [area current-pos]
  (let [all-unknown (map first (filter (fn [[_ v]] (= v :unknown)) area))]
    (first (sort-by (partial distance-between current-pos) all-unknown))))

(comment
  (nearest-unknown-position (add-coordinate-to-area {} [0 0] :moved) [0 0])
   ;; => [0 1]
  (nearest-unknown-position {} [0 0])
  ;; => nil
  )

(defn calc-next-direction [area current-pos]
  (let [target-position (nearest-unknown-position area current-pos)]
    (if (nil? target-position)
      nil
      (let [path-to-target (shortest-path (conj (map first (only-reachable-spots-of area)) target-position)
                                          current-pos
                                          target-position)
            next-pos (first path-to-target)]
        (calc-direction next-pos current-pos)))))



(comment
  (calc-next-direction (add-coordinate-to-area {} [0 0] :moved) [0 0])
  ;; => :north
  (calc-next-direction {[0 0] :moved} [0 0])
  ;; => nil
  (calc-next-direction {[0 0] :moved [0 1] :unknown} [0 0])
  ;; => :north
  (calc-next-direction {[0 0] :moved [0 -1] :unknown} [0 0])
  ;; => :south
  (calc-next-direction {[0 0] :moved [1 0] :unknown} [0 0])
  ;; => :east
  (calc-next-direction {[0 0] :moved [-1 0] :unknown} [0 0])
  ;; => :west
  )

(def initial-program-state (intcode/initialize-and-run robot-control-program))

(defn move-robot-until-target-found
  ([] (let [after-first-move
            (intcode/run (assoc initial-program-state
                                :input [(directions :north)]))]
        (move-robot-until-target-found after-first-move {[0 0] :moved} :north [0 0])))
  ([program-state area previous-direction last-pos]
   (let [status-code (get status-codes (first (:output program-state)))
         new-pos (calc-new-pos last-pos previous-direction)
         new-area (add-coordinate-to-area area new-pos status-code)
         current-pos (if (= status-code :wall) last-pos new-pos)
         new-direction (calc-next-direction new-area current-pos)]
     (if (nil? new-direction)
       {:area new-area :pos current-pos}
       (case status-code
         (:moved :wall) (recur (intcode/run (assoc program-state
                                                   :input [(directions new-direction)]
                                                   :output []))
                               new-area
                               new-direction
                               current-pos)


         :moved-on-target {:area new-area :pos current-pos})))))


(defn move-robot-to-every-spot
  ([] (let [after-first-move
            (intcode/run (assoc initial-program-state
                                :input [(directions :north)]))]
        (move-robot-to-every-spot after-first-move {[0 0] :moved} :north [0 0])))
  ([program-state area previous-direction last-pos]
   (let [status-code (get status-codes (first (:output program-state)))
         new-pos (calc-new-pos last-pos previous-direction)
         new-area (add-coordinate-to-area area new-pos status-code)
         current-pos (if (= status-code :wall) last-pos new-pos)
         new-direction (calc-next-direction new-area current-pos)]
     (if (nil? new-direction)
       {:area new-area :pos current-pos}
       (recur (intcode/run (assoc program-state
                                  :input [(directions new-direction)]
                                  :output []))
              new-area
              new-direction
              current-pos)))))

(def full-area-map (:area (move-robot-to-every-spot)))

(comment
  (def target-pos (first (filter (fn [[_ v]] (= v :moved-on-target)) full-area-map)))
  ;; => #'clj-adventofcode-2019.day15/target-pos
  target-pos
  ;; => [[-12 18] :moved-on-target]
  (count (shortest-path (map first (only-reachable-spots-of full-area-map)) [0 0] [-12 18]))
  ;; => 366  ; part 1 solution
  )

(comment
  ; part 2
  (def all-reachable-spots (map first (only-reachable-spots-of full-area-map)))

  (defn shortest-path-length-for [coordinates source destination]
    (count (shortest-path coordinates source destination)))

  (last (sort (for [spot all-reachable-spots] (shortest-path-length-for all-reachable-spots [-12 18] spot))))
  ;; => 384 ; part 2 solution
  )