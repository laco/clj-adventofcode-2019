(ns clj-adventofcode-2019.intcode
  (:require
   [clojure.string]
   [clojure.math.numeric-tower :as math]
   [clojure.java.io :as io]))


(def operations {1  :add
                 2  :mul
                 3  :in
                 4  :out
                 99 :exit
                 5  :jump-if-true
                 6  :jump-if-false
                 7  :less-than
                 8  :eq
                 9  :adjust-rel-base
                 })

(def number-of-params
  {
   :add             3
   :mul             3
   :in              1
   :out             1
   :exit            0
   :jump-if-true    2
   :jump-if-false   2
   :less-than       3
   :eq              3
   :adjust-rel-base 1
   })


(defn opcode-to-operation [opcode]
  (get operations (mod opcode 100))
  )


(defn parameter-mode-for-parameter [opcode param-num]
  (get {0 :position 1 :immediate 2 :relative}
       (mod (quot opcode
                  (* 10 (math/expt 10 param-num))) 10))
  )

(defn load-from-address [address state]
  (if (< address (count (:program state)))
    (nth (:program state) address)
    (get (:extra-memory state) address 0)))


(defn save-to-address [address value state]
  (if (< address (count (:program state)))
    (assoc-in state [:program address] value)
    (assoc-in state [:extra-memory address] value)))


(defn get-address [param-number opcode state]
  (let [position (:position state)
        param-mode (parameter-mode-for-parameter opcode param-number)]
    (case param-mode
      :immediate (+ position param-number)
      :position (load-from-address (+ position param-number) state)
      :relative (+ (:rel-base state) (load-from-address (+ position param-number) state))
      :else (throw (AssertionError. "Wrong param.")))
    ))


(defn get-param [param-number opcode state]
  (load-from-address (get-address param-number opcode state) state))



(defn initialize-program
  ([program] (initialize-program program []))
  ([program input]
   {
    :program      program
    :position     0
    :input        input
    :output       []
    :extra-memory {}
    :status       :running
    :rel-base     0
    }))


(defn exec-next-operation [state]
  (let [opcode (nth (:program state) (:position state))
        operation (opcode-to-operation opcode)
        num-of-params (number-of-params operation)
        next-position (inc (+ (:position state) num-of-params))
        ]
    (case operation
      (:add :mul) (let [first-param (get-param 1 opcode state)
                        second-param (get-param 2 opcode state)
                        calculation-fn (cond
                                         (= operation :add) +
                                         (= operation :mul) *
                                         :else (throw (AssertionError. "Wrong operation code.")))
                        result (calculation-fn first-param second-param)
                        destination-addr (get-address 3 opcode state)]
                    (assoc (save-to-address destination-addr result state)
                      :position next-position
                      ))
      :in (let [
                input (:input state)
                first-param (get-address 1 opcode state)]
            (if (empty? input)
              (assoc state :status :waiting-for-input)
              (assoc (save-to-address first-param (first input) state)
                :input (rest input)
                :status :running
                :position next-position
                )))
      :out (let [first-param (get-param 1 opcode state)]
             (assoc state
               :output (conj (:output state) first-param)
               :position next-position))
      :exit (assoc state :status :exited)

      (:jump-if-true :jump-if-false)
      (let [first-param (get-param 1 opcode state)
            second-param (get-param 2 opcode state)]
        (if (or
              (and (= operation :jump-if-true) (not= first-param 0))
              (and (= operation :jump-if-false) (= first-param 0))
              ) (assoc state :position second-param)
                (assoc state :position next-position)))

      (:less-than :eq)
      (let [first-param (get-param 1 opcode state)
            second-param (get-param 2 opcode state)
            third-param (get-address 3 opcode state)
            result ({
                     :less-than (if (< first-param second-param) 1 0)
                     :eq        (if (= first-param second-param) 1 0)} operation)]
        (assoc (save-to-address third-param result state)
          :position next-position
          ))

      :adjust-rel-base (let [first-param (get-param 1 opcode state)]
                         (assoc state
                           :rel-base (+ (:rel-base state) first-param)
                           :position next-position))
      )))


(defn run [state]
  (let [next-state (exec-next-operation state)]
    (if (not= (:status next-state) :running)
      next-state
      (recur next-state)
      )))


(defn initialize-and-run [& params]
  (run (apply initialize-program params)))


(defn load-program-from-resource [resource-name]
  (apply vector (map #(read-string (clojure.string/trim-newline %))
                     (clojure.string/split
                       (slurp (io/resource resource-name)) #","))))

