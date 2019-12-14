(ns clj-adventofcode-2019.intcode
  (:require
    [clojure.math.numeric-tower :as math]
    ))


(def operations {1  :add
                 2  :mul
                 3  :in
                 4  :out
                 99 :exit
                 5  :jump-if-true
                 6  :jump-if-false
                 7  :less-than
                 8  :eq
                 })

(def number-of-params
  {
   :add           3
   :mul           3
   :in            1
   :out           1
   :exit          0
   :jump-if-true  2
   :jump-if-false 2
   :less-than     3
   :eq            3
   })


(defn opcode-to-operation [opcode]
  (get operations (mod opcode 100))
  )


(defn parameter-mode-for-parameter [opcode param-num]
  (get {0 :position 1 :immediate}
       (mod (quot opcode
                  (* 10 (math/expt 10 param-num))) 10))
  )


(defn get-param [param-number opcode program position]
  (let [param-mode (parameter-mode-for-parameter opcode param-number)]
    (case param-mode
      :immediate (nth program (+ position param-number))
      :position (nth program (nth program (+ position param-number)))
      :else (throw (AssertionError. "Wrong param."))
      )
    ))


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
    }))


(defn exec-next-operation [state]
  (let [
        program (:program state)
        position (:position state)
        opcode (nth (:program state) (:position state))
        operation (opcode-to-operation opcode)
        num-of-params (number-of-params operation)
        next-position (inc (+ (:position state) num-of-params))
        ]
    (case operation
      (:add :mul) (let [first-param (get-param 1 opcode program position)
                        second-param (get-param 2 opcode program position)
                        calculation-fn (cond
                                         (= operation :add) +
                                         (= operation :mul) *
                                         :else (throw (AssertionError. "Wrong operation code.")))
                        result (calculation-fn first-param second-param)
                        destination-addr (nth program (+ position 3))]
                    (assoc state
                      :program (assoc program destination-addr result)
                      :position next-position
                      ))
      :in (let [
                input (:input state)
                first-param (nth program (inc position))]
            (if (empty? input)
              (assoc state :status :waiting-for-input)
              (assoc state
                :program (assoc program first-param (first input))
                :input (rest input)
                :status :running
                :position next-position
                )))
      :out (let [first-param (get-param 1 opcode program position)]
             (assoc state
               :output (conj (:output state) first-param)
               :position next-position))
      :exit (assoc state :status :exited)

      (:jump-if-true :jump-if-false)
      (let [first-param (get-param 1 opcode program position)
            second-param (get-param 2 opcode program position)]
        (if (or
              (and (= operation :jump-if-true) (not= first-param 0))
              (and (= operation :jump-if-false) (= first-param 0))
              ) (assoc state :position second-param)
                (assoc state :position next-position)))

      (:less-than :eq)
      (let [first-param (get-param 1 opcode program position)
            second-param (get-param 2 opcode program position)
            third-param (nth program (+ 3 position))
            result ({
                     :less-than (if (< first-param second-param) 1 0)
                     :eq        (if (= first-param second-param) 1 0)} operation)]
        (assoc state
          :program (assoc program third-param result)
          :position next-position
          ))
      )))


(defn run [state]
  (let [next-state (exec-next-operation state)]
    (if (not= (:status next-state) :running)
      next-state
      (recur next-state)
      )))


(defn initialize-and-run [& params]
  (run (apply initialize-program params)))