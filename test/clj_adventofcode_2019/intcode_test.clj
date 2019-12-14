(ns clj-adventofcode-2019.intcode-test
  (:require [clojure.test :refer :all]
            [clj-adventofcode-2019.intcode :refer :all]
            [clojure.math.numeric-tower :as math]))


(deftest day-02
  (testing "Addition"
    (let [result (run (initialize-program [1, 0, 0, 0, 99]))]
      (is (= (:position result) 4))
      (is (= (:program result) [2 0 0 0 99]))
      (is (= (:status result) :exited))))

  (testing "Multiplication"
    (let [result (run (initialize-program [2 3, 0, 3, 99]))]
      (is (= (:position result) 4))
      (is (= (:program result) [2 3 0 6 99]))
      (is (= (:status result) :exited))))

  (testing "Advanced multiplication"
    (let [result (run (initialize-program [2, 4, 4, 5, 99, 0]))]
      (is (= (:program result) [2, 4, 4, 5, 99, 9801]))
      (is (= (:status result) :exited))))

  (testing "Combined"
    (is (= (:program (initialize-and-run [1, 1, 1, 4, 99, 5, 6, 0, 99]))
           [30, 1, 1, 4, 2, 5, 6, 0, 99])))
  )


(deftest day-05-param-modes
  (testing "Position mode - equal to"
    (let [program [3, 9, 8, 9, 10, 9, 4, 9, 99, -1, 8]]
      (is (= (:output (initialize-and-run program [7]))
             [0]))
      (is (= (:output (initialize-and-run program [8]))
             [1]))))

  (testing "Position mode - less than"
    (let [program [3, 9, 7, 9, 10, 9, 4, 9, 99, -1, 8]]
      (is (= (:output (initialize-and-run program [7]))
             [1]))
      (is (= (:output (initialize-and-run program [8]))
             [0]))))

  (testing "immediate mode - equal to"
    (let [program [3, 3, 1108, -1, 8, 3, 4, 3, 99]]
      (is (= (:output (initialize-and-run program [9]))
             [0]))
      (is (= (:output (initialize-and-run program [8]))
             [1]))))

  (testing "immediate mode - less than"
    (let [program [3, 3, 1107, -1, 8, 3, 4, 3, 99]]
      (is (= (:output (initialize-and-run program [7]))
             [1]))
      (is (= (:output (initialize-and-run program [8]))
             [0]))))
  )

(deftest day-05-jumps
  (testing "position mode jump"
    (let [program [3, 12, 6, 12, 15, 1, 13, 14, 13, 4, 13, 99, -1, 0, 1, 9]]
      (is (= (:output (initialize-and-run program [0]))
             [0]))
      (is (= (:output (initialize-and-run program [1]))
             [1]))
      (is (= (:output (initialize-and-run program [2]))
             [1]))
      ))

  (testing "immediate mode jump"
    (let [program [3, 3, 1105, -1, 9, 1101, 0, 0, 12, 4, 12, 99, 1]]
      (is (= (:output (initialize-and-run program [0]))
             [0]))
      (is (= (:output (initialize-and-run program [1]))
             [1]))
      (is (= (:output (initialize-and-run program [2]))
             [1]))
      ))
  )

(deftest day-05-larger-example
  (let [program [3, 21, 1008, 21, 8, 20, 1005, 20, 22, 107, 8, 21, 20, 1006, 20, 31,
                 1106, 0, 36, 98, 0, 0, 1002, 21, 125, 20, 4, 20, 1105, 1, 46, 104,
                 999, 1105, 1, 46, 1101, 1000, 1, 20, 4, 20, 1105, 1, 46, 98, 99]]
    (testing "output 999 if the input value is below 8"
      (is (= (:output (initialize-and-run program [7]))
             [999]))
      )
    (testing "output 1000 if the input value is equal to 8"
      (is (= (:output (initialize-and-run program [8]))
             [1000]))
      )

    (testing "output 1001 if the input value is greater than 8"
      (is (= (:output (initialize-and-run program [9]))
             [1001]))
      )))

(deftest day-09-relative-mode-plus-extra-mem
  (testing "takes no input and produces a copy of itself as output"
    (let [program [109 1 204 -1 1001 100 1 100 1008 100 16 101 1006 101 0 99]
          result (initialize-and-run program)]
      (is (= (:output result) program))))

  (testing "should output a 16-digit number"
    (let [program [1102, 34915192, 34915192, 7, 4, 7, 99, 0]]
      (is (> (first (:output (initialize-and-run program))) (math/expt 10 15)))
      )
    )

  (testing "should output the large number in the middle"
    (is (= [1125899906842624] (:output (initialize-and-run [104, 1125899906842624, 99])))))
  )