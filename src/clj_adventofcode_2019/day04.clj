(ns clj-adventofcode-2019.day04)

;--- Day 4: Secure Container ---
;You arrive at the Venus fuel depot only to discover it's protected by a password. The Elves had written the password on a sticky note, but someone threw it out.
;
;However, they do remember a few key facts about the password:
;
;It is a six-digit number.
;The value is within the range given in your puzzle input.
;Two adjacent digits are the same (like 22 in 122345).
;Going from left to right, the digits never decrease; they only ever increase or stay the same (like 111123 or 135679).
;Other than the range rule, the following are true:
;
;111111 meets these criteria (double 11, never decreases).
;223450 does not meet these criteria (decreasing pair of digits 50).
;123789 does not meet these criteria (no double).
;How many different passwords within the range given in your puzzle input meet these criteria?
;
;Your puzzle input is 134564-585159.

(def all-six-digit-numbers-in-range (range 134564 585159))

(defn digits [n]
  (->>
    n
    str
    (map
      (comp read-string str))))


(defn two-adjacent-digits-are-the-same [digits]
  (let [adjacent-digits (partition 2 1 digits)]
    (some (fn [[a b]] (= a b)) adjacent-digits)))


(defn left-to-right-no-decrease [digits]
  (let [adjacent-digits (partition 2 1 digits)]
    (every? (fn [[a b]] (<= a b)) adjacent-digits)))

(def possible-passwords (filter
  (fn [number]
    (let [d (digits number)]
      (and
        (two-adjacent-digits-are-the-same d)
        (left-to-right-no-decrease d))
      ))
  all-six-digit-numbers-in-range))

(def part1-solution (count possible-passwords))


;--- Part Two ---
;An Elf just remembered one more important detail: the two adjacent matching digits are not part of a larger group of matching digits.
;
;Given this additional criterion, but still ignoring the range rule, the following are now true:
;
;112233 meets these criteria because the digits never decrease and all repeated digits are exactly two digits long.
;123444 no longer meets the criteria (the repeated 44 is part of a larger group of 444).
;111122 meets the criteria (even though 1 is repeated more than twice, it still contains a double 22).
;How many different passwords within the range given in your puzzle input meet all of the criteria?
;
;Your puzzle input is still 134564-585159.

(defn fit-into-group? [group digit]
  (or (empty? group) (= (first group) digit))
  )

(defn split-to-same-number-groups
  ([digits] (split-to-same-number-groups [] [] digits))
  ([groups current-group digits]
   (if (empty? digits)
     (conj groups current-group)
     (let [
           current-digit (first digits)]
       (if (fit-into-group? current-group current-digit)
         (recur groups (conj current-group current-digit) (rest digits))
         (recur (conj groups current-group) [current-digit] (rest digits)))))))

(defn has-two-large-same-number-group? [digits]
  (let [same-number-groups (split-to-same-number-groups digits)]
    (some (fn [group] (= 2 (count group))) same-number-groups)
    ))

(def part2-solution (count (filter has-two-large-same-number-group? (map digits possible-passwords))))