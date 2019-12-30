(ns clj-adventofcode-2019.day16)

(defn digits [n]
  (->>
   n
   str
   (map
    (comp read-string str))))

(digits 123456)

(def my-input (str "59782619540402316074"
                   "78302218034684759368"
                   "37571229433076679762"
                   "20344797950034514416"
                   "91877877658504052795"
                   "53538057343218254955"
                   "34399127207245390950"
                   "62973365881491407265"
                   "71457118013850022826"
                   "30494752854444244301"
                   "16922392127584449789"
                   "23612715040961674807"
                   "07096198155369207586"
                   "70506795611260008846"
                   "06348302062331309952"
                   "98022405587358756907"
                   "59302769424040089000"
                   "32118417964877701733"
                   "57003673931768403098"
                   "80824397712924986707"
                   "65812002897452795532"
                   "89300165042557391962"
                   "34042446213979992396"
                   "61623953690503728748"
                   "51854914571896058891"
                   "96438407777301912099"
                   "33860249608456231207"
                   "68409036628948085303"
                   "15202972278888943670"
                   "88102095139829881625"
                   "90896085150414396795"
                   "10475597764135250152"
                   "2955134675"))

(def base-pattern [0 1 0 -1])

(defn list-replicate [num list]
  (mapcat (partial repeat num) list))

(digits my-input)

(comment
  (list-replicate 2 base-pattern)
  ;; => (0 0 1 1 0 0 -1 -1)
  (take 10 (cycle (list-replicate 2 base-pattern)))
  ;; => (0 0 1 1 0 0 -1 -1 0 0)
  
)

(defn
  get-pattern-value-for [output-digit-num input-digit-num]
  (if (< input-digit-num output-digit-num) 0
      (let [pattern-list (cycle (list-replicate output-digit-num base-pattern))]
        (nth pattern-list input-digit-num))))

(def memo-get-pattern-value-for (memoize get-pattern-value-for))

(comment
  (get-pattern-value-for 1 1)
  ;; => 1
  
  (get-pattern-value-for 1 2)
  ;; => 0
  
  (get-pattern-value-for 1 3)
  ;; => -1
  
  (get-pattern-value-for 1 4)
  ;; => 0
  
  (get-pattern-value-for 2 1)
  ;; => 0
  
  (get-pattern-value-for 2 2)
  ;; => 1
  
  (get-pattern-value-for 2 3)
   ;; => 1
  
  (get-pattern-value-for 2 4)
  ;; => 0
    
  (get-pattern-value-for 3 2)
  ;; => 0

  )

(defn calc-out-digit [input-signal out-digit-number]
  (let [elements-for-sum (map-indexed (fn [idx item]
                                        (* (memo-get-pattern-value-for out-digit-number(inc idx) ) item)) input-signal)]
    (rem (Math/abs (reduce + elements-for-sum)) 10)))





(comment
  (calc-out-digit (digits "12345678") 2)
  ;; => 8
  )

(defn calc-next-phase [input-signal]
  (println (str "calc-next-phase" (take 8 input-signal)))
  (for [odigit (range 1 (inc (count input-signal)))]
    (calc-out-digit input-signal odigit)
    )
  )
;; => #'clj-adventofcode-2019.day16/calc-next-phase


(comment
  (calc-next-phase (digits "12345678"))
  ;; => (4 8 2 2 6 1 5 8)
  
  (time (calc-next-phase (digits my-input)))

  (nth (iterate calc-next-phase (digits my-input)) 1)
  )

(def part1-solution (nth (iterate calc-next-phase (digits my-input)) 100))

(comment
  (take 8 part1-solution)
  ;; => (2 7 2 2 9 2 6 9)
  )

; part 2


(def message-offset (read-string (subs my-input 0 7)))


(- (* 650 10000) message-offset)
;; => 521739

(def part2-zero-phase (vec (drop message-offset 
      (take (* (count (digits my-input)) 10000)
      (cycle (digits my-input))))))
;; => #'clj-adventofcode-2019.day16/part2-zero-phase


(def count-of-digits (count part2-zero-phase))

(comment
  message-offset
  ;; => 5978261

  count-of-digits
  ;; => 521739
  )

; (defn part2-digit-after-one-phase [prev-phase-digits digit-pos]
;   (let [num-of-prev-digits-needed (- count-of-digits digit-pos)]
;     (rem (reduce + (subvec prev-phase-digits (- count-of-digits num-of-prev-digits-needed))) 10)))


; (comment
;   (def tmp
;     (vec
;      (for [i (range 10000)]
;        (part2-digit-after-one-phase part2-zero-phase i))))
;   )

(defn reverse-sum-up-digits [prev-phase]
  (loop [idx (dec (count prev-phase)) res '() acc 0]
    (if (< idx 0)
      (vec res)
      (let [new-acc (rem (+ acc (nth prev-phase idx)) 10)]
        (recur (dec idx)
               (conj res new-acc)
               new-acc)))))
      
(comment
  (time (doall (reverse-sum-up-digits part2-zero-phase))))

(def part2-solution (nth (iterate reverse-sum-up-digits part2-zero-phase) 100))

(take 8 part2-solution) ; => (2 6 8 5 7 1 6 4)