(ns clj-adventofcode-2019.day14
  (:require
   [clojure.java.io :as io]
   ; [clojure.math.numeric-tower :as math]
   [clojure.string :as str]
   ))

(defn parse-chemical [chemical-str]
  (let [q-and-n (str/split (str/trim chemical-str) #" ")
        quantity (read-string (first q-and-n))
        name (keyword (second q-and-n))]
    [name quantity]))
       

(defn parse-input-line [line]
  (let [[in out] (str/split line #"=>")
        in-chemicals (into {} (map parse-chemical (str/split in #",")))
        out-chemical (into {} [(parse-chemical out)])]
    { :input in-chemicals :output out-chemical}))
;; => #'clj-adventofcode-2019.day13/parse-input-line



(comment
  (parse-input-line "1 KWFD, 1 TVJPG => 8 VCMVN")
  ;; => {:input {:KWFD 1, :TVJPG 1}, :output {:VCMVN 8}}
)

(def list-of-reactions (map parse-input-line (str/split-lines (slurp (io/resource "day14_input.txt")))))


(def sample1-reactions
  (map 
   parse-input-line 
   ["10 ORE => 10 A"
    "1 ORE => 1 B"
    "7 A, 1 B => 1 C "
    "7 A, 1 C => 1 D"
    "7 A, 1 D => 1 E "
    "7 A, 1 E => 1 FUEL"]
   ))

(def sample2-reactions
  (map 
   parse-input-line
   ["9 ORE => 2 A"
    "8 ORE => 3 B"
    "7 ORE => 5 C"
    "3 A, 4 B => 1 AB"
    "5 B, 7 C => 1 BC"
    "4 C, 1 A => 1 CA"
    "2 AB, 3 BC, 4 CA => 1 FUEL"])
  )

(defn get-reaction-for-chemical [chemical reactions]
   (first (filter (fn [r] (contains? (:output r) chemical)) reactions)))

(defn calc-mult-factor
  ([minimum requested] (calc-mult-factor minimum requested 1))
  ([minimum requested factor]
   (if (<= requested (* factor minimum)) factor
       (recur minimum requested (inc factor)))))


(defn calc-ore
  ([reactions] (calc-ore reactions (list [:FUEL 1]) {} 0))
  ([reactions fuel-count] (calc-ore reactions (list [:FUEL fuel-count]) {} 0))
  ([reactions needed-chemicals spares ore-count]
   ;(println (str "needed: " needed-chemicals " spares:" spares " ore:" ore-count))
   (if (empty? needed-chemicals)
     {:ORE ore-count :spares spares}
     (let [[chemical quantity] (first needed-chemicals)
           spare-count (get spares chemical 0)]
       (cond
         (= chemical :ORE) (recur reactions (pop needed-chemicals) spares (+ ore-count quantity))
         (> spare-count 0)
         (cond
           (< quantity spare-count) (recur reactions
                                           (pop needed-chemicals)
                                           (assoc spares chemical (- spare-count quantity))
                                           ore-count)
           (= quantity spare-count) (recur reactions
                                           (pop needed-chemicals)
                                           (dissoc spares chemical)
                                           ore-count)
           (> quantity spare-count) (recur reactions
                                           (conj (pop needed-chemicals) [chemical (- quantity spare-count)])
                                           (dissoc spares chemical)
                                           ore-count))
         :else
         (let [reaction (get-reaction-for-chemical chemical reactions)
               min-reaction-quantity (get (:output reaction) chemical)
               mult-factor (calc-mult-factor min-reaction-quantity quantity)
               spare-count (- (* mult-factor min-reaction-quantity) quantity)
               new-needed-chemicals (map (fn [[ch c]] [ch (* mult-factor c)]) (:input reaction))
               ]
           (recur reactions
                  (into (pop needed-chemicals) new-needed-chemicals)
                  (if (> spare-count 0) (assoc spares chemical spare-count) spares)
                  ore-count
                  )
           
           ))))))

(comment
  (calc-ore sample1-reactions)
  ;; => {:ORE 31, :spares {:A 2}}

  (calc-ore sample2-reactions)
  ;; => {:ORE 165, :spares {:C 3, :B 1}}

  (def sample3-reactions
    (map parse-input-line ["157 ORE => 5 NZVS"
                           "165 ORE => 6 DCFZ"
                           "44 XJWVT, 5 KHKGT, 1 QDVJ, 29 NZVS, 9 GPVTF, 48 HKGWZ => 1 FUEL"
                           "12 HKGWZ, 1 GPVTF, 8 PSHF => 9 QDVJ"
                           "179 ORE => 7 PSHF"
                           "177 ORE => 5 HKGWZ"
                           "7 DCFZ, 7 PSHF => 2 XJWVT"
                           "165 ORE => 2 GPVTF"
                           "3 DCFZ, 7 NZVS, 5 HKGWZ, 10 PSHF => 8 KHKGT"]))


  (calc-ore sample3-reactions)
  ;; => {:ORE 13312, :spares {:QDVJ 8, :KHKGT 3, :NZVS 4, :PSHF 3, :DCFZ 5}}

  (def sample4-reactions
    (map parse-input-line ["2 VPVL, 7 FWMGM, 2 CXFTF, 11 MNCFX => 1 STKFG"
                           "17 NVRVD, 3 JNWZP => 8 VPVL"
                           "53 STKFG, 6 MNCFX, 46 VJHF, 81 HVMC, 68 CXFTF, 25 GNMV => 1 FUEL"
                           "22 VJHF, 37 MNCFX => 5 FWMGM"
                           "139 ORE => 4 NVRVD"
                           "144 ORE => 7 JNWZP"
                           "5 MNCFX, 7 RFSQX, 2 FWMGM, 2 VPVL, 19 CXFTF => 3 HVMC"
                           "5 VJHF, 7 MNCFX, 9 VPVL, 37 CXFTF => 6 GNMV"
                           "145 ORE => 6 MNCFX"
                           "1 NVRVD => 8 CXFTF"
                           "1 VJHF, 6 MNCFX => 4 RFSQX"
                           "176 ORE => 6 VJHF"]))

  (calc-ore sample4-reactions)
  ;; => {:ORE 180697, :spares {:JNWZP 6, :NVRVD 1, :RFSQX 3, :VJHF 3, :GNMV 5, :VPVL 3, :MNCFX 2}}

  (def sample5-reactions
    (map parse-input-line ["171 ORE => 8 CNZTR"
                           "7 ZLQW, 3 BMBT, 9 XCVML, 26 XMNCP, 1 WPTQ, 2 MZWV, 1 RJRHP => 4 PLWSL"
                           "114 ORE => 4 BHXH"
                           "14 VRPVC => 6 BMBT"
                           "6 BHXH, 18 KTJDG, 12 WPTQ, 7 PLWSL, 31 FHTLT, 37 ZDVW => 1 FUEL"
                           "6 WPTQ, 2 BMBT, 8 ZLQW, 18 KTJDG, 1 XMNCP, 6 MZWV, 1 RJRHP => 6 FHTLT"
                           "15 XDBXC, 2 LTCX, 1 VRPVC => 6 ZLQW"
                           "13 WPTQ, 10 LTCX, 3 RJRHP, 14 XMNCP, 2 MZWV, 1 ZLQW => 1 ZDVW"
                           "5 BMBT => 4 WPTQ"
                           "189 ORE => 9 KTJDG"
                           "1 MZWV, 17 XDBXC, 3 XCVML => 2 XMNCP"
                           "12 VRPVC, 27 CNZTR => 2 XDBXC"
                           "15 KTJDG, 12 BHXH => 5 XCVML"
                           "3 BHXH, 2 VRPVC => 7 MZWV"
                           "121 ORE => 7 VRPVC"
                           "7 XCVML => 6 RJRHP"
                           "5 BHXH, 4 VRPVC => 5 LTCX"]))

  (calc-ore sample5-reactions)
  ;; => {:ORE 2210736,
  ;;     :spares
  ;;     {:BHXH 3,
  ;;      :WPTQ 1,
  ;;      :MZWV 4,
  ;;      :KTJDG 3,
  ;;      :XCVML 3,
  ;;      :RJRHP 1,
  ;;      :LTCX 1,
  ;;      :PLWSL 1,
  ;;      :VRPVC 5,
  ;;      :ZLQW 3,
  ;;      :XDBXC 1,
  ;;      :BMBT 1,
  ;;      :FHTLT 5}}
  )

; part 1 solution:

(calc-ore list-of-reactions)
;; => {:ORE 751038,
;;     :spares
;;     {:MHLBR 3,
;;      :CNBS 3,
;;      :VSFTG 2,
;;      :DNZQ 1,
;;      :HBXF 4,
;;      :KCMH 3,
;;      :TCKR 4,
;;      :CDSP 5,
;;      :QSNCW 1,
;;      :JNSP 3,
;;      :GDPKF 1,
;;      :VCWNW 5,
;;      :DBKN 5,
;;      :LPXM 1,
;;      :NTXHM 7,
;;      :XHLNT 3,
;;      :PGHF 3,
;;      :BDHX 5,
;;      :MPKC 2,
;;      :BWSPN 5,
;;      :FDVNC 4,
;;      :KWFD 1,
;;      :VSPSC 1,
;;      :NHSTB 2,
;;      :FNPC 1,
;;      :QSVN 2,
;;      :GRQC 2,
;;      :BXVJK 3,
;;      :CLZT 5,
;;      :BMVRK 2,
;;      :PZWSP 5,
;;      :CHDTJ 6,
;;      :GMQDW 1,
;;      :FDWSG 7,
;;      :RWDSX 2,
;;      :ZDGPL 5,
;;      :FVQM 4,
;;      :XHST 6,
;;      :WTPN 3,
;;      :FJVMD 3,
;;      :VCMVN 1,
;;      :MHBH 5,
;;      :SRWZ 1,
;;      :CXKN 7,
;;      :NMZND 7,
;;      :DPRL 2,
;;      :QMJK 3,
;;      :PFDBV 1}}


; -- part 2 ----
(def trillion 1000000000000)


(defn calc-maximum-amount-of-fuel [reactions max-ore start-from]
  (loop [fuel-count start-from
         ore-calc-result (calc-ore reactions (list [:FUEL start-from]) {} 0)]
    (let [ore-count (:ORE ore-calc-result)]
      (println (str "FUEL:" fuel-count " ORE:" ore-count))
      (if (>= ore-count max-ore) {:result ore-calc-result :fuel-count fuel-count}
          (recur (inc fuel-count)
                (calc-ore reactions (list [:FUEL 1]) (:spares ore-calc-result) ore-count) 
                 )))))

(calc-maximum-amount-of-fuel list-of-reactions trillion (+ 1331490N
                                                           (bigint (/ 1331490N 2))
                                                           (bigint (/ 1331490N 32))))

; solution: 2074843N



;(bigint (/ trillion 751038)) --> 1331490N


(calc-ore list-of-reactions 1331490N) ; -> 641 730 243 750

(calc-ore list-of-reactions (* 1331490N 2)); -> 1 283 460 368 877

(calc-ore list-of-reactions (+ 1331490N (bigint (/ 1331490N 2)))) ; -> 962 595 395 958 

(calc-ore list-of-reactions (+ 1331490N 
                               (bigint (/ 1331490N 2)) 
                               (bigint (/ 1331490N 32)))) ; -> 982 649 417 604

(calc-ore list-of-reactions (+ 1331490N
                               (bigint (/ 1331490N 2))
                               (bigint (/ 1331490N 16)))) ; -> 1 002 703 545 017


; (+ 1331490N (bigint (/ 1331490N 2)) (bigint (/ 1331490N 32))) ; 2038844N


;; ------------------------------------------------------------------------------
;; keep this for historical reasons... the first two attempts were not went well... :(

; (comment
;  (get-reaction-for-chemical :A sample1-reactions)
;  ;; => {:input {:ORE 10}, :output {:A 10}}
;   )


; (defn revert-reaction-for [chemical quantity reactions]
;   (let [reaction (get-reaction-for-chemical chemical reactions)
;         min-reaction-quantity (get (:output reaction) chemical)
;         input (:input reaction)
;         mult-factor (calc-mult-factor min-reaction-quantity quantity)
;         used-quantity (* min-reaction-quantity mult-factor)
;         waste-count (- used-quantity quantity)]
;     {:input (into {} (map (fn [[k v]] [k (* mult-factor v)]) input))
;      :waste {chemical waste-count}}))


; (defn revert-reaction-with-leftover [chemical quantity reactions]
;   ;(println (str "revert-reaction-with-leftover" chemical quantity))
;   (let [reaction (get-reaction-for-chemical chemical reactions)
;         min-reaction-quantity (get (:output reaction) chemical)
;         quotient (quot quantity min-reaction-quantity)
;         ;leftover (mod quantity min-reaction-quantity)
;         input (:input reaction)]
;     (into {chemical (* -1 (* quotient min-reaction-quantity))} (map (fn [[k v]] [k (* quotient v)]) input))
;     ))

; (defn remove-zeros [state]
;   (into {} (filter (fn [[_ v]] (> v 0)) state)))

; (comment
;   (remove-zeros {:A 10 :B 0 :C 1 :D 0})
;   ;; => {:A 10, :C 1}
;   )


; (defn apply-transformation [state transformation]
;   (remove-zeros (into state (for [[ch qu] transformation]
;                 [ch (+ (get state ch 0) qu)]))))

; (comment
;   (apply-transformation {:A 10 :B 20 :C 5} {:A -5 :C -5})
;   ;; => {:A 5, :B 20}
;   )

; (defn filter-not-ore [state]
;   (filter (fn [[c _]] (not= c :ORE)) state)
;   )


; (defn rec-reaction-revert [state reactions]
;   (let [transformations (for [[ch qu] (filter-not-ore state)]
;                           (revert-reaction-with-leftover ch qu reactions))
;         new-state (reduce apply-transformation state transformations)]
;     ;(println new-state transformations)
;     (if (= state new-state) state
;         (recur new-state reactions))))

; (comment
;   (rec-reaction-revert {:FUEL 1} sample1-reactions)
;   ;; => {:A 8, :ORE 21}
;   (rec-reaction-revert {:FUEL 1} sample2-reactions)
;   ;; => {:B 2, :C 2, :ORE 150}

;   (rec-reaction-revert {:FUEL 1} list-of-reactions)
;   ;; => {:CNBS 3,
;   ;;     :HVJVQ 1,
;   ;;     :HBXF 1,
;   ;;     :KCMH 6,
;   ;;     :CDSP 5,
;   ;;     :QSNCW 1,
;   ;;     :JNSP 7,
;   ;;     :GDPKF 3,
;   ;;     :CNTFK 1,
;   ;;     :VCWNW 1,
;   ;;     :DBKN 3,
;   ;;     :ORE 400427,
;   ;;     :NTXHM 4,
;   ;;     :XHLNT 1,
;   ;;     :DLHG 2,
;   ;;     :ZGSFW 1,
;   ;;     :BWSPN 3,
;   ;;     :FDVNC 5,
;   ;;     :NHSTB 4,
;   ;;     :FNPC 5,
;   ;;     :QSVN 7,
;   ;;     :GRQC 3,
;   ;;     :BXVJK 2,
;   ;;     :CLZT 1,
;   ;;     :BMVRK 4,
;   ;;     :PZWSP 6,
;   ;;     :CHDTJ 2,
;   ;;     :GMQDW 3,
;   ;;     :FDWSG 1,
;   ;;     :RWDSX 1,
;   ;;     :ZDGPL 4,
;   ;;     :FVQM 2,
;   ;;     :XHST 4,
;   ;;     :WTPN 3,
;   ;;     :FJVMD 3,
;   ;;     :VCMVN 3,
;   ;;     :MHBH 5,
;   ;;     :SRWZ 1,
;   ;;     :CXKN 2,
;   ;;     :NMZND 5,
;   ;;     :DPRL 2,
;   ;;     :QMJK 1}
;   )

; (defn missing-quantity-for-chemical [chemical quantity reactions]
;   (let [reaction (get-reaction-for-chemical chemical reactions)]
;     (- (get (:output reaction) chemical) quantity)))

; (defn calc-ore
;   ([reactions] (calc-ore reactions {:FUEL 1} #{} #{ {} }  []))
;   ([reactions initial-state dry work-queue results]
;    (if (empty? work-queue)
;      results
;      (let [additional-chemicals (first work-queue)
;            final-state (rec-reaction-revert (apply-transformation initial-state additional-chemicals) reactions)]
;        (when (= (mod (count dry) 1000) 0)
;          (println (str "!!!"
;                        " WQ:" (count work-queue)
;                        " AC:" (count additional-chemicals)
;                        " FS:" (count final-state)
;                        " DRY:" (count dry)
;                        " RES:" (count results))))
;        (if (contains? dry additional-chemicals)
;          (recur reactions initial-state dry (set (rest work-queue)) results)
;          (if (and (= 1 (count final-state)) (= (first (keys final-state)) :ORE))
;            ; this case is done, publish the result
;            (recur reactions initial-state (conj dry additional-chemicals) (set (rest work-queue)) (conj results [additional-chemicals (:ORE final-state)]))
;            ; add additional cases based on leftover chemicals
;            (recur reactions
;                   initial-state
;                   (conj dry additional-chemicals)
;                   (into (set (rest work-queue))
;                         (filter (fn [s] (not (contains? dry s)))
;                                 (for [[leftover-chemical quantity] (filter-not-ore final-state)]
;                                   (assoc additional-chemicals
;                                          leftover-chemical (missing-quantity-for-chemical
;                                                             leftover-chemical quantity reactions)))))
;                   results)))))))


; (comment
;   (calc-ore sample1-reactions)
;   ;; => [[{:A 2} 31]]

;   (calc-ore sample2-reactions)
;   ;; => [[{:B 1, :C 3} 165] [{:C 3, :B 1} 165]]
;   )

; (into (set (rest #{:a :b :c})) [:d :e :f])

; (calc-ore list-of-reactions)

; ;; => Execution error (StackOverflowError) at clj-adventofcode-2019.day13/get-reaction-for-chemical$fn (day14.clj:56).
; ;;    null


; ; (comment
; ;   (calc-mult-factor 3 1)
; ;   ;; => 1
; ;   (calc-mult-factor 3 5)
; ;   ;; => 2
; ;   (calc-mult-factor 3 10)
; ;   ;; => 4
; ;   (calc-mult-factor 3 12)
; ;   ;; => 4
; ; )

; ; (defn get-reaction-for-chemical [chemical reactions]
; ;   (first (filter (fn [r] (= chemical (second (:output r)))) reactions))
; ;   )

; ; (comment
; ;   (get-reaction-for-chemical :C sample1-reactions))

; ; (defn get-input-for-chemical [quantity chemical reactions]
; ;   (let [reaction (get-reaction-for-chemical chemical reactions)
; ;         min-out-quantity (first (:output reaction))
; ;         multiplication-factor (calc-mult-factor min-out-quantity quantity)
; ;         waste (filter (fn [x] (< 0 (first x))) [[(- (* multiplication-factor min-out-quantity) quantity) chemical]])
; ;         input (map (fn [[q n]] [(* multiplication-factor q) n])(:input reaction))
; ;         ]
; ;     {:reaction reaction :multiplication multiplication-factor :waste waste :input input}))

; ; (comment
; ;   (get-input-for-chemical 2 :C sample1-reactions)
; ;   ;; => {:reaction {:input [[7 :A] [1 :B]], :output [1 :C]}, :multiplication 2, :waste (), :input ([14 :A] [2 :B])}
; ;   (get-input-for-chemical 15 :A sample1-reactions)
; ;   ;; => {:reaction {:input [[10 :ORE]], :output [10 :A]}, :multiplication 2, :waste ([5 :A]), :input ([20 :ORE])}
; ;   (get-input-for-chemical 2 :CA sample2-reactions)
; ;   ;; => {:reaction {:input [[4 :C] [1 :A]], :output [1 :CA]}, :multiplication 2, :waste (), :input ([8 :C] [2 :A])}
; ;   )

; ; (defn get-ore-for-chemical [quantity chemical reactions]
; ;   (let [R (get-input-for-chemical quantity chemical reactions)
; ;         input (:input R)
; ;         waste (:waste R)
; ;         ]
; ;     (if (and (= (count input) 1)
; ;              (= (second (first input)) :ORE))
; ;       {:input input :waste waste}
; ;       (reduce (fn [a b] {:input (concat (:input a) (:input b))
; ;                          :waste (concat (:waste a) (:waste b))})
; ;               (for [i input] (get-ore-for-chemical (first i) (second i) reactions))))))

; ; (comment
; ;   (get-ore-for-chemical 10 :A sample1-reactions)
; ;   ;; => {:input ([10 :ORE]), :waste ()}

; ;   (get-ore-for-chemical 1 :C sample1-reactions)
; ;   ;; => {:input ([10 :ORE] [1 :ORE]), :waste ([3 :A])}

; ;   (get-ore-for-chemical 1 :D sample1-reactions)
; ;   ;; => {:input ([10 :ORE] [10 :ORE] [1 :ORE]), :waste ([3 :A] [3 :A])}

; ;   (get-ore-for-chemical 1 :FUEL sample1-reactions)
; ;   ;; => {:input ([10 :ORE] [10 :ORE] [10 :ORE] [10 :ORE] [1 :ORE]), :waste ([3 :A] [3 :A] [3 :A] [3 :A])}


; ;   )