(ns y2025.d1
  (:require [clojure.test :as t :refer [deftest]]
            [ clojure.string :as string]))
;; PROBLEM LINK https://adventofcode.com/2025/day/1

;; Generator Logic

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
    (->> (clojure.string/split-lines input)
         (map (fn [el] { :dir (first el) :num (Integer/parseInt  (clojure.string/join  (rest el)))  }))))

; if L, i need to go backwards so subtract 100, but also loop around if it exceeds 100
; if R , i need to increment so + 100 but also loop around if it exceeds 100
(defn calc-new-num-p1 [ch prev n]
    (if (= ch \L)
        (let [diff (- prev n)]
            (cond 
                (zero? (rem (Math/abs diff) 100)) 0
                (and (neg? diff) (> diff -99)) (- 100 (rem  (Math/abs diff) 100)) 
                :else diff)) 
        (let [addition (+ prev n)]
            (cond 
                (zero? (rem addition 100) ) 0
                (> addition 99) (rem addition 100)
                :else addition)
            )))

(defn calc-new-num-p2 [ch prev n]
    (if (= ch \L)
        (let [diff (- prev n)]
            (cond 
                (zero? (rem (Math/abs diff) 100)) { :num  0 :crossed (Integer/parseInt (str  (first (str (Math/abs diff))))) }
                (and (neg? diff) (> diff -99)) {:num  (- 100 (rem  (Math/abs diff) 100)) :crossed 1} 
                :else {:num  diff :crossed 0})) 
        (let [addition (+ prev n)]
            (cond 
                (zero? (rem addition 100)) {:num  0 :crossed (Integer/parseInt (str (first (str (Math/abs  addition)))))}
                (> addition 99) {:num (rem addition 100) :crossed (Integer/parseInt (str (first (str addition))))} 
                :else {:num  addition :crossed 0})
            )))
; L99  0
(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
    (->> 
        (reduce (fn [acc cur] 
            (cond 
                (= (:dir cur) \L) (conj acc (calc-new-num-p1 \L (last acc) (:num cur)))
                (= (:dir cur) \R) (conj acc (calc-new-num-p1 \R (last acc) (:num cur))))) [50] input)
        (filter zero?)
        (count)))
(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
    (->> 
        (reduce (fn [acc cur] 
            (cond 
                (= (:dir cur) \L) (conj acc (calc-new-num-p2 \L (->>  (last acc) :num) (:num cur)))
                (= (:dir cur) \R) (conj acc (calc-new-num-p2 \R (->>  (last acc) :num) (:num cur))))) [{:num  50 :crossed 0}] input)
        (filter #(->> % :crossed pos?) )
        (map :crossed) 
        (apply +)
        ))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question
(def sample-data 
"L68
L30
R48
L5
R60
L55
L1
L99
R14
L82")


(solve-part-2 
    (generator sample-data))

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
