(ns y2025.d3
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2025/day/3

;; Generator Logic

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
    (->>  (map seq (clojure.string/split-lines input))
          (map #(map str %))  
          #_(map #(map Integer/parseInt %))
          (map vec)))

(defn find-max-joltage [ar]
    ; create pairs of elements, like a double loop
    ; find biggest pair
    (->>  (for [[i x] (map-indexed vector ar) 
                [j y] (map-indexed vector ar)  
                :when (and (> j i) (not= i j)) ]
                (Integer/parseInt (str x y)))
           (apply max)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
    (->>  (map find-max-joltage input)
          (apply +)))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input])

(def sample-data
"987654321111111
811111111111119
234234234234278
818181911112111")
;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question
#_(find-max-joltage  (nth (generator sample-data) 3 ))

#_(solve-part-1 (generator sample-data))

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
