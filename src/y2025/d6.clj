(ns y2025.d6
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2025/day/6

;; Generator Logic

;; Solution Logic

;; Entry Points
(def ops {
    "*" * 
    "+" +
})
(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
    (map  #(-> (clojure.string/trim %) 
               (clojure.string/split #" +"))  (clojure.string/split-lines input)))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
(let [
        transformed (->>  (apply mapv vector input) 
                          (map (juxt peek (comp (partial map Integer/parseInt) pop))))] 
      (->>  (map (fn [[op  nums]]
                    (apply (ops op) nums)) transformed)
            (reduce +))))

(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input])

(def sample-data
"123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +  ")







;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
