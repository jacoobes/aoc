(ns y2025.d2
  (:require [clojure.test :as t :refer [deftest]]
            [clojure.string :as string]))

;; PROBLEM LINK https://adventofcode.com/2025/day/2

;; Generator Logic

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
  (->> (string/split (string/trim input) #",")
       (map #(string/split % #"-")) 
       (mapcat (fn [r] (range (Long/parseLong (first r))  (inc  (Long/parseLong (second r))) )))
  ))
(defn find-invalid-ids [arr]
    (map (fn [el]  (let [n (str el)
                         splitted-half (split-at (/ (count n) 2) n) ]
                (if (= (first splitted-half) (second splitted-half))
                    {:invalid true  :num (Long/parseLong n)}
                    {:invalid false :num (Long/parseLong n)}))), arr))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
    (->>  (find-invalid-ids input)
          (filter :invalid)
          (map :num)
          (apply +)))
(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]

)

(def sample-data "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124")

#_(solve-part-1 
    (generator sample-data))
;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
