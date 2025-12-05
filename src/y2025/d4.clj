(ns y2025.d4
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2025/day/4

;; Generator Logic

;; Solution Logic

;; Entry Points

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
    (->>  (clojure.string/split-lines input) (map vec) vec))

(def offset [ [-1 0]
              [-1 -1]  
              [-1 1]
              [1 0]
              [1 -1]
              [1 1]
              [0 1]
              [0 -1]])

(defn get-removable [input]
   (->>  (for [[i row]  (vec  (map-indexed vector input))
          [j el]   (map-indexed vector row) 
                  :when (= el \@) 
                  :let [adjacent (map #(get-in input [(+ i (first %)) (+ j (second %))] ) offset)]]
                  { :pos [i j] :adj adjacent})
         (map (fn [mp] (assoc mp :adj (filter  #(= % \@) (:adj mp) )) ))
         (filter (fn [mp] (< (count (:adj mp))  4)))))


(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
    (->> ;can be refactored to use get-removable 
        (get-removable input)
        (count)))

(defn mark-remove [acc cur]
   (assoc-in acc (:pos cur) \.))
        
(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
  (loop [grid input
         adjacent-rolls (get-removable grid)
         total (count adjacent-rolls) ] 
        (if (zero?  (count adjacent-rolls))
            total
            (let [updated-grid (reduce mark-remove grid adjacent-rolls)
                  adjacent-updated (get-removable updated-grid) ] 
                (recur  updated-grid adjacent-updated (+ total (count adjacent-updated)))))))

(def sample-data 
"..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

#_(solve-part-1 (generator sample-data))
#_(solve-part-2 (generator sample-data))

#_(get-removable (generator sample-data))

;; Tests
;; Use tests to verify your solution. Consider using the sample data provided in the question

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
