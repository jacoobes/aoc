(ns y2025.d5
  (:require [clojure.test :as t :refer [deftest]]))

;; PROBLEM LINK https://adventofcode.com/2025/day/5

(defn generator
  "The generator fn is used to parse your input into. The output of this fn will be passed into each of the solving fns"
  [input]
    (let [[ranges ids] (->>  (clojure.string/split input #"\n\n")
                       (map clojure.string/split-lines)) 
          ids (map Long/parseLong ids)
          ranges (->>  (map #(clojure.string/split % #"-") ranges)
                       (map (fn [ar]  (map Long/parseLong ar)))
                       (map vec)) ] 
          {:ranges (vec ranges) 
           :ids ids}))

(defn in-any-of-range [ids, rngs]
    (reduce (fn [acc cur] 
        (let [in-ranges (filter #(<= (first %) cur (second %)) rngs)]
            (if (not-empty in-ranges)
                (assoc acc cur in-ranges)
                acc))) {} ids))

(defn solve-part-1
  "The solution to part 1. Will be called with the result of the generator"
  [input]
    (let [{:keys [ranges ids]}  input] 
        (->>  (in-any-of-range ids ranges)
              (count)) ))


(defn solve-part-2
  "The solution to part 2. Will be called with the result of the generator"
  [input]
    (let [{:keys [ranges ids]}  input
          #_ranges #_(map (fn [v] [(first v) (inc (second v))]) ranges) 
          sorted_ranges (vec (sort (comp + compare) ranges))]
        (->> (reduce (fn [acc cur]  
                (let [top (peek acc)
                      #_ (println "top" top "cur" cur) ] 
                    (cond 
                        (>= (get cur 0) (get top 1)) (conj (pop acc) [(get top 0) (get cur 1)])
                        (= (inc (get top 1)) (get cur 0)) (conj (pop acc) [(get top 0) (get cur 1)])
                        :else (conj acc cur)))) 
                    [(get sorted_ranges 0)] (vec (rest sorted_ranges)))
             (map #(inc (- (second %) (first %))))
             vec
             (apply +)
             )))

; 314967122375415
; 334291616815636
; 334291616815636
; 334291616815652


(def sample-data 
"3-5
5-7
10-14
16-20
12-18

1
5
8
11
17
32")

#_(solve-part-1 
    (generator sample-data))

(solve-part-2 
    (generator sample-data)
    )

(deftest sample-test
  (t/is (= 2 (+ 1 1))))
