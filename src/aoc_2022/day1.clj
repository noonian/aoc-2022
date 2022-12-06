(ns aoc-2022.day1
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

(def input (util/input-for "day1"))

(def elves
  (->> (str/split input #"\n\n")
       (map #(str/split-lines %))
       (map (fn [rations] (map #(Long/parseLong %) rations)))))

;; part 1
(->> (map #(apply + %) elves)
     (apply max))

;; part 2

(->> (map #(apply + %) elves)
     (sort >)
     (take 3)
     (apply +))
