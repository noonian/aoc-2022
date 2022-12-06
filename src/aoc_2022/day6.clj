(ns aoc-2022.day6
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

(def input (util/input-for "day6"))

(defn packet-start? [s]
  (= 4 (count (distinct (take 4 s)))))

(defn message-start? [s]
  (= 14 (count (distinct (take 14 s)))))

;; part1
(->> (partition 4 1 input)
     (take-while (complement packet-start?))
     (count)
     (+ 4))

;; part2
(->> (partition 14 1 input)
     (take-while (complement message-start?))
     (count)
     (+ 14))
