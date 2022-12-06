(ns aoc-2022.day3
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (util/input-for "day3"))

(defn shared-item [rucksack]
  (let [n (count rucksack)
        [left right] (split-at (long (/ n 2)) rucksack)]
    (first
     (set/intersection (set left) (set right)))))

(def priority
  (merge
   (zipmap (map char (range (int \a) (inc (int \z))))
           (range 1 27))
   (zipmap (map char (range (int \A) (inc (int \Z))))
           (range 27 53))))

;; part1

(->> (str/split-lines input)
     (map shared-item)
     (map priority)
     (apply +))

;; part2

(defn common-item [packs]
  (first
   (apply set/intersection (map set packs))))

(->> (str/split-lines input)
     (partition 3 3)
     (map common-item)
     (map priority)
     (apply +))
