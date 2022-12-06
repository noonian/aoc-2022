(ns aoc-2022.day4
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]))

(def input (util/input-for "day4"))

(def ex-input
  "2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8")

(defn parse-pair [s]
  (let [[a b c d] (map #(Long/parseLong %)
                       (next (re-find #"(\d+)-(\d+),(\d+)-(\d+)" s)))]
    [(set (range a (inc b)))
     (set (range c (inc d)))]))

(defn parse-input [s]
  (mapv parse-pair (str/split-lines s)))

(defn fully-contains? [[a b]]
  (or (set/subset? a b)
      (set/subset? b a)))

(defn overlaps? [[a b]]
  (seq (set/intersection a b)))

;; part1

(->> (parse-input input)
     (filter fully-contains?)
     count)

;; part2

(->> (parse-input input)
     (filter overlaps?)
     count)
