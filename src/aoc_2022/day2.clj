(ns aoc-2022.day2
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

(def input (util/input-for "day2"))

(def part1-mapping
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :rock
   "Y" :paper
   "Z" :scissors})

(def part2-mapping
  {"A" :rock
   "B" :paper
   "C" :scissors
   "X" :lose
   "Y" :draw
   "Z" :win})

(defn winning-shape [shape]
  (condp = shape
    :rock :paper
    :paper :scissors
    :scissors :rock))

(defn losing-shape [shape]
  (winning-shape (winning-shape shape)))

(defn parse-input [mapping input]
  (->> (str/split-lines input)
       (map #(str/split % #" "))
       (map #(map (fn [shape] (get mapping shape)) %))))

(def shape-value
  {:rock 1
   :paper 2
   :scissors 3})

(defn winner [round]
  (if (apply = round)
    :draw
    (condp = (set round)
      #{:rock :scissors} :rock
      #{:rock :paper} :paper
      #{:scissors :paper} :scissors)))

(defn outcome [[a b :as round]]
  (let [winning-shape (winner round)]
    (cond
      (= :draw winning-shape) {:shape-score (shape-value b) :result :draw}
      :else
      {:shape-score (shape-value b)
       :result (cond
                 (= :draw winning-shape) :draw
                 (= a winning-shape) :lose
                 (= b winning-shape) :win)})))

(defn score [{:keys [shape-score result]}]
  (+ shape-score (condp = result
                   :lose 0
                   :draw 3
                   :win 6)))

;; part 1

(->> (parse-input part1-mapping input)
     (map (comp score outcome))
     (apply +))

;; part 2

(defn choose-shape* [[opp-shape goal]]
  (condp = goal
    :lose (losing-shape opp-shape)
    :win  (winning-shape opp-shape)
    :draw opp-shape))

(defn choose-shape [[opp-shape :as round]]
  [opp-shape (choose-shape* round)])

(->> (parse-input part2-mapping input)
     (map choose-shape)
     (map (comp score outcome))
     (apply +))
