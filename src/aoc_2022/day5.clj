(ns aoc-2022.day5
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

(def input (util/input-for "day5"))

(def ex-input
  "    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2")

(defn parse-stack-line
  "Return map of {stack-index stack-id} for a line like:
  \"[A]    [B]\". This input would return {1 \"A\", 2 \"B\", 3 \"C\"}."
  [s]
  (let [len (count s)]
    (loop [i 1
           stacks {}
           remaining s]
      (if-not (seq remaining)
        stacks
        (let [stack (subs remaining 0 3)
              len (count remaining)
              more (subs remaining 3 len)]
          (recur (inc i)
                 (cond-> stacks
                   (not (str/blank? stack)) (assoc i (subs stack 1 2)))
                 (if (str/blank? more)
                   more
                   (subs more 1 (count more)))))))))

(defn parse-stacks [s]
  (let [lines (str/split-lines s)
        levels (map parse-stack (butlast lines))
        stacks-ids (map #(Long/parseLong %)
                        (str/split (str/trim (last lines)) #"\s+"))]
    (reduce (fn [stacks level]
              (merge-with conj stacks level))
            (into {} (for [id stacks-ids] [id []]))
            (reverse levels))))

(defn parse-op [s]
  (let [[quantity from to]
        (map #(Long/parseLong %)
             (next (re-find #"move ([0-9]+) from ([0-9]+) to ([0-9]+)" s)))]
    {:quantity quantity
     :from from
     :to to}))

(defn parse-input [s]
  (let [[initial-config proc] (str/split s #"\n\n")
        stacks (parse-stacks initial-config)]
    {:stacks    stacks
     :procedure (mapv parse-op (str/split-lines proc))
     :ids       (into [] (sort (keys stacks)))}))

(defn move [stacks {:keys [quantity from to]}]
  (-> stacks
      (update to into (reverse (take-last quantity (get stacks from))))
      (update from #(into [] (drop-last quantity %)))))

(defn move2 [stacks {:keys [quantity from to]}]
  (-> stacks
      (update to into (take-last quantity (get stacks from)))
      (update from #(into [] (drop-last quantity %)))))

;; part1

(let [{:keys [stacks procedure ids]} (parse-input input)
      final-stacks (reduce move stacks procedure)]
  (map #(last (get final-stacks %)) ids))

;; part2

(let [{:keys [stacks procedure ids]} (parse-input input)
      final-stacks (reduce move2 stacks procedure)]
  (map #(last (get final-stacks %)) ids))
