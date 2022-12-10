(ns aoc-2022.day10
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

(def input (util/input-for "day10"))
(def ex2 (util/input-for "day10.ex2"))

(defn parse-op [s]
  (let [[op & args] (str/split s #"\s")]
    (into [(keyword op)] (map parse-long args))))

(defn parse-input [s]
  (map parse-op (str/split-lines s)))

(def initial-env
  {:cycle 1
   :X 1})

;; returns list of envs for each cycle of op
(defmulti eval-op (fn [env op arg] op))

(defmethod eval-op :noop [env _ _]
  [(update env :cycle inc)])

(defmethod eval-op :addx [env _ n]
  [(update env :cycle inc)
   (-> env
       (update :cycle + 2)
       (update :X + n))])

(defn advance [{:keys [program cycle] :as env}]
  (apply eval-op env (first program)))

(defn cycles [program]
  (reduce (fn [res [op arg]] (into res (eval-op (last res) op arg)))
          [initial-env]
          program))

(defn signal-strength [{:keys [cycle X]}]
  (* cycle X))

(defn solve [cycles]
  (->> (take-nth 40 (drop 19 cycles))
       (map signal-strength)
       (apply +)))

;; part1

(solve (cycles (parse-input input)))

;; part2

(defn format-crt [cycles]
  (str/join "\n"
    (for [y (range 6)]
      (str/join ""
        (for [x (range 40)
              :let [n (+ x (* y 40))
                    center (-> (get cycles n) :X)
                    visible? (<= (abs (- x center)) 1)]]
          (if visible? "#" "."))))))


(comment

  (println (format-crt (cycles (parse-input input))))

  )
