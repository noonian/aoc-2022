(ns aoc-2022.day9
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

(def input (util/input-for "day9"))

(def ex-input
  "R 4
U 4
L 3
D 1
R 4
D 1
L 5
R 2")

(defn parse-direction [s]
  (let [[_ dir n] (re-find #"(U|D|L|R) (\d+)" s)]
    {:dir (keyword dir)
     :n (Long/parseLong n)}))

(defn simplify [{:keys [dir n]}]
  (repeat n dir))

(defn parse-directions [s]
  (->> (str/split-lines s)
       (map parse-direction)
       (mapcat simplify)
       (into [])))

(defn move [[x y] dir]
  (condp = dir
    :R [(inc x) y]
    :U [x (inc y)]
    :L [(dec x) y]
    :D [x (dec y)]))

(defn touching? [[x1 y1 :as p1] [x2 y2 :as p2]]
  (and (<= (abs (- x1 x2)) 1)
       (<= (abs (- y1 y2)) 1)))

(defn follow
  "Returns new tail position."
  [[hx hy :as head] [tx ty :as tail]]
  (if (touching? tail head)
    tail
    [(cond
       (= tx hx) tx
       (> hx tx) (inc tx)
       :else (dec tx))
     (cond
       (= ty hy) ty
       (> hy ty) (inc ty)
       :else (dec ty))]))

(defn evolve-dir [{:keys [knots history] :as env} dir]
  (let [new-head (move (first knots) dir)
        new-knots (reductions follow new-head (drop 1 knots))]
    (assoc env
      :knots new-knots
      :history (conj history new-knots))))

(defn make-env [n]
  (let [knots (into [] (repeat n [0 0]))]
    {:knots knots :history [knots]}))

(defn evolve-knots
  ([dirs] (evolve-knots (make-env 10) dirs))
  ([env dirs]
   (reduce evolve-dir env dirs)))

(defn format-knots [knots]
  (let [x-min (apply min (map first knots))
        x-max (apply max (map first knots))
        y-min (apply min (map second knots))
        y-max (apply max (map second knots))
        knot? (set knots)]
    (str/join "\n"
      (for [y (range y-max (dec y-min) -1)]
        (str/join ""
          (for [x (range x-min (inc x-max))
                :let [pos [x y]]]
            (if (knot? pos) "#" ".")))))))

;; part1

(->> (evolve-knots (make-env 2) (parse-directions input))
     :history
     (map last)
     (into #{})
     count)

;; part2

(def ex-input2
  "R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20")

(->> (evolve-knots (parse-directions input))
     :history
     (map last)
     (distinct)
     count
     ;; format-knots
     ;; println
     )
