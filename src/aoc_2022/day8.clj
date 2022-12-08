(ns aoc-2022.day8
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]))

(def input (util/input-for "day8"))

(def ex-input
  "30373
25512
65332
33549
35390")

(defn parse-input [s]
  (let [grid (mapv #(mapv (comp parse-long str) %) (str/split-lines s))
        point->height
        (into {}
          (for [y (range (count grid))
                x (range (count (first grid)))
                :let [point [x y]]]
            [point (get-in grid [y x])]))]
    {:grid grid
     :point->height point->height
     :x-max (count (first grid))
     :y-max (count grid)}))

(defn edge-rays [{:keys [x-max y-max]} [px py :as point]]
  [(for [x (range 0 px)] [x py])
   (for [x (range (inc px) x-max)] [x py])
   (for [y (range 0 py)] [px y])
   (for [y (range (inc py) y-max)] [px y])])

(defn tall-as?
  "Returns true if the tree at point a is as tall as the tree at point
  b."
  [{:keys [point->height]} a b]
  (>= (point->height a)
      (point->height b)))

(def shorter? (complement tall-as?))

(defn on-edge? [{:keys [x-max y-max]} [x y :as point]]
  (or (zero? x)
      (zero? y)
      (= x x-max)
      (= y y-max)))

(defn visible-through? [ctx point trees]
  (every? #(shorter? ctx % point) trees))

(defn visible? [{:keys [x-max y-max point->height] :as ctx} point]
  (or (on-edge? ctx point)
      (some #(visible-through? ctx point %) (edge-rays ctx point))))

;; part1

(let [ctx (parse-input input)]
  (->> (keys (:point->height ctx))
       (filter #(visible? ctx %))
       count))

;; part2

(defn manhattan-distance [[x1 y1] [x2 y2]]
  (+ (abs (- x1 x2))
     (abs (- y1 y2))))

(defn viewing-distance [ctx point direction-trees]
  (let [closest-blocking-tree (->> direction-trees
                                   (filter #(tall-as? ctx % point))
                                   (sort-by (partial manhattan-distance? point))
                                   first)]
    (if-not closest-blocking-tree
      (count direction-trees)
      (manhattan-distance point closest-blocking-tree))))

(defn scenic-score [ctx point]
  (->> (edge-rays ctx point)
       (map #(viewing-distance ctx point %))
       (apply *)))

(let [{:keys [point->height] :as ctx} (parse-input input)]
  (->> (keys point->height)
       (map #(scenic-score ctx %))
       (apply max)))
