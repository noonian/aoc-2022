(ns aoc-2022.day7
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [clojure.pprint :refer [pprint]]))

(def input (util/input-for "day7"))

(defn parse-command [lines]
  (let [[_ op arg] (str/split (first lines) #"\s+")
        [out remaining] (split-with (complement #(str/starts-with? % "$"))
                                    (rest lines))]
    [{:op op
      :arg arg
      :out out}
     remaining]))

(defn parse-input [s]
  (loop [lines (str/split-lines s)
         res []]
    (if-not (seq lines)
      res
      (let [[cmd remaining] (parse-command lines)]
        (recur remaining (conj res cmd))))))

(defn dir? [s] (str/starts-with? s "dir"))

(defn parse-file [s]
  (let [[size filename] (str/split s #" ")]
    {:name filename
     :size (Long/parseLong size)}))

(defn parse-dir [s]
  (subs s (count "dir ") (count s)))

(defn ls [{:keys [cwd index] :as env} out]
  (let [dirs (mapv parse-dir (filter dir? out))
        files (mapv parse-file (remove dir? out))
        file-size (reduce + 0 (map :size files))
        node {:name (last cwd)
              :path cwd
              :dirs dirs
              :files files
              :file-size file-size}]
    (assoc-in env [:index cwd] node)))

(defn cd [env target]
  (cond
    (= "/" target) (assoc env :cwd ["/"])
    (= ".." target) (update env :cwd #(into [] (drop-last %)))
    :else (update env :cwd conj target)))

(def ex-input
  "$ cd /
$ ls
dir a
14848514 b.txt
8504156 c.dat
dir d
$ cd a
$ ls
dir e
29116 f
2557 g
62596 h.lst
$ cd e
$ ls
584 i
$ cd ..
$ cd ..
$ cd d
$ ls
4060174 j
8033020 d.log
5626152 d.ext
7214296 k")

(defn analyze-cmd [env {:keys [op arg out] :as cmd}]
  (condp = op
    "cd" (cd env arg)
    "ls" (ls env out)))

(defn compute-sizes [index]
  (let [table (atom index)
        compute-size (fn compute-size [dir-path]
                       (let [{:keys [total-size file-size path dirs]} (get @table dir-path)
                             size (or total-size
                                      (and (not (seq dirs)) file-size)
                                      (->> (for [dir dirs] (conj path dir))
                                           (map compute-size)
                                           (apply +)
                                           (+ file-size)))]
                         (swap! table assoc-in [path :total-size] size)
                         size))]
    (doseq [path (keys @table)]
      (compute-size path))
    @table))

(defn analyze [input]
  (let [commands (parse-input input)
        env (-> (reduce analyze-cmd
                        {:cwd [] :fs {}}
                        commands)
                (update :index compute-sizes))]
    env))

(def index (compute-sizes (:index env)))

;; part1

(->> (analyze input)
     :index
     vals
     (filter #(< (:total-size %) 100000))
     (map :total-size)
     (apply +))

;; part2

(def total-space 70000000)
(def needed-unused-space 30000000)

(defn find-delete-target [index]
  (let [used (:total-size (get index ["/"]))
        unused (- total-space used)
        needed (- needed-unused-space unused)]
    (->> (sort-by :total-size (vals index))
         (filter #(> (:total-size %) needed))
         first)))

(->> (analyze input)
     :index
     find-delete-target
     :total-size)
