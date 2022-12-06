(ns aoc-2022.util
  (:require [clojure.java.io :as io]
            [clojure.string :as str]))

(defn input-for [input-name]
  (str/trim-newline (slurp (io/resource (format "%s.txt" input-name)))))
