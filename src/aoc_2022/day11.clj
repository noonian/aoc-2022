(ns aoc-2022.day11
  (:require [aoc-2022.util :as util]
            [clojure.string :as str]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]
            [clojure.pprint :refer [pprint]]
            [camel-snake-kebab.core :as csk]
            [camel-snake-kebab.extras :as cske]))

(def input (util/input-for "day11"))

(def ex-input
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1")

(def monkey
  "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3")

(defn parse-num-list [s]
  (mapv parse-long (str/split s #", ")))

(defn mult [a b modulo]
  (mod (* a b) modulo))

(def ops
  {"*" *
   "+" +})

(defn mult [a b]
  (merge-with + a b))

(defn compile-operation
  "Returns a function of the old worry that returns the new worry."
  [operation]
  (let [[_ a op-str b] (re-find #"new = (old|\d+) (\*|\+) (old|\d+)" operation)
        op (get ops op-str)]
    (fn [worry]
      (op (if (= "old" a) worry (parse-long a))
          (if (= "old" b) worry (parse-long b))))))

(defn compile-test
  "Returns a function of the new worry that returns the next monkey to
  throw to."
  [{:keys [value contents]}]
  (let [[_ test-s] (re-find #"divisible by (\d+)" value)
        [_ then-s] (re-find #"If true: throw to monkey (\d+)" (first contents))
        [_ else-s] (re-find #"If false: throw to monkey (\d+)" (second contents))
        n (parse-long test-s)
        then (parse-long then-s)
        else (parse-long else-s)]
    {:divide-by n
     :then then
     :else else}))

(defn perform-test [{:keys [divide-by then else]} n]
  (if (zero? (mod n divide-by))
    then
    else))

(def prop-parsers
  {:starting-items #(parse-num-list (:value %))
   :operation #(compile-operation (:value %))
   :test compile-test})

(defn unindent [lines]
  (mapv #(subs % 2 (count %)) lines))

(defn indented? [line] (str/starts-with? line "  "))

(defn parse-prop [lines]
  (let [[prop value-str] (str/split (first lines) #": ")
        prop-name (csk/->kebab-case-keyword prop)
        contents (take-while indented? (rest lines))]
    [{:name prop-name
      :value value-str
      :contents (unindent contents)}
     (cond->> (rest lines)
       (seq contents) (drop-while indented?))]))

(defn parse-props [lines]
  (loop [props []
         others lines]
    (if-not (seq others)
      props
      (let [[prop remaining-lines] (parse-prop others)]
        (recur (conj props prop) remaining-lines)))))

(defn parse-monkey [s]
  (let [lines (str/split-lines s)
        [_ id] (re-find #"Monkey (\d+):" (first lines))]
    (into {:id (parse-long id)}
      (for [prop (parse-props (unindent (rest lines)))
            :let [prop-name (:name prop)
                  parser (get prop-parsers prop-name identity)]]
        [prop-name (parser prop)]))))

(defn parse-monkeys [s]
  (mapv parse-monkey (str/split s #"\n\n")))

(defn add-relief [worry]
  (long (/ worry 3)))

(defn eval-turn [env id]
  (let [{:keys [starting-items operation test]} (get-in env [:monkeys id])]
    (loop [{:keys [monkeys inspects] :as res} (assoc-in env [:monkeys id :starting-items] [])
           items starting-items]
      (if-not (seq items)
        res
        (let [worry (first items)
              new-worry (cond-> (operation worry)
                          (not (:no-relief env)) (add-relief)
                          (:no-relief env) (mod (:modulo env)))
              target-monkey (perform-test test new-worry)
              new-env
              (-> res
                  (update-in [:inspects id] inc)
                  (update-in [:monkeys target-monkey :starting-items] conj new-worry))]
          (recur new-env (next items)))))))

(defn eval-round [env]
  (reduce eval-turn env (range (count (:monkeys env)))))

(defn find-modulo [monkeys]
  (apply * (map (comp :divide-by :test) monkeys)))

(defn make-env [monkeys]
  {:monkeys monkeys
   :modulo (find-modulo monkeys)
   :inspects (into {} (for [i (range (count monkeys))] [i 0]))})

(defn rounds [env]
  (iterate eval-round env))

(defn monkey-business [inspects]
  (->> (vals inspects)
       (sort-by -)
       (take 2)
       (apply *)))

;; part1

(->> (rounds (make-env (parse-monkeys input)))
     (drop 20)
     first
     :inspects
     monkey-business)

;; part2

(->> (rounds (assoc (make-env (parse-monkeys input)) :no-relief true))
     (drop 10000)
     first
     :inspects
     monkey-business)
