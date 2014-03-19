(ns code-jam.rotate
  (:require [clojure.string :as str]))


; https://code.google.com/codejam/contest/544101/dashboard#s=p0

(def lines (str/split-lines
             (slurp "resources/rotate/A-small-practice.in")))

(def no-problems (first lines))
(def problems (rest lines))

(defn take-problem-from [data]
  (let [[n-k & rest] data
        [n k] (map read-string (str/split n-k #" "))
        [grid remaining] (split-at n rest)]
    [{:n n :k k :grid grid} remaining]))

(def first-problem (first (take-problem-from problems)))

(defn to-printable-grid [{grid :grid}]
  (str/join "\n" grid))

(defn print-grid [{grid :grid}]
  (println (str/join "\n" grid)))


(let [grid (:grid first-problem)])

(def test-grid ["123"
                "456"
                "789"
                ])

(defn rotate [grid f]
  (let [row-length (count (first grid))
    rotated (map
      (fn [i] (map #(get % i) grid))
      (f (range row-length)))]
    #_(map #(apply str %) rotated)
    rotated
    ))

(defn rotate-left [grid]
  (rotate grid reverse))

(defn rotate-right [grid]
  (map #(apply str (reverse %))
    (rotate grid identity)))

(map
  (fn [i] (map #(get % i) (:grid first-problem)))
  (range (count (:grid first-problem))))