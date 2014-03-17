(ns code-jam.scalar
  (:require [clojure.string :refer [join split split-lines]]
            [clojure.math.combinatorics :as combo]))

(defrecord Problem [xs ys])

(defn str->numbers [s]
  (map #(Integer/parseInt %)
       (split s #"\s+")))

(defn scalar-prodcut [xs ys]
  (reduce + (map * xs ys)))

(defn min-scalar-product [p]
  (let [xs (sort (:xs p))
        ys (reverse (sort (:ys p)))]
    (scalar-prodcut xs ys)))



(def p (->Problem (str->numbers "1 -5 3") (str->numbers "-2 1 4")))
(def p2 (->Problem
          [677 463 -569 516 401 -998 882]
          [890 588 959 909 948 -617 -655]))


(def data (rest
            (split-lines
              (slurp "/Users/barkerr/Downloads/A-small-practice.in"))))

(defn solve [d]
  (let [[args xs ys] (take 3 d)
        xs (str->numbers xs)
        ys (str->numbers ys)
        p (->Problem xs ys)]
    (min-scalar-product p)))


(defn do-solve []
  (loop [d data
         i 1]
    (when d
      (do
        (println (format "Case #%d: %d" i (solve d)))
        (recur (drop 3 d) (inc i))))))

(defn idxd [c]
  (map-indexed vector c))

(defn div-by-3? [n] (zero? (mod n 3)))

(defn div-by-n? [x n] (zero? (mod x n)))

(defn take-every-3rd [c]
  (->> (idxd c)
       (filter (fn [[i _]] (div-by-3? i)))
       (map (fn [[_ v]] v))))

(defn take-every-n [n c]
  (->> (idxd c)
       (filter (fn [[i _]] (div-by-n? i n)))
       (map (fn [[_ v]] v))))

(defn slice-3-ways [c]
  (let [a (take-every-3rd data)
        b (take-every-3rd (rest data))
        c (take-every-3rd (drop 2 data))]
    (map vector a b c)))

(defn slice-3-ways2 [c]
  (group-by (fn [[k _]] (quot k 3)) (idxd c)))

(defn slice-3-ways3 [c]
  (let [v (vec c)
        s (count c)]
  (for [i (range 0 s 3)] (subvec v i (+ i 3)))))

(defn slice-n [n c]
  (apply map vector (for [i (range n)]
                      (take-every-n n (drop i c)))))
