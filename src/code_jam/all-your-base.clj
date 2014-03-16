(ns code-jam.all-your-base
  (:require [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.math BigInteger)))

;https://code.google.com/codejam/contest/189252/dashboard#s=p0

(def alpha-numeric
  (map str (concat [1 0] (range 2 10) (map char (range 65 91)))))

(defn trans-map [s]
    (zipmap (distinct s) alpha-numeric))

(defn translate [s]
  (apply str
         (map (trans-map s) s)))

(defn to-number [s]
  (let [translated (translate s)
        radix (max 2
                   (count (distinct s)))]
    (BigInteger. translated radix)))

(defn min-base [s]
  (count (set s)))

(defn problems [f]
  (drop 1 (str/split-lines
            (slurp (io/resource f)))))

(defn -main [& args]
  (spit "result.txt"
        (str/join "\n"
                  (map
                    (fn [n result] (format "Case #%s: %s" n result))
                    (iterate inc 1)
                    (map to-number (problems "A-large-practice.in"))))))
