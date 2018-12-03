(ns advent.three
  (:require [clojure.java.io :as jio]
            [clojure.set :as s]))

;;;;;;;;;;
;; DATA
;;;;;;;;;;


(defn span
  [{:keys [x y w h]}]
  {:span-x (apply sorted-set (range x (+ x w)))
   :span-y (apply sorted-set (range y (+ y h)))})

(defn str->coord
  [s]
  (let [[claim x y w h] (->> s
                             (re-find #"#(\d*) @ (\d*),(\d*): (\d*)x(\d*)")
                             rest
                             (map #(Integer/parseInt %)))
        parsed {:claim claim
                :x x
                :y y
                :w w
                :h h}]
    (merge parsed (span parsed))))

(def >claims
  (with-open [input (jio/reader "inputs/day03.txt")]
    (-> input line-seq doall)))

(def >>claims (map str->coord >claims))

;;;;;;;;;;
;; LIBRARY
;;;;;;;;;;

(defn intersection-on-axis
  "Find the points on one axis where two claims overlap"
  [axis a b]
  (s/intersection (axis a)
                  (axis b)))

(defn intersections
  "Combine axes of intersection to get the rectangle where claims overlap"
  [a b]
  (for [x (intersection-on-axis :span-x a b)
        y (intersection-on-axis :span-y a b)]
    (vector x y)))

(defn intersections-for-claim
  "For one claim, find points of intersection against all other claims."
  [claims claim]
  (->> claims
       (remove #(= (:claim claim)
                   (:claim %)))
       (reduce #(concat %1 (intersections claim %2))
               [])))

(defn all-intersections
  "For all claims, find points of intersection for each one and combine."
  [claims]
  (->> claims
       (map (partial intersections-for-claim claims))
       (apply concat)
       (reduce conj #{})))

(defn number-of-intersectings
  "Count total number of intersections for all claims."
  [claims]
  (->> claims
       all-intersections
       count))

;;;;;;;;;;;;;;;;;;;;;;;;
;; FIRST PROBLEM
;;;;;;;;;;;;;;;;;;;;;;;;

(defn problem-1
  "Find the total number of points overlapped by two or more claims."
  [c]
  (number-of-intersectings c))

#_(problem-1 >>claims)
; 107043


;;;;;;;;;;;;;;;;;;;;;;;;
;; SECOND PROBLEM
;;;;;;;;;;;;;;;;;;;;;;;;

(defn problem-2
  "Find a claim which doesn't overlap any other claim."
  [claims]
  (loop [c (first claims)
         cs (rest claims)]
    (cond
      (empty? (intersections-for-claim claims c)) c
      (empty? cs) nil
      :else (recur (first cs)
                   (rest cs)))))

#_(problem-2 >>claims)
;; claim 346


;;;;;;;;;;;;;;;;;;;;;;;;
;; SCRATCH
;;;;;;;;;;;;;;;;;;;;;;;;

(comment
  (def coords ["#1 @ 596,731: 11x27"
               "#2 @ 20,473: 23x22"
               "#3 @ 730,802: 23x23"
               "#4 @ 212,725: 28x25"
               "#5 @ 65,785: 13x15"
               "#6 @ 495,395: 16x11"
               "#7 @ 750,29: 26x17"
               "#8 @ 658,927: 22x11"
               "#9 @ 109,286: 11x16"
               "#10 @ 935,957: 11x20"
               "#11 @ 647,392: 23x26"
               "#12 @ 878,21: 12x24"
               "#13 @ 816,319: 16x13"
               "#14 @ 339,150: 27x25"])

  (def minicoords ["#1 @ 1,3: 4x4"
                   "#2 @ 3,1: 4x4"
                   "#3 @ 5,5: 2x2"])

  (def parsed
    (map str->coord minicoords))
  (def f (first parsed))
  (def s (second parsed))
  (def t (nth parsed 2))

  "")
