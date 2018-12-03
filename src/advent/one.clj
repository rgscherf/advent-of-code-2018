(ns advent.one
  (:require [clojure.java.io :as jio]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE: DAY 1
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def >ints
  (with-open [input (jio/reader "inputs/day01.txt")]
    (map #(Integer/parseInt %)
         (-> input line-seq doall))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHALLENGE THE FIRST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(reduce + >ints)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHALLENGE THE SECOND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(loop [{:keys [seen freq]}  {:seen #{0}
                             :freq 0}
       n (cycle >ints)]
  (let [new-freq (+ freq (first n))]
    (if (contains? seen new-freq)
      new-freq
      (recur {:seen (conj seen new-freq)
              :freq new-freq}
             (rest n)))))
