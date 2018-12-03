(ns advent.two
  (:require [clojure.string :as str]
            [clojure.java.io :as jio]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADVENT OF CODE: DAY 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def >ids
  (with-open [input (jio/reader "inputs/day02.txt")]
    (-> input line-seq doall)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHALLENGE THE FIRST
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn freqn?
  [code n]
  (if (empty?
       (filter #(= n (second %))
               (frequencies code)))
    0
    1))

(def startchecksum {:twos 0
                    :threes 0})

(defn check-code
  [{:keys [twos threes]} code]
  {:twos (+ twos (freqn? code 2))
   :threes (+ threes (freqn? code 3))})

(let [{:keys [twos threes]} (reduce check-code startchecksum >ids)]
  (* twos threes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; CHALLENGE THE SECOND
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn strdiff
  "Get the diff of two strings, as a sequence of chars. Letters not common to both strings (see docstring for `common-letters`) are nil."
  [a b]
  (map #(if (= %1 %2)
          %1
          nil)
       a
       b))

(defn off-by-one?
  "Are two strings off by one character?"
  [as bs]
  (->> (strdiff as bs)
       (filter nil?)
       count
       (= 1)))

(defn common-letters
  "Return the letters common to two strings. Letters are common if the same letters occur in the same position in both strings."
  [a b]
  (->> (strdiff a b)
       (str/join "")))

(defn check-this-id
  "check an id against all other ids
  loop/recur until an off-by-one is found or end of seq is reached."
  [all-ids id]
  (loop [i id
         search all-ids]
    (cond
      (empty? search) nil
      (off-by-one? i (first search)) (do (println "curr is ->" i)
                                         (println "next is ->" (first search))
                                         (common-letters i (first search)))
      :else (recur i (rest search)))))

(defn longest-common
  [all-ids]
  (loop [ids all-ids]
    (let [checked (check-this-id all-ids (first ids))]
      (cond
        (empty? ids) nil
        checked checked
        :else (recur (rest ids))))))

(comment

  (def f "iosnxmfkezbcjpdgwvrtaktluq")
  (def s "aosnxmfkezbcjpdgwvrtaqhpwy")
  (strdiff f s)
  (off-by-one? f s) (longest-common >ids)

  (let [testset ["abcde"
                 "fghij"
                 "klmno"
                 "pqrst"
                 "fguij"
                 "axcye"
                 "wvxyz"]]
    (longest-common testset))

  "")
