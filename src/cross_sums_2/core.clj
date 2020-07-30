(ns cross-sums-2.core
  (:require [clojure.core.logic :as l]
            [clojure.set :as set]
            [tupelo.core :refer [spyx]]
            [clojure.core.logic.fd :as fd]))

(defn ->flags [fs]
  (->> fs (map (fn [[direction x y sum distance]]
                 {:direction (if (= direction :d) :down :right)
                  :x x :y y :sum sum :distance distance}))))

(def flags-sample-1 #{{:direction :down :x 1 :y 0 :sum 4 :distance 2}
                      {:direction :down :x 2 :y 0 :sum 6 :distance 2}
                      {:direction :right :x 0 :y 1 :sum 3 :distance 2}
                      {:direction :right :x 0 :y 2 :sum 7 :distance 2}})

(def f1 (->flags '([:d 1 0 4 2] [:d 2 0 6 2] [:r 0 1 3 2] [:r 0 2 7 2])))
(def f2 (->flags '([:d 1 0 5 2] [:d 2 0 8 2] [:r 0 1 4 2] [:r 0 2 9 2])))
(def f3 (->flags '([:d 1 0 4 2] [:d 2 0 7 2] [:r 0 1 3 2] [:r 0 2 8 2])))


(defn flags->flags-down [flags]
  (filter #(= (:direction %) :down) flags))

(defn flags->flags-right [flags]
  (filter #(= (:direction %) :right) flags))

(defn down->coords [{:keys [x y distance]}]
  (let [y-coords (range (inc y) (inc (+ y distance)))]
    (map #(vector x %) y-coords)))

(defn right->coords [{:keys [x y distance]}]
  (let [x-coords (range (inc x) (inc (+ x distance)))]
    (map #(vector % y) x-coords)))

(defn x-shape-coords->lvp
  "Translate one x-y coordinate pair & x-shape (e.g. 2x3 matrix has x-shape of 3)
  to an 'lvp' integer ('lvar vector position'). For example, a coordinate [1 1] on
  a 2x3 matrix would be 4:
  [[0 1 2]
   [3 4 5]]
      ∧                         ∨
  which corresponds to [0 1 2 3 4 5].

  (x-shape-coords->lvp 3 [1 1]) ;; => 4
  "
  [x-shape [x y]]
  (-> y (* x-shape) (+ x)))

(defn generate-lvp-lvar-map
  "Given x-shape and a seq of coordinate pairs, return a sorted map of lvars,
  keyed by their 'lvar vector position' (aka lvp)."
  [x-shape coords]
  (->> coords
       (map #(x-shape-coords->lvp x-shape %))
       (distinct)
       (reduce (fn [acc lvp] (assoc acc lvp (l/lvar))) (sorted-map))))

(defn flags->lvars-map [flags]
  (let [r-coords (->> flags flags->flags-right (mapcat right->coords))
        d-coords (->> flags flags->flags-down (mapcat down->coords))
        coords (set/union (into #{} r-coords) (into #{} d-coords))
        max-x (-> (apply max-key first coords) first)
        x-shape (inc max-x)]
    {:lvp-lvar-map (generate-lvp-lvar-map x-shape coords)
     :x-shape x-shape}))

(defn sumo [l sum]
  (l/fresh [a d sum-of-remaining]
    (l/conde
     [(l/== l ()) (l/== sum 0)]
     [(l/conso a d l)
      (fd/+ a sum-of-remaining sum)
      (sumo d sum-of-remaining)])))

(defn adds-up [{:keys [sum lvars]}]
  (sumo lvars sum))

(defn flags->entry-values [flags]
  (let [{:keys [lvp-lvar-map x-shape]} (flags->lvars-map flags)
        all-lvars (vals lvp-lvar-map)
        downs (->> flags flags->flags-down
                   (map (fn [{sum :sum :as down-flag}]
                          {:sum sum
                           :lvars (->> down-flag
                                       down->coords
                                       (map #(x-shape-coords->lvp x-shape %))
                                       (map #(get lvp-lvar-map %)))})))
        rights (->> flags flags->flags-right
                    (map (fn [{sum :sum :as right-flag}]
                           {:sum sum
                            :lvars (->> right-flag
                                        right->coords
                                        (map #(x-shape-coords->lvp x-shape %))
                                        (map #(get lvp-lvar-map %)))})))
        val-range (range 1 10)
        in-range (fn [x] (fd/in x (apply fd/domain val-range)))]
    (l/run* [q]
      (l/== q all-lvars)
      (l/everyg in-range all-lvars)
      (l/everyg adds-up rights)
      (l/everyg adds-up downs)
      (fd/distinct all-lvars))))
