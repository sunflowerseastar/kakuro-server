(ns cross-sums-2.core
  (:require [clojure.core.logic :as l]
            [clojure.set :as set]
            [tupelo.core :refer [spyx]]
            [clojure.core.logic.fd :as fd]))

(def f1 #{
             {:direction :down :x 1 :y 0 :sum 4 :distance 2}
             {:direction :down :x 2 :y 0 :sum 6 :distance 2}
             {:direction :right :x 0 :y 1 :sum 3 :distance 2}
             {:direction :right :x 0 :y 2 :sum 7 :distance 2}
             })


(defn flags->flags-down [flags]
  (filter #(= (:direction %) :down) flags))

(defn flags->flags-right [flags]
  (filter #(= (:direction %) :right) flags))

(defn d->dcs [{:keys [x y distance]}]
  (let [y-coords (range (inc y) (inc (+ y distance)))]
    (map #(vector x %) y-coords)))

(defn r->rcs [{:keys [x y distance]}]
  (let [x-coords (range (inc x) (inc (+ x distance)))]
    (map #(vector % y) x-coords)))

(defn coords-and-x-shape->vector-position [[x y] x-shape]
  (-> y (* x-shape) (+ x)))

(defn coords-and-shape->lvar-lookup-map [coords shape]
  (->> coords
       (map #(coords-and-x-shape->vector-position % (second shape)))
       (distinct)
       (reduce (fn [a b] (assoc a b (l/lvar))) {})
       )
  )

(defn flags->lvar-lookup-map [flags]
  (let [
        r-coords (->> flags flags->flags-right (mapcat r->rcs))
        d-coords (->> flags flags->flags-down (mapcat d->dcs))
        coords (set/union (into #{} r-coords) (into #{} d-coords))
        max-x (-> (apply max-key first coords) first)
        max-y (-> (apply max-key second coords) second)
        shape [(inc max-y) (inc max-x)]
        ]
    ;; (spyx shape)
    (coords-and-shape->lvar-lookup-map coords shape)
    ;; [coords max-x max-y shape]
    ))

(defn sumo [l sum]
  (l/fresh [a d sum-of-remaining]
    (l/conde
     [(l/== l ()) (l/== sum 0)]
     [(l/conso a d l)
      (fd/+ a sum-of-remaining sum)
      (sumo d sum-of-remaining)])))

(defn z4 []
  (let [
        lv1 (l/lvar)
        lv2 (l/lvar)
        lv3 (l/lvar)
        lv4 (l/lvar)
        r1 [lv1 lv2]
        r2 [lv3 lv4]
        d1 [lv1 lv3]
        d2 [lv2 lv4]
        board [r1 r2]
        b2 (concat r1 r2)
        val-range (range 1 10)
        in-range (fn [x] (fd/in x (apply fd/domain val-range)))
        ]
    (l/run* [q]
      (l/== q b2)
      (l/everyg in-range b2)
      (fd/distinct b2)
      ;; (sumo r1 3)
      ;; (sumo r2 7)
      ;; (sumo d1 4)
      ;; (sumo d2 6)
      (sumo r1 9)
      (sumo r2 9)
      (sumo d1 5)
      (sumo d2 13)
      )))

;; TODO for each (->> flags flags->flags-right) and down
;; do a (sumo [<each lvar>] <sum>)
(defn z5 []
  (let [
        lookup (flags->lvar-lookup-map f1)
        board (vals lookup)
        rights (->> flags flags->flags-right)
        downs (->> flags flags->flags-down)
        val-range (range 1 5)
        in-range (fn [x] (fd/in x (apply fd/domain val-range)))
        ]
    (l/run 2 [q]
      (l/== q board)
      (l/everyg in-range board)
      (fd/distinct board)
      )))
