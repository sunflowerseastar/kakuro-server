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

(defn coords-and-x-shape->vector-position [[x y] x-shape]
  (-> y (* x-shape) (+ x)))


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

(defn b1 [flags]
  (let [
        r-coords (->> flags flags->flags-right (mapcat r->rcs))
        d-coords (->> flags flags->flags-down (mapcat d->dcs))
        coords (set/union r-coords d-coords)
        max-x (-> (apply max-key first coords) first)
        max-y (-> (apply max-key second coords) second)
        shape [(inc max-y) (inc max-x)]
        ]
    (spyx shape)
    ;; [coords max-x max-y shape]
    ))


#_(l/defne rows-satisfy [n rows]
  ([_ ()])
  ([n [curr-row . rest-rows]]
   (rows-satisfy rest-rows)
   (l/+)
   )
  )

#_(l/run* [q]
  (l/== q board)
  (l/everyg in-range board)
  (map #() rows)
  (l/everyg rows)
  )

(l/run* [qs]
  (l/== qs (map vector (repeatedly l/lvar) (range 2))))


(defn sumo [l sum]
  (l/fresh [a d sum-of-remaining]
    (l/conde
     [(l/== l ()) (l/== sum 0)]
     [(l/conso a d l)
      (fd/+ a sum-of-remaining sum)
      (sumo d sum-of-remaining)])))

(defn z1 []
  (let [
        row1 (into [] (repeatedly 2 l/lvar))
        rows (into [] row1)
        ;; row2 (repeatedly 2 l/lvar)
        in-range (fn [x] (fd/in x (apply fd/domain (range 1 3))))
        ]
    (spyx row1 rows)
    (l/run* [r1]
      (l/== r1 row1)
      (l/everyg in-range row1)
      ;; (sumo r1 6)
      (l/everyg fd/distinct row1)
      ;; (l/everyg fd/distinct board)
      )))

(defn z3 []
  (let [board (repeatedly 3 l/lvar)
        rows (into [] (map vec (partition 2 board)))
        val-range (range 1 4)
        in-range (fn [x] (fd/in x (apply fd/domain val-range)))]
    (l/run* [q]
      (l/== q board)
      (l/everyg in-range board)
      (fd/distinct board)
      ;; (l/everyg fd/distinct rows)
      )))

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

(def rs ())

(defn z2 []
  (let [board (repeatedly 6 l/lvar)
        rows (into [] (map vec (partition 3 board)))
        val-range (range 1 4)
        in-range (fn [x] (fd/in x (apply fd/domain val-range)))]
    (l/run* [q]
      (l/== q board)
      (l/everyg in-range board)
      (l/everyg fd/distinct rows)
      )))
