(ns kakuro-server.core
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.core.memoize :as memo]
            [clojure.set :as set]
            [compojure.core :as compojure]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [muuntaja.core :as m]
            [muuntaja.middleware :as mw]
            [ring.adapter.jetty :refer :all]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.cors :refer [wrap-cors]]
            [ring.middleware.stacktrace :refer [wrap-stacktrace]]
            [ring.util.http-response :refer :all]
            [tupelo.core :refer [spyx]])
  (:use [clojail.core :only [thunk-timeout]]))

(def timeout-ms 10000)

(defn clue-notation->clues
  "A clue is {:direction :down|:right :sum int :distance int}

  A single vector in clue-notation is the same thing, but in a compact notation:
  [:direction x y sum distance], ex. [:d 1 0 4 2]

  This function takes clue-notation and returns [expanded] clues."
  [clue-notation]
  (->> clue-notation
       (map (fn [[direction x y sum distance]]
              {:direction (if (= direction :d) :down :right)
               :x x :y y :sum sum :distance distance}))))

(def clues-expanded-1 #{{:direction :down :x 1 :y 0 :sum 4 :distance 2}
                        {:direction :down :x 2 :y 0 :sum 6 :distance 2}
                        {:direction :right :x 0 :y 1 :sum 3 :distance 2}
                        {:direction :right :x 0 :y 2 :sum 7 :distance 2}})
(def clue-notation-1 '([:d 1 0 4 2] [:d 2 0 6 2] [:r 0 1 3 2] [:r 0 2 7 2]))
(def c-2 '([:d 1 0 5 2] [:d 2 0 8 2] [:r 0 1 4 2] [:r 0 2 9 2]))
(def c-3 '([:d 1 0 4 2] [:d 2 0 7 2] [:r 0 1 3 2] [:r 0 2 8 2]))
(def c-4 '([:d 1 0 3 2] [:d 2 0 12 2] [:d 3 0 13 2] [:r 0 1 15 3] [:r 0 2 13 3]))
(def c-5 '([:d 1 0 12 3] [:d 2 0 17 3] [:d 3 0 13 3] [:r 0 1 19 3] [:r 0 2 16 3] [:r 0 3 7 3]))
(def c-6 '([:d 1 0 6 3] [:d 2 0 17 3] [:d 3 0 22 3] [:r 0 1 15 3] [:r 0 2 13 3] [:r 0 3 17 3]))
(def c-7 '([:d 1 0 16 2] [:d 2 0 9 2] [:d 3 1 5 2] [:r 0 1 16 2] [:r 0 2 13 3] [:r 2 3 1 1]))
(def c-8-no-solutions '([:d 1 0 3 2] [:d 2 0 5 2] [:r 0 1 4 2] [:r 0 2 4 2]))

(defn clues->clues-down [clues]
  (filter #(= (:direction %) :down) clues))

(defn clues->clues-right [clues]
  (filter #(= (:direction %) :right) clues))

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

(defn clues->lvars-map [clues]
  (let [r-coords (->> clues clues->clues-right (mapcat right->coords))
        d-coords (->> clues clues->clues-down (mapcat down->coords))
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

(defn clue-notation->solution-vector [clue-notation]
  (let [clues (clue-notation->clues clue-notation)
        {:keys [lvp-lvar-map x-shape]} (clues->lvars-map clues)
        all-lvars (vals lvp-lvar-map)
        downs (->> clues
                   clues->clues-down
                   (map (fn [{sum :sum :as down-clue}]
                          {:sum sum
                           :lvars (->> down-clue
                                       down->coords
                                       (map #(x-shape-coords->lvp x-shape %))
                                       (map #(get lvp-lvar-map %)))})))
        rights (->> clues
                    clues->clues-right
                    (map (fn [{sum :sum :as right-clue}]
                           {:sum sum
                            :lvars (->> right-clue
                                        right->coords
                                        (map #(x-shape-coords->lvp x-shape %))
                                        (map #(get lvp-lvar-map %)))})))
        is-single-digit #(fd/in % (apply fd/domain (range 1 10)))]
    (-> (l/run 1 [q]
          (l/== q all-lvars)
          (l/everyg is-single-digit all-lvars)
          (l/everyg adds-up rights)
          (l/everyg adds-up downs)
          (l/everyg #(fd/distinct (:lvars %)) rights)
          (l/everyg #(fd/distinct (:lvars %)) downs))
        first
        vec)))

(def memo-clue-notation->solution-vector
  (memo/lru clue-notation->solution-vector :lru/threshold 16))

(defn find-solution [req]
  (try
    (thunk-timeout
     #(let [solution (-> req
                         :body-params
                         :clue-notation
                         memo-clue-notation->solution-vector)]
        (ok {:status :ok :solution solution}))
     timeout-ms)
    (catch Exception e (do (println "timeout") (bad-request "timeout")))))

(compojure/defroutes site-routes
  (compojure/GET "/" [] "ok")
  (compojure/POST "/solve" req (find-solution req))
  (compojure/GET "/api/" [] "ok")
  (compojure/POST "/api/solve" req (find-solution req))
  (route/not-found "Page not found"))

(def api
  (-> (handler/site site-routes)
      (mw/wrap-format)
      (wrap-cors :access-control-allow-origin #"http://localhost:9500" :access-control-allow-methods [:get :post])
      (wrap-content-type)))
