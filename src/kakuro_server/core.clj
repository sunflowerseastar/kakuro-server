(ns kakuro-server.core
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.fd :as fd]
            [clojure.core.memoize :as memo]
            [compojure.core :as compojure]
            [compojure.handler :as handler]
            [compojure.route :as route]
            [muuntaja.core :as m]
            [muuntaja.middleware :as mw]
            [ring.adapter.jetty :refer :all]
            [ring.middleware.content-type :refer [wrap-content-type]]
            [ring.middleware.cors :refer [wrap-cors]]
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

(defn clue->coords [{:keys [direction x y distance]}]
  (if (= direction :right)
    (map #(vector % y) (range (inc x) (inc (+ x distance))))
    (map #(vector x %) (range (inc y) (inc (+ y distance))))))

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
       ;; one lvar is created for each entry on the board
       (reduce (fn [acc lvp] (assoc acc lvp (l/lvar))) (sorted-map))))

(defn clues->lvars-map
  "The lvp-lvar-map is a sorted map that has one lvar for every entry (coordinate pair).
  The x-shape is needed for lvar-group generation later, and would require re-computations
  if not returned now along with the lvp-lvar-map."
  [clues]
  (let [coords-2 (->> clues (mapcat clue->coords) distinct)
        x-shape (-> (apply max-key first coords-2) first inc)]
    {:lvp-lvar-map (generate-lvp-lvar-map x-shape coords-2)
     :x-shape x-shape}))

(defn clues->lvar-groups [clues x-shape lvp-lvar-map]
  (->> clues
       (map (fn [{:keys [direction sum] :as clue}]
              {:sum sum
               :lvars (->> clue
                           clue->coords
                           (map #(x-shape-coords->lvp x-shape %))
                           (map #(get lvp-lvar-map %)))}))))

(defn sumo
  "from https://spin.atomicobject.com/2015/12/14/logic-programming-clojure-finite-domain-constraints/
  (and https://blog.taylorwood.io/2018/05/10/clojure-logic.html )"
  [vars sum]
  (l/fresh [vhead vtail run-sum]
    (l/conde
     [(l/== vars ()) (l/== sum 0)]
     [(l/conso vhead vtail vars)
      (fd/+ vhead run-sum sum)
      (sumo vtail run-sum)])))

(defn adds-up [{:keys [sum lvars]}]
  (sumo lvars sum))

(defn clue-notation->solution-vector [clue-notation]
  (let [clues (clue-notation->clues clue-notation)
        {:keys [lvp-lvar-map x-shape]} (clues->lvars-map clues)
        all-lvars (vals lvp-lvar-map)
        lvar-groups (clues->lvar-groups clues x-shape lvp-lvar-map)
        is-single-digit #(fd/in % (apply fd/domain (range 1 10)))]
    (-> (l/run* [q]
          (l/== q all-lvars)
          (l/everyg is-single-digit all-lvars)
          (l/everyg adds-up lvar-groups)
          (l/everyg #(fd/distinct (:lvars %)) lvar-groups))
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
