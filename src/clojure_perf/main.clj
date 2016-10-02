(ns clojure-perf.main
  (:require [clojure.pprint :as pprint]
            [clojure.core.reducers :as r]
            [criterium.core :refer [quick-bench with-progress-reporting]])
  (:gen-class))

(defmacro measure
  [& body]
  (let [expr (if (second body) (cons 'do body) (first body))]
    `(do (pprint/write '~expr :dispatch pprint/code-dispatch)
         (pprint/fresh-line)
         (quick-bench ~expr)
         (pprint/fresh-line))))

(defn measure-seq-starts-with
  [a b]
  (measure
    (= b (take (count b) a)))

  (let [va (vec a)
        vb (vec b)]
    (measure
      (= vb (subvec va 0 (count vb)))))

  (measure
    (every? true? (map = b (concat a (repeat (Object.))))))

  (measure
    (loop [coll a subcoll b]
      (if-not (seq subcoll)
        true
        (if-not (seq coll)
          false
          (and (= (first coll) (first subcoll))
               (recur (next coll) (next subcoll))))))))

(defn measure-map-into-vector
  [coll f]
  (measure
    (vec (map f coll)))

  (measure
    (mapv f coll))

  (measure
    (into [] (r/map f coll)))

  (measure
    (into [] (map f) coll))

  (measure
    (loop [in coll
           out (transient [])]
      (if-not (seq in)
        (persistent! out)
        (recur (next in)
               (conj! out (f (first in)))))))

  (measure
    (loop [i 0
           in coll
           out (transient (vec coll))]
      (if-not (seq in)
        (persistent! out)
        (recur (inc i)
               (next in)
               (assoc! out i (f (first in))))))))


(defn measure-map-filter-into-vector
  [coll f pred]
  (measure
    (vec (filter pred (map f coll))))

  (measure
    (filterv pred (mapv f coll)))

  (measure
    (into [] (r/filter pred (r/map f coll))))

  (measure
    (into [] (comp (map f) (filter pred)) coll))

  (measure
    (loop [in coll
           out (transient [])]
      (if-not (seq in)
        (persistent! out)
        (let [elem (f (first in))]
          (recur (next in)
                 (if (pred elem)
                   (conj! out elem)
                   out))))))

  (measure
    (loop [in coll
           out (transient [])]
      (if-not (seq in)
        (persistent! out)
        (let [elem (f (first in))]
          (recur (next in)
                 (if (pred elem)
                   (conj! out elem)
                   out)))))))

(defn -main
  [& raw-args]
  (measure-seq-starts-with (range 1000) (range 1000))
  (measure-map-into-vector (range 1000) inc)
  (measure-map-filter-into-vector (range 1000) inc even?))
