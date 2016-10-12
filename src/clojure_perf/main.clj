(ns clojure-perf.main
  (:require [clojure.core.reducers :as r]
            [clojure-perf.macros :refer [defbench measure]])
  (:gen-class))

(defbench predicate-find
  [coll pred]
  (measure
    (first (filter pred coll)))

  (measure
    (reduce (fn [_ elem]
              (when (pred elem)
                (reduced elem)))
            nil
            coll))

  (measure
    (r/reduce (fn [_ elem]
                (when (pred elem)
                  (reduced elem)))
              nil
              coll))

  (measure
    (loop [in coll]
      (when (seq in)
        (let [elem (first in)]
          (if (pred elem)
            elem
            (recur (next in))))))))

(defbench seq-starts-with
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
      (if (empty? subcoll)
        true
        (if (empty? coll)
          false
          (and (= (first coll) (first subcoll))
               (recur (next coll) (next subcoll))))))))

(defbench map-into-vector
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
      (if (empty? in)
        (persistent! out)
        (recur (next in)
               (conj! out (f (first in)))))))

  (measure
    (loop [i 0
           in coll
           out (transient (vec coll))]
      (if (empty? in)
        (persistent! out)
        (recur (inc i)
               (next in)
               (assoc! out i (f (first in)))))))

  (let [vcoll (vec coll)]
    (measure
      (let [size (count vcoll)
            out (transient [])]
        (loop [i 0]
          (if (= i size)
            (persistent! out)
            (do (conj! out (f (vcoll i)))
                (recur (inc i)))))))))

(defbench map-filter-into-vector
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
    (persistent! (reduce (fn [out elem]
                           (let [out-elem (f elem)]
                             (if (pred out-elem)
                               (conj! out out-elem)
                               out)))
                         (transient [])
                         coll)))

  (measure
    (loop [in coll
           out (transient [])]
      (if (empty? in)
        (persistent! out)
        (let [elem (f (first in))]
          (recur (next in)
                 (if (pred elem)
                   (conj! out elem)
                   out)))))))

(defbench mapcat-into-vector
  [coll f]
  (measure
    (vec (mapcat f coll)))

  (measure
    (into [] (r/mapcat f coll)))

  (measure
    (into [] (mapcat f) coll))

  (measure
    (into [] (comp (map f) cat) coll))

  (measure
    (persistent! (reduce (fn [out elem]
                           (doseq [out-elem (f elem)]
                             (conj! out out-elem))
                           out)
                         (transient [])
                         coll)))
  (measure
    (loop [in coll
           out (transient [])]
      (if (empty? in)
        (persistent! out)
        (do (doseq [out-elem (f (first in))]
              (conj! out out-elem))
            (recur (next in)
                   out))))))

(defbench zip-into-vector
  [a b f]
  (measure
    (vec (map f a b)))

  (measure
    (mapv f a b))

  (measure
    (into [] (map f a b)))

  (measure
    (into [] (sequence (map f) a b))))

(defn -main
  [& raw-args]
  (predicate-find (range 1000) (partial = 500))
  (seq-starts-with (range 1000) (range 1000))
  (map-into-vector (range 1000) inc)
  (map-filter-into-vector (range 1000) inc even?)
  (mapcat-into-vector (range 1000) (partial repeat 4))
  (zip-into-vector (range 1000) (range 1000) +))
