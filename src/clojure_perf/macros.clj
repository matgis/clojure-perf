(ns clojure-perf.macros
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [criterium.core :as criterium]))

(def ^:private dividing-line-width 80)
(def ^:private dividing-line-thin (apply str (repeat dividing-line-width \-)))
(def ^:private dividing-line-thick (apply str (repeat dividing-line-width \=)))

(defmacro measure
  [& body]
  (let [expr (if (second body) (cons 'do body) (first body))]
    `(do (pprint/write '~expr :dispatch pprint/code-dispatch)
         (pprint/fresh-line)
         (let [output# (with-out-str (criterium/quick-bench ~expr))
               lines# (-> output# string/trim-newline string/split-lines)]
           (println ";")
           (doseq [line# lines#]
             (println ";" line#)))
         (println ~(str "; " dividing-line-thin))
         (pprint/fresh-line))))

(defmacro defbench
  [name args & body]
  `(defmacro ~name
     ~args
     (let [pair# (gensym "pair")]
       `(do (println)
            (println \')
            (println ~~(str "[;" dividing-line-thick))
            (println ~~(str "  " name))
            (doseq [~pair# (sort-by first (quote ~~(mapv (fn [arg] [(str arg) arg]) args)))]
              (println "   " (first ~pair#) (second ~pair#)))
            (println ~~(str "];" dividing-line-thick))
            (println)
            (let ~~(into [] (mapcat (fn [arg] [(list 'quote arg) arg])) args)
              ~~@(map (fn [form] (list 'quote form)) body))))))
