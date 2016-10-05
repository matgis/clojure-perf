(ns clojure-perf.macros
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [criterium.core :as criterium]))

(def ^:private dividing-line-width 80)
(def dividing-line-thin (apply str (repeat dividing-line-width \-)))
(def dividing-line-thick (apply str (repeat dividing-line-width \=)))

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
         (println ";" dividing-line-thin)
         (pprint/fresh-line))))

(defmacro defbench
  [name args & body]
  `(defn ~name
     ~args
     (println)
     (println ";" '~name)
     (println ";" dividing-line-thick)
     (println)
     ~@body))
