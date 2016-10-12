(ns clojure-perf.macros
  (:require [clojure.pprint :as pprint]
            [clojure.string :as string]
            [criterium.core :as criterium]))

(def ^:private dividing-line-width 80)
(def ^:private dividing-line-thin (apply str (repeat dividing-line-width \-)))
(def ^:private dividing-line-thick (apply str (repeat dividing-line-width \=)))
(def ^:dynamic compared-result-atom nil)

(defmacro measure
  [& body]
  (let [expr (if (second body) (cons 'do body) (first body))]
    `(do (pprint/write '~expr :dispatch pprint/code-dispatch)
         (pprint/fresh-line)
         (let [report# (criterium/quick-benchmark ~expr nil)
               output# (with-out-str (criterium/report-result report#))
               lines# (-> output# string/trim-newline string/split-lines)
               first-result# (if (some? compared-result-atom)
                               @compared-result-atom
                               ::nondeterministic-result)]
           (when (not= ::nondeterministic-result first-result#)
             (if (= ::no-first-result first-result#)
               (reset! compared-result-atom
                       (if (and (seq (:results report#))
                                (next (:results report#))
                                (apply = (:results report#)))
                         (first (:results report#))
                         ::nondeterministic-result))
               (when-not (every? (partial = first-result#)
                                 (:results report#))
                 (throw (ex-info "Return value differs from other measurements inside benchmark."
                                 {:expected first-result#
                                  :actual (first (filter (partial not= first-result#)
                                                         (:results report#)))})))))
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
            (binding [compared-result-atom (atom ::no-first-result)]
              (let ~~(into [] (mapcat (fn [arg] [(list 'quote arg) arg])) args)
                ~~@(map (fn [form] (list 'quote form)) body)))))))
