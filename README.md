# clojure-perf

A simple harness for evaluating the performance of different approaches to common operations in Clojure.

## Usage

Fork to add your own measurements, then `lein run`. Outputs Clojure-formatted code with comments to `stdout`.

## Sample output

```clj
'
[;================================================================================
  map-filter-into-vector
    coll (range 1000)
    f inc
    pred even?
];================================================================================

(vec (filter pred (map f coll)))
;
; Evaluation count : 16356 in 6 samples of 2726 calls.
;              Execution time mean : 37,981743 µs
;     Execution time std-deviation : 1,291759 µs
;    Execution time lower quantile : 36,705455 µs ( 2,5%)
;    Execution time upper quantile : 40,106614 µs (97,5%)
;                    Overhead used : 1,720351 ns
;
; Found 1 outliers in 6 samples (16,6667 %)
;   low-severe   1 (16,6667 %)
;  Variance from outliers : 13,8889 % Variance is moderately inflated by outliers
; --------------------------------------------------------------------------------

(into [] (comp (map f) (filter pred)) coll)
;
; Evaluation count : 40152 in 6 samples of 6692 calls.
;              Execution time mean : 15,386098 µs
;     Execution time std-deviation : 376,575643 ns
;    Execution time lower quantile : 14,928971 µs ( 2,5%)
;    Execution time upper quantile : 15,934518 µs (97,5%)
;                    Overhead used : 1,720351 ns
;
; Found 1 outliers in 6 samples (16,6667 %)
;   low-severe   1 (16,6667 %)
;  Variance from outliers : 13,8889 % Variance is moderately inflated by outliers
; --------------------------------------------------------------------------------

(persistent!
  (reduce
    (fn [out elem]
      (let [out-elem (f elem)]
        (if (pred out-elem) (conj! out out-elem) out)))
    (transient [])
    coll))
;
; Evaluation count : 46536 in 6 samples of 7756 calls.
;              Execution time mean : 13,287228 µs
;     Execution time std-deviation : 233,182419 ns
;    Execution time lower quantile : 12,925394 µs ( 2,5%)
;    Execution time upper quantile : 13,508232 µs (97,5%)
;                    Overhead used : 1,720351 ns
; --------------------------------------------------------------------------------

(loop [in coll out (transient [])]
  (if (empty? in)
    (persistent! out)
    (let [elem (f (first in))]
      (recur (next in) (if (pred elem) (conj! out elem) out)))))
;
; Evaluation count : 31824 in 6 samples of 5304 calls.
;              Execution time mean : 19,464396 µs
;     Execution time std-deviation : 528,708695 ns
;    Execution time lower quantile : 18,989846 µs ( 2,5%)
;    Execution time upper quantile : 20,216732 µs (97,5%)
;                    Overhead used : 1,720351 ns
; --------------------------------------------------------------------------------
```

## License

Copyright © 2016 Mats Gisselson

Distributed under the Eclipse Public License either version 1.0 or (at your option) any later version.
