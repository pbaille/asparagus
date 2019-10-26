(in-ns 'asparagus.core)

(cr/quick-bench (clojure.core/pos? 1))
;; Evaluation count : 112426284 in 6 samples of 18737714 calls.
;; Execution time mean : 3,806819 ns
;; Execution time std-deviation : 0,259767 ns
;; Execution time lower quantile : 3,450392 ns ( 2,5%)
;; Execution time upper quantile : 4,108933 ns (97,5%)
;; Overhead used : 1,820839 ns

(cr/quick-bench (!! (pos? 1)))
;; Evaluation count : 84289644 in 6 samples of 14048274 calls.
;; Execution time mean : 6,079716 ns
;; Execution time std-deviation : 0,213952 ns
;; Execution time lower quantile : 5,729717 ns ( 2,5%)
;; Execution time upper quantile : 6,275833 ns (97,5%)
;; Overhead used : 1,820839 ns

(let [f #(when (clojure.core/pos? %) %)]
  (cr/quick-bench (f 1)))
;; Evaluation count : 93116190 in 6 samples of 15519365 calls.
;; Execution time mean : 5,065023 ns
;; Execution time std-deviation : 0,361711 ns
;; Execution time lower quantile : 4,723565 ns ( 2,5%)
;; Execution time upper quantile : 5,469298 ns (97,5%)
;; Overhead used : 1,820839 ns

(time (dotimes  [_ 10000] (let [x 1] (cond (clojure.core/pos? x) x))))
(time (dotimes  [_ 10000] (?let [(pos? x) 1] x)))

