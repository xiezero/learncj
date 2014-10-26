(ns learncj.logic
  (:refer-clojure :exclude [==])
  (:use [clojure.core.logic])
  (:require [clojure.core.logic.fd :as fd]))


;;
(defn queens-safe? [xs]
  "not attact"
  (if (empty? xs) true
               (and (apply distinct? xs)
                    (apply distinct? (map-indexed #(+ %1 %2) xs))
                    (apply distinct? (map-indexed #(- %2 %1) xs))
                    )))
(queens-safe? [1 0])
(queens-safe? '(1 3 5 7 2 0 6 4))

(defn draw-n-queen [queens]
  (for [x queens]
    (str (apply str (repeat x "[ ]")) "[*]" (apply str (repeat (dec (- (count queens) x)) "[ ]")) "\n")))

(println (draw-n-queen '(2 4 1 7 0 6 3 5) ))
(defn n-queens [n]
  (let [vars (zipmap (range n) (repeatedly n lvar))
        sum (repeatedly n lvar)
        mi (repeatedly n lvar)
        ]
    (run* [qs]
          (== qs vars)
          ;range
          (everyg #(fd/in % (apply fd/domain (range n))) (vals vars))
          (everyg #(fd/in % (apply fd/domain (range (* n 2)))) sum)
          (everyg #(fd/in % (apply fd/domain (range (- n) n))) mi)
          ;not in same cols
          (fd/distinct (vals vars))
          ;not in same diagonal
          (everyg (fn [[i x]] (fd/+ i x (nth sum i))) vars)
          (everyg (fn [[i x]] (fd/- i x (nth mi i))) vars)
          (fd/distinct sum)
          (fd/distinct mi)
          )))

(queens-safe? (vals (first (n-queens 8))))
(println (draw-n-queen (vals (first (n-queens 8)))))




