(ns asparagus.boot.defmacro
  (:require [asparagus.boot.prelude :as p]))

(defn eval-unquotes [e]
  (p/walk? e
           p/holycoll?
           (fn [x]
             (if-not (p/unquote? x)
               x (eval (second x))))))

(eval-unquotes '(+ 1 2 ~(+ 1 2)))

(defmacro defmac [name argv & body]
  `(defmacro ~name [~'& xs#]
     (let [~argv (eval-unquotes xs#)]
       ~@body)))
