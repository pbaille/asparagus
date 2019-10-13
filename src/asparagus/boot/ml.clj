(ns asparagus.boot.ml
  (:require [asparagus.boot.prelude :as p]
            [clojure.core.match :as m]
            [clojure.core.match.protocols :as mp]
            [clojure.set :as set]))

(defmacro deft [n fields & body]
  (let [recsym (p/sym 'R_ n)
        constr (p/sym '-> recsym)
        predsym (p/sym n "?")]
    `(do (declare ~n ~predsym)
         (defrecord ~recsym ~fields ~@body)
         (def ~n (with-meta ~constr {::type-constructor true}))
         (defn ~predsym [x#] (instance? ~recsym x#))
         (defmethod clojure.pprint/simple-dispatch
           ~recsym [x#]
           (clojure.pprint/simple-dispatch (cons '~n (map (partial get x#) ~(mapv keyword fields)))))

         (defmethod print-method
           ~recsym [x# w#]
           (print-method (cons '~n (map (partial get x#) ~(mapv keyword fields)))
                         w#)))))

(defn type-constructor? [x]
  (some-> x meta ::type-constructor))

(defn type-constructor-varsym? [x]
  (and (symbol? x)
       (some-> x resolve deref type-constructor?)))

(defmethod m/emit-pattern ::type 
  [[c & xs]]
  (apply c xs))

(extend-protocol mp/ISyntaxTag
  clojure.lang.ISeq
  (syntax-tag [[x1 & xs]]
    (if (type-constructor-varsym? x1)
      ::type ::m/seq)))

(defmethod m/emit-pattern ::type [[c & ms]]
  #_(println "in ::type emit pattern" c ms)
  (m/emit-pattern (list (into {} (apply @(resolve c) ms))
                        :guard `(fn [x#] (~(p/sym c "?") x#)))))

;; slightly modified version of core.match version,
;; in order to not consider constructor symbol as duplicate wildcards
(defn wildcards-and-duplicates
  "Returns a vector of two elements: the set of all wildcards and the 
   set of duplicate wildcards.  The underbar _ is excluded from both."
  [patterns]
  (loop [remaining patterns seen #{} dups #{}]
    (if-let [patterns (seq remaining)]
      (let [pat (first patterns)
            pats (rest patterns)]
        (cond
          (or (= pat '_) (= pat '&))
          (recur pats seen dups)

          (symbol? pat)
          (if (contains? seen pat)
            (recur pats seen (conj dups pat))
            (recur pats (conj seen pat) dups))

          (vector? pat)
          (recur (concat pats pat) seen dups)

          (map? pat)
          (recur (concat pats (vals pat)) seen dups)

          (seq? pat)
          (cond
            (= (first pat) 'quote)
            (recur pats seen dups)

            (type-constructor-varsym? (first pat)) ;;           <<------- here
            (recur (concat pats (next pat)) seen dups)

            (= (first pat) :or)
            (let [wds (map wildcards-and-duplicates
                        (map list (take-nth 2 pat)))
                   mseen (apply set/union (map first wds))]
              (recur pats (set/union seen mseen)
                (apply set/union dups
                  (set/intersection seen mseen)
                  (map second wds))))

            (= (second pat) :as)
            (recur (concat pats (take-nth 2 pat)) seen dups)

            :else
            (recur (conj pats (first pat)) seen dups))
          :else
          (recur pats seen dups)))
      [seen dups])))

;; purely functional
(alter-var-root #'m/wildcards-and-duplicates
                (fn [_] wildcards-and-duplicates))


;; examples

(macroexpand '(deft fork [left right]))

(deft fork [left right])

(meta fork)

(fork 1 2)

(deft false-fork [left right])

(m/match [(fork 4 5) 2]
         [1 y] (+ 1 y)
         [(fork x y) z] [x y z]
         [(fork x y) (fork a b)] [x y a b])

(deft num [val])
(deft fork [left right])

(defmacro defm [name & clauses]
  (let [argv (vec (repeatedly (count (first clauses)) gensym))]
    `(defn ~name ~argv (m/match ~argv ~@clauses))))

(fn t [x y]
  (m/match [x y]
           [(num x) (num y)] (+ x y)
           [(fork (num x) (num y)) (num z)] [x y z]
           [x' y'] [x' y']))

(defm sum
  [(num x) (num y)] (num (+ x y))
  [(fork x y) z] (sum (sum x y) z)
  [x (fork y z)] (sum x (sum y z)))

(sum (fork (num 3) (fork (num 1) (num 2)))
     (num 4))
