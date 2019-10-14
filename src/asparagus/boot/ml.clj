(ns asparagus.boot.ml
  (:require [clojure.core.match :as m]
            [clojure.core.match.protocols :as mp]
            [clojure.set :as set]))

;; utils

(defn mksym [& xs]
  (symbol (apply str (map name xs))))

(defn map-keys [f x]
  (zipmap (map f (keys x)) (vals x)))

;; type

(defmacro deft
  [n fields & body]
  (let [recsym (mksym 'R_ n)
        constr (mksym '-> recsym)
        predsym (mksym n "?")]
    `(do (declare ~n ~predsym)
         (defrecord ~recsym ~fields ~@body)
         (def ~n (with-meta ~constr {::type-constructor true}))
         (def ~(mksym 'map-> n) ~(mksym 'map-> recsym))
         (def ~predsym (with-meta (fn  [x#] (instance? ~recsym x#)) {::type-predicate true}))
         (defmethod clojure.pprint/simple-dispatch
           ~recsym [x#]
           (clojure.pprint/simple-dispatch (cons '~n (map (partial get x#) ~(mapv keyword fields)))))

         (defmethod print-method
           ~recsym [x# w#]
           (print-method (cons '~n (map (partial get x#) ~(mapv keyword fields)))
                         w#)))))

(defn type-constructor? [x]
  (some-> x meta ::type-constructor))

(defn type-predicate? [x]
  (some-> x meta ::type-predicate))

(defn type-constructor-varsym? [x]
  (and (symbol? x)
       (some-> x resolve deref type-constructor?)))

(defn type-predicate-varsym? [x]
  (and (symbol? x)
       (some-> x resolve deref type-predicate?)))

;; core.match extension

(do 
  (defmethod m/emit-pattern ::type 
    [[c & xs]]
    (apply c xs))

  (extend-protocol mp/ISyntaxTag
    clojure.lang.ISeq
    (syntax-tag [[x1 & xs]]
      (cond
        (type-constructor-varsym? x1) ::type
        (type-predicate-varsym? x1) ::pred
        :else ::m/seq)))

  (defmethod m/emit-pattern ::type [[c & ms]]
    #_(println "in ::type emit pattern" c ms)
    (m/emit-pattern (list (into {} (apply @(resolve c) ms))
                          :guard `(fn [x#] (~(mksym c "?") x#)))))

  (defmethod m/emit-pattern ::pred [[c x]]
    #_(println "in ::type emit pattern" c ms)
    (m/emit-pattern (list x :guard c)))

  ;; slightly modified version of core.match version,
  ;; in order to not consider constructors symbol as duplicate wildcards
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

              (or (type-predicate-varsym? (first pat))
                  (type-constructor-varsym? (first pat))) ;;           <<------- here
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
                  (fn [_] wildcards-and-duplicates)))

;; match functions

(defmacro fm [x & xs]
  (let [[name clauses] (if (symbol? x) [x xs] [(gensym) (cons x xs)])
        by-arity (group-by (comp count first) (partition 2 clauses))]
    `(fn ~name
       ~@(map (fn [[argv clauses]]
                `(~argv (m/match ~argv ~@(apply concat clauses))))
              (map-keys (fn [n] (vec (repeatedly n gensym)))
                        by-arity)))))

(defmacro defm
  "a simple pattern matched function"
  [name & clauses]
  `(def ~name (fm ~name ~@clauses)))

;; def constructor

(defmacro defc
  "another taste of deft, see tutorial"
  [name fields & body]
  (let [sym (gensym)]
    `(do
       (deft ~name ~fields)
       (let [~sym ~name]
         (defm ~name
           ~@(interleave (take-nth 2 body)
                        (map (fn [x] `(apply ~sym ~x)) (take-nth 2 (next body)))))))))

(do :tut


    (deft num [val])

    ;; defines a simple record with a field 'val

    ;;it can be instanciated like this

    (num 1) ;;=> (num 1)

    ;; it prints in a more concise way than default clojure record e.g (num 1)

    ;; we can access its field with normal clojure syntax

    (:val (num 1)) ;;=> 1

    ;; a predicate is available too

    (num? (num 1)) ;;=> true


    ;; a type can have several fields

    (deft fork [left right])

    ;; and take protocols implementation as defrecord do

    (deft myfun [f]
      clojure.lang.IFn
      (invoke [this x] ((:f this) x)))

    (= 1 ((myfun identity) 1))

    ;; pattern-matching

    (defm sum
      [(num x) (num y)] (num (+ x y))
      [(fork x y) z] (sum (sum x y) z)
      [x (fork y z)] (sum x (sum y z)))

    (= (sum (num 1) (num 2))
       (num 3))

    (= (num 10)
       (sum (fork (num 3) (fork (num 1) (num 2)))
            (num 4)))

    ;; fm for anoymous form
    (let [f (fm [x y] :a
                [x y z] :b)]
      (and (= :a (f 1 2))
           (= :b (f 1 2 3))))

    ;; sometimes you want the whole structure, not destructured

    (= ((fm [(num? x) (fork y z)] [x y z])
        ;; (num? x) is what I call a type-predicate pattern
        ;; it binds x to the whole structure
        (num 1)
        (fork 2 3))

       [(num 1) 2 3])


    ;; even a type has the same structure it will not match
    (comment
      (deft false-fork [left right])
      (sum (false-fork (num 1) (num 2))
           (num 1)))

    ;; defm can have several arities

    (defm myfun
      [0 x] :a
      [1 x] :b
      [x y] :c
      [0 x y] :d
      [x y z g] :e)

    (= :a (myfun 0 42))
    (= :b (myfun 1 'iop))
    (= :c (myfun 2 3))
    (= :d (myfun 0 :foo :bar))
    (= :e (myfun 0 :foo :bar :baz))

    ;; defc

    ;; defc define a new type, like deft
    ;; but with a pattern matched constructor function

    (defc duo [a b] ;; this is the same as deft
      ;; constructor cases
      ;; each case return the fields values
      [(num x) (num y)] [x y]
      [(fork x _) (fork _ y)] [x y]
      [x y] [x y]
      ;; the constructor can have several arities as long as it returns the required fields
      [(num x) (num y) z] [(+ x y) z]) 

    (duo (num 1) (num 2)) ;;=> (duo 1 2)
    (duo (fork :a :b) (fork :c :d)) ;;=> (duo :a :d)
    (duo :what :ever) ;=> (duo :what :ever)
    (duo (num 1) (num 2) 3) ;=> (duo 3 3)

    ;; extra features

    ;; maybe it could be nice to extend pattern matching with predicate syntax

    (m/match [(num 1) :foobar]
             [(num? x) y] [x y]) ;=> [(num 1) :foobar] x has been bound without destructuring

    ;; for this we should marktype predicate with meta tag and implement match

  )
