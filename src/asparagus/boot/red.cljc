(ns asparagus.boot.red
  (:require [asparagus.boot.prelude :as p
             :refer [$ cp]])
  #?(:cljs (:require-macros [asparagus.boot.levels2
                             :refer [upd_ upd sugarize]])))

(defrecord Abstraction [val])

(defn abstraction? [x] (instance? Abstraction x))

(defn abstraction [val]
  (Abstraction. val))

(def A abstraction)

(defn level [x]
  (if (abstraction? x)
    (inc (level (:val x)))
    0))

(defn deep-val [x]
  (if (abstraction? x)
    (deep-val (:val x))
    x))

;; Declaration
;; --------------------------------------------------------------

(defprotocol ICombine (-combine [x y]))
(defprotocol IMergeInto (-merge-into [x y]))
(defprotocol IConcretize (-concretize [x y]))

(defn >>
  ([x] x)
  ([x y]
   (let [lx (level x)
         ly (level y)]
     (cond
       (satisfies? ICombine x) (-combine x y)
       (= lx ly) (-merge-into y x)
       (= lx (dec ly)) (-concretize y x)
       (> lx ly) (>> x (A (constantly y) (- lx ly)))
       (< lx ly) (>> (A (constantly x)) y))))
  ([x y & ys]
   (reduce >> (>> x y) ys)))

;; variations

(def >>* (partial apply >>))
(defn >>_ [& xs] #(reduce >> % xs))

;; Primitives extensions
;; --------------------------------------------------------------

(defn- zipmaps
  [f m1 m2]
  (reduce
    (fn [ret k]
      (assoc ret k (f (get m1 k) (get m2 k))))
    m1 (into (set (keys m1)) (keys m2))))

#?(:clj
   (extend-protocol IMergeInto

     nil
     (-merge-into [_ x] x)

     Object
     (-merge-into [y _] y)

     clojure.lang.Seqable
     (-merge-into [y x] (concat x y))

     clojure.lang.IPersistentVector
     (-merge-into [y x] (mapv >> x y))

     clojure.lang.IPersistentMap
     (-merge-into [y x] (zipmaps >> x y))

     clojure.lang.IPersistentSet
     (-merge-into [y x] (if x (into x y) y))

     clojure.lang.Fn
     (-merge-into [y x] (if x (comp x y) y)))

   :cljs
   (extend-protocol IMergeInto
     nil
     (-merge-into [_ x] x)
     default
     (-merge-into [y _] y)
     function
     (-merge-into [y x] (if x (comp x y) y))
     object
     (-merge-into [y x]
       (cond (not (coll? y)) y
             (map? y) (zipmaps >> x y)
             (vector? y) (mapv >> x y)
             (seq? y) (concat x y)
             (set? y) (into y x)))))

#?(:clj
   (extend-protocol IConcretize

     nil
     (-concretize [_ x] x)

     Object
     (-concretize [y x] y)

     clojure.lang.Fn
     (-concretize [y x] (y x))

     clojure.lang.Seqable
     (-concretize [y x]
       (map #(-concretize % x) y))

     clojure.lang.IPersistentVector
     (-concretize [y x]
       (mapv #(-concretize % x) y))

     clojure.lang.IPersistentMap
     (-concretize [y x]
       (p/$vals y #(-concretize % x)))

     clojure.lang.IPersistentSet
     (-concretize [y x]
       (into #{} (map #(-concretize % x) y))))

   :cljs
   (extend-protocol IConcretize
     nil
     (-concretize [_ x] x)
     default
     (-concretize [y _] y)
     function
     (-concretize [y x] (y x))
     object
     (-concretize [y x]
       (if (coll? y)
         (let [k #(-concretize % x)]
           (cond
             (map? y) (u/map-vals k y)
             (vector? y) (mapv k y)
             (seq? y) (map k y)
             (set? y) (into #{} (map k y))))
         y))))

(extend-type Abstraction
  IMergeInto
  (-merge-into [y x]
    (A (-merge-into (:val x) (:val y))))
  IConcretize
  (-concretize [y x]
    #_(p/prob 'concretize y x)
    (if (abstraction? (:val y))
      (A (-concretize (:val y) (:val x)))
      (-concretize (:val y) x))))

(defmethod clojure.pprint/simple-dispatch Abstraction [e]
  (clojure.pprint/simple-dispatch
   (let [lvl (level e)]
     (if (= 1 lvl)
       (list 'A  (:val e))
       (list (p/sym 'A (str lvl)) (deep-val e))))))

(defmacro §
  ([v] `(A ~v))
  ([pat & body]
   `(§ (fn [~pat] ~@body))))

(defmacro §§
  ([v] `(A (A ~v)))
  ([pat & body]
   `(§§ (fn [~pat] ~@body))))

(defmacro §§§
  ([v] `(A (A (A ~v))))
  ([pat & body]
   `(§§§ (fn [~pat] ~@body))))

(defmacro §_
  [& body]`(§ ~'_ ~@body))

(defmacro §§_
  [& body]`(§§ ~'_ ~@body))

(defmacro §§§_
  [& body]`(§§§ ~'_ ~@body))

(comment

  (>> 1 (§ inc))
  (>> 1 (>> (§ x (p/prob 'a x) (inc x))
            (§§ a (p/prob 'b a) [a a])))

  (>> 1 (§ [inc inc]))

  (>> 1 (>> (§ inc) (§ inc)))

  (>> 1 (>> (§ inc) (§_ [_ _])))
  ((>> 12 (>> inc (§§ u u))) 1)

  )

(defn macrocall? [[s & _]]
  (and (symbol? s)
       (:macro (meta (resolve  s)))))

(defn red-fn [e]
  (cp e
      seq?
      (if (macrocall? e)
        (cons (first e) ($ (next e) red-fn))
        (cons `>> ($ e red-fn)))
      p/holycoll? ($ e red-fn)
      e))

(defmacro red [& xs]
  (red-fn xs))

(macroexpand '(red 1 (§ (inc inc))))

(red
 (1 (§ (inc inc))))

;; Maps
;; --------------------------------------------------------------

(defn- split-kvs [xs]
  (loop [taken {} [x y & xs :as remains] xs]
    (if (keyword? x)
      (recur (assoc taken x y) xs)
      [taken remains])))

(defn hm
  "like & except that it can take a flat series of key values before updates
   (hm :a 1 :b 2 {:c 3} ...) <=> (& {:a 1 :b 2} {:c 3} ...)"
  ([] {})
  ([x] x)
  ([x y & ys]
   (if (keyword? x)
     (let [[m rs] (split-kvs ys)
           base (assoc m x y)]
       (if rs (>>* base rs) base))
     (>>* x y ys))))

(def hm* (partial apply hm))
