(ns asparagus.boot.types
  (:refer-clojure :exclude [parents >= <=])
  (:require [asparagus.boot.prelude :as p
             :refer [$ $vals]]))

;; here is an attempt to hide direct interop
;; the goal is ultimatly to provide a more readable and powerful
;; way to create generic functions, see asparagus.boot.generics

(defn- cmap [& xs]
  ($vals (apply hash-map xs)
         vector))

(def atoms
  (cmap
    :fun clojure.lang.Fn
    :num java.lang.Number
    :str java.lang.String
    :sym clojure.lang.Symbol
    :key clojure.lang.Keyword))

(def colls
  {:seq [clojure.lang.ISeq]
   :map [clojure.lang.PersistentArrayMap
         clojure.lang.PersistentHashMap]
   :set [clojure.lang.IPersistentSet]
   :vec [clojure.lang.IPersistentVector]})

(def prims
  (merge (cmap :nil nil)
    atoms colls))

(def groups
  {:prim (p/kset prims)
   :atom (p/kset atoms)
   :coll (p/kset colls)
   :word #{:key :str :sym}
   :line #{:vec :seq}
   :hash #{:map :set}})

;; state --------------------------------------------

;; the global type registry
;; prims key holds a map from type-keyword -> class
;; groups key hold a map from type-keyword -> #{type-keyword}
(defonce reg
  (atom
    {:prims prims
     :groups groups}))

(defn group+
  "add a group to the type registry"
  [k members]
  (swap! reg update-in
    [:groups k]
    (fn [x#] (into (or x# #{}) members))))

(defn prim+
  "add a prim to type registry"
  [k classes groups]
  (swap! reg assoc-in [:prims k] classes)
  (doseq [g groups] (group+ g k)))

;; api ----------------------------------------------

(defmacro regfn [name doc & cases]
  (assert (string? doc) "doc please")
  (if (vector? (first cases))
    `(regfn ~name ~doc (~@cases))
    `(defn ~name ~doc
       ([x#] (~name @reg x#))
       ~@cases)))

(regfn childs
  "child seq"
  [reg k]
  (when-let [cs (get-in reg [:groups k])]
    (concat cs (mapcat (partial childs reg) cs))))

(regfn parents
  "parents seq"
  [{:as reg gs :groups} x]
  (when-let [ps (keys (filter (fn [[_ xs]] (xs x)) gs))]
    (concat ps (mapcat (partial parents reg) ps))))

(regfn childof
  "is x a child of y?"
  [x y] ((set (childs y)) x))

(regfn parentof
  "is x a parent of y?"
  [x y] ((set (parents y)) x))

(regfn >=
  "same or parentof"
  [x y]
  (or (parentof x y)
    (= x y)))

(regfn <=
  "same or childof"
  [x y]
  (or (childof x y)
      (= x y)))

(regfn classes

  "returns all classes for a type
   registry can be passed as first argument
   if not, global registry is derefered and used"

  [{:keys [prims groups]} t]

  (cond

    (class? t) [t]

    (= :any t)
    (cons Object
      (-> (vals prims)
        flatten p/remnil))

    (set? t)
    (mapcat classes t)

    :else
    (or (prims t)
      (when-let [cs (groups t)]
        (classes cs)))))

(regfn details
  "details about a type"
  [{:keys [prims groups]} t]
  [t (if-let [c (prims t)]
       c ($ (groups t) classes))])

(defn isa
  "test if something belongs to a type"
  ([k] (partial isa k))
  ([k x] (isa @reg k x))
  ([reg k x]
   (reduce
     (fn [a c] (or a (instance? c x)))
     nil (classes reg k))))

(defn all-types
  "return a set containing all types of a registry
   or of the global registry"
  ([] (all-types @reg))
  ([reg]
   (into #{:any}
     (concat
       (p/kset (reg :prims))
       (p/kset (reg :groups))))))

(def builtin-types
  (into #{:any} (concat (keys prims) (keys groups))))

;; tests --------------------------------------------

(p/asserts
  (classes :nil)
  (classes :coll)
  (classes :prim)
  (classes :any)
  (details :vec))

(p/asserts
  (isa :any)
  (isa :line ())
  (isa :line [1 2 3])
  (not (isa :line {}))
  (isa :word 'a)
  (isa #{:sym :key} 'a)
  ((isa :word) :pouet))

(p/asserts
  (parents @reg :vec)
  (childs :coll))

;; fast-isa -----------------------------------------

(def preds-symbols
  {:fun `fn?
   :vec `vector?
   :seq `seq?
   :set `set?
   :map `map?
   :num `number?
   :key `keyword?
   :sym `symbol?
   :str `string?
   :nil `nil?})

(defn class->symbol [c]
  (-> (str c)
      (clojure.string/split #" ")
      second symbol))

(defn symbolic-preds->or-form [ps]
  (if (= (count ps) 1)
    (first ps)
    `(clojure.core/or ~@ps)))

(defn compile-pred-map
  ([] (compile-pred-map @reg))
  ([reg]
   (let [gsym (gensym)

         prim?
         (set (keys (:prims reg)))

         prims-preds
         (reduce
          (fn [a [k cs]]
            (assoc
             a k
             (if-let [p (preds-symbols k)]
               (list p gsym)
               (->> (map class->symbol cs)
                    (map #(list `instance?  % gsym))
                    symbolic-preds->or-form))))
          {} (:prims reg))

         groups-preds
         (reduce
          (fn [a k]
            (assoc
             a k
             (->> (filter prim? (childs k))
                  (map prims-preds)
                  symbolic-preds->or-form)))
          {} (keys (:groups reg)))]

     (->> (merge prims-preds groups-preds)
          (map (fn [[k v]] [k `(fn [~gsym] (when ~v ~gsym))]))
          (into {})))))

(defn predmap
  ([] (predmap @reg))
  ([reg]
   (eval (compile-pred-map reg))))

(def builtin-preds
  (predmap {:prims prims :groups groups}))



