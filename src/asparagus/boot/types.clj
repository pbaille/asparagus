(ns asparagus.boot.types
  (:refer-clojure :exclude [parents >= <=])
  (:require [asparagus.boot.prelude :as p
             :refer [$ $vals]]))

;; this is a thin layer over clojure class hierarchy
;; the need for this comes from asparagus.boot.generics
;; which is a collection of tools to define generic functions

;; for a quick introduction by examples, see the tutorial section at the end of this file

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
      (and (= x y) x)))

(regfn <=
  "same or childof"
  [x y]
  (or (childof x y)
      (and (= x y) x)))

(regfn classes

  "returns all classes for a type
   registry can be passed as first argument
   if not, global registry is derefered and used"

  [{:keys [prims groups]} t]

  (cond

    (class? t) [t]

    (= :any t) (list Object)

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
  "test if something belongs to a type
   in practice, this function is quite slow
   for fast typechecks see 'type preds' section below"
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

;; type preds -----------------------------------------

;; in this section, we will compile a fast predicate for each type (prims and groups)
;; and store all those predicates in #'builtin-preds
;; I'm thinking about user defined types, maybe the predicate compilation should be somehow automated or at least eased

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

;; holds a map of typetag -> guard
(def guards (predmap @reg))

(defmacro compile-guards!
  "recompile the guards map, used by group+ and prim+
   not intended to be used directly"
  []
  `(def ~'guards (predmap @reg)))

;; extension ------------------------------------------

(defn group+
  "add a group to the type registry
   k: the name of the group (a keyword)
   members: the list of types that belongs to this group"
  [k members]
  (swap! reg update-in
         [:groups k]
         (fn [x#] (into (or x# #{}) members)))
  (compile-guards!)
  @reg)

(defn prim+
  "add a prim to type registry
   k: the name of the type (a keyword)
   classes: the clojure classes that correspond to the type
   groups: the groups your type belongs to"
  [k classes groups]
  (swap! reg assoc-in [:prims k] classes)
  (doseq [g groups] (group+ g [k]))
  (compile-guards!)
  @reg)

;; tuto -----------------------------------------------

;; asparagus.boot.type is a thin and simple layer on top of clojure's class hierarchy
;; lets first inpect the registry, which old the state of the system

'(clojure.pprint/pprint  @reg)

;; looks like:
'{:prims
  {:num [java.lang.Number],
   :fun [clojure.lang.Fn],
   :vec ...},
  :groups
  {:prim #{:num :fun :vec :key :sym :str :nil :seq :set :map},
   :atom #{:num :fun :key :sym :str},
   :coll ...}}

;; so we use keyword to represent what I will refer from now as 'typetags'

;; it contains 2 keys

;; :prims which holds a map of typetag -> [class]
;; it simply stays that a typetag correspond to some classes
;; for example, the entry: :map -> [clojure.lang.PersistentArrayMap clojure.lang.PersistentHashMap]
;; stays that :map type tag correspond to the classes clojure.lang.PersistentArrayMap and clojure.lang.PersistentHashMap
;; a typetag can correspond to several classes

;; :groups which holds a map of typetag -> #{typetag}
;; it lets you group several typetags together under another typetag
;; as an example, the entry: :coll -> #{:vec :seq :set :map}
;; stays that :vec, :seq, :set and :map belong to the :coll group

;; you can extend the registry like this

;; adding a primitive
(prim+ :char ;; the introduced typetag 
       [java.lang.Character] ;; the classes that belongs to it
       [:prim :atom] ;; the groups it belongs to
       )

;; enriching or declaring a group
(group+ :hash ;; the introduced or enriched typetag
        [:map :set] ;; the members that belongs to it
        )

;; inspection utilities

(childs :hash) ;;=> (:set :map)

(childof :set :hash) ;;=> :set
(childof :vec :hash) ;;=> nil

;; >= behaves like childof but is also true if the two given typetag are equals
(<= :hash :hash) ;;=> :hash
(<= :map :hash) ;;=> :map
(<= :vec :hash) ;;=> nil

(parents :map) ;;=> (:prim :coll :hash)

(parentof :hash :map) ;;=> :hash
(parentof :hash :vec) ;;=> nil

;; >= behaves like parentof but is also true if the two given typetag are equals
(>= :hash :hash) ;;=> :hash
(>= :hash :map) ;;=> :hash
(>= :hash :vec) ;;=> nil

;; you can list all classes that belongs to a typetag

(classes :word) ;;=> (clojure.lang.Keyword clojure.lang.Symbol java.lang.String)

(all-types)
;; #{:num :fun :hash :vec :key :coll :sym
;;   :str :line :word :nil :seq :set :atom
;;   :map :prim :char :any}

;; isa lets you test if something belongs to a typetag
(isa :vec []) ;;=> true
(isa :vec "aze") ;;=> false

;; isa is kinf of slow, it is enough for non critical application (like compile time stuff)
;; if you want fast typechecks you can use those, for any registry update (prim+, group+) it is recomputed

(let [coll? (:coll guards)]
  (coll? [1 2]) ;;=> [1 2]
  (coll? "yo") ;;=> nil
  )






