(ns asparagus.boot.types
  (:refer-clojure :exclude [parents >= <=])
  (:require [asparagus.boot.prelude :as p
             :refer [cs $ $vals]]
            [asparagus.boot.state :refer [state]]
            [clojure.core :as c]
            [clojure.set :as set]))

;; this is a thin layer over clojure class hierarchy
;; the need for this comes from asparagus.boot.generics
;; which is a collection of tools to define generic functions

;; for a quick introduction by examples, see the tutorial section at the end of this file

(do :builtins

    (defn- cmap [& xs]
      ($vals (apply hash-map xs)
             hash-set))

    (def atoms
      (cmap
       :fun clojure.lang.Fn
       :num java.lang.Number
       :str java.lang.String
       :sym clojure.lang.Symbol
       :key clojure.lang.Keyword))

    (def colls
      {:lst #{clojure.lang.ISeq}
       ;;:lst #{clojure.lang.PersistentList}
       :map #{clojure.lang.PersistentArrayMap
              clojure.lang.PersistentHashMap}
       :set #{clojure.lang.IPersistentSet}
       :vec #{clojure.lang.IPersistentVector}})

    (def prims
      (merge (cmap :nil nil)
             atoms colls))

    (def groups
      {:prim (p/kset prims)
       :atom (p/kset atoms)
       :coll (p/kset colls)
       :word #{:key :str :sym}
       :line #{:vec :lst}
       :hash #{:map :set}})

    (def preds-symbols
      {:fun `fn?
       :vec `vector?
       ;:seq `seq?
       :lst `seq?
       :set `set?
       :map `p/holymap?
       :num `number?
       :key `keyword?
       :sym `symbol?
       :str `string?
       :nil `nil?}))

(do :registry
    
    ;; the global type registry
    ;; prims key holds a map from type-keyword -> class
    ;; groups key hold a map from type-keyword -> #{type-keyword}

    (swap! state assoc :types (merge prims groups))

    (defn get-reg []
      (:types @state))

    (defn get-type [t]
      (get (get-reg) t))

    (defn get-guards []
      (:guards @state))

    (defn get-guard [t]
      (get (get-guards) t))

    #_(p/pp @reg)

    (defn all-paths
      ([]
       (all-paths (get-reg)))
      ([x]
       (all-paths x (map vector (keys x))))
      ([x lines]
       (doall
        (mapcat
         (fn [l]
           (let [ll (last l)
                 xs (seq (get x ll))
                 overlap (seq (set/intersection (set l) (set xs)))]
             (cs (not xs) [l]
                 overlap (throw (Exception. (str "cycle! " l)))
                 (all-paths x (map (partial conj l) xs)))))
         lines))))

    #_(all-paths)

    (defn cyclic? [x]
      (try (not (all-paths x))
           (catch Exception e true)))

    #_(cyclic? @reg)

    (defmacro regfn
      [name doc & cases]
      (assert (string? doc) "doc please")
      (if (vector? (first cases))
        `(regfn ~name ~doc (~@cases))
        `(defn ~name ~doc
           ([x#] (~name (get-reg) x#))
           ~@cases))))

(do :basics

    (regfn childs
           "children seq"
           [x k]
           (loop [ret [] ps (all-paths x [[k]])]
             (if-let [ps (seq (filter seq ps))]
               (let [new (set/difference (set (map first ps)) (set ret))]
                 (recur (concat ret new)
                        (map rest ps)))
               (vec (rest ret)))))

    (regfn parents
           "parents seq"
           [reg x]
           (when-let [ps (keys (filter (fn [[_ xs]] (xs x)) reg))]
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

           [reg t]

           (cond

             (class? t) [t]

             (= :any t) (list Object)

             (set? t)
             (mapcat #(classes reg %) t)

             (= :nil t) [nil]

             :else
             (when-let [cs (reg t)]
               (->> cs
                    (mapcat #(childs reg %))
                    (concat cs)
                    (filter class?)))))

    (defn all-types
      "return a set containing all types of a registry
   or of the global registry"
      ([] (all-types (get-reg)))
      ([reg]
       (into #{:any} (p/kset reg))))

    (def builtin-types
      (into #{:any} (concat (keys prims) (keys groups)))))

(do :assertions

    (p/asserts
     (classes :nil)
     (classes :coll)
     (classes :prim)
     (classes :any)
     (classes :sym)
     (classes #{:sym :key}))

    (p/asserts
     (parents :vec)
     (childs :coll))
    )

(do :preds

    ;; in this section, we will compile a fast predicate for each type (prims and groups)
    ;; and store all those predicates in #'builtin-preds

    (defn class->symbol [c]
      (-> (str c)
          (clojure.string/split #" ")
          second symbol))

    (defn symbol->class [s]
      (if (symbol? s)
        (let [k (resolve s)]
          (if (class? k) k
              (p/error "not a resolvable class symbol")))
        (p/error "symbol->class needs a symbol and got: " s)))

    (defn ensure-class [x]
      (if (class? x) x (symbol->class x)))

    (defn symbolic-preds->or-form [ps]
      (if (= (count ps) 1)
        (first ps)
        `(clojure.core/or ~@ps)))

    (defn symbolic-pred-body [reg k gsym]
      (cs [psym (preds-symbols k)]
          (list psym gsym)
          (class? k)
          (list `instance?  (class->symbol k) gsym)
          [members (reg k)]
          (symbolic-preds->or-form
           (map #(symbolic-pred-body reg % gsym) members))))

    #_(symbolic-pred-body @reg :hash 'x)

    (defn symbolic-pred [reg k]
      (let [gsym (gensym)]
        `(fn [~gsym] (when ~(symbolic-pred-body reg k gsym) ~gsym))))

    #_(symbolic-pred @reg :atom)
    #_(symbolic-pred (assoc @reg :iop #{:hash clojure.lang.AMapEntry}) :iop)

    (defn compile-pred-map
      ([] (compile-pred-map (get-reg)))
      ([reg]
       (->> reg
            (map (fn [[k v]] [k (symbolic-pred reg k)]))
            (into {}))))

    (defn predmap
      ([] (predmap (get-reg)))
      ([reg] (eval (compile-pred-map reg))))

    (def builtin-preds (predmap))

    (defmacro sync-guards!
      "recompile the guards map, used by group+ and prim+
       not intended to be used directly"
      [] `(swap! state assoc :guards (predmap (get-reg))))

    ;; initializing guards with builtin types
    (sync-guards!)

    (defn isa

      "check if 'x is of type 'tag
       a state can be explicitly passed as first argument,
       else the global state is used"

      ([tag] (partial isa tag))
      ([tag x] (isa @state tag x))
      ([state tag x]
       (cs [guard (get-in state [:guards tag])]
           (guard x)
           (set? tag)
           (loop [[t1 & ts] tag]
             (or (isa state t1 x)
                 (when ts (recur ts)))))))

    (p/asserts
     (isa :any)
     (isa :line ())
     (isa :line [1 2 3])
     (not (isa :line {}))
     (isa :word 'a)
     (isa #{:sym :key} 'a)
     ((isa :word) :pouet))

    
    )

(do :extension

    (defn conj-type [reg {:keys [tag childs parents]}]
      (reduce
       (fn [reg p]
         (update reg p (fnil conj #{}) tag))
       (update reg tag (fnil into #{}) childs)
       parents))

    (defmacro tag+

      "add a type tag to the type registry (living in asparagus.boot.state/state)
       tag: the typetag we are defining/extending (a keyword)
       childs: a seq of other typetags or classes that belongs to the defined tag
       parents: a seq of other typetags that the defined tag belongs to
       & impls: optional generic implementations for the defined tag"

      ([{:keys [tag childs parents impls]}]
       (let [exists? ((get-reg) tag)
             generic-updates
             (if exists? (cons tag parents) parents)]
         `(do (swap! state
                     update :types
                     conj-type
                     {:tag ~tag
                      :childs ~childs
                      :parents ~parents})
              (asparagus.boot.generics/sync-types! ~(vec generic-updates))
              ~(when impls `(asparagus.boot.generics/type+ ~tag ~@impls))
              (sync-guards!))))

      ([tag childs]
       `(tag+ ~tag ~childs []))
      ([tag childs parents & impls]
       `(tag+ ~{:tag tag :childs childs :parents parents :impls (vec impls)})))

    (defmacro type+

      "declare a new usertype (a clojure record)
       tag: the typetag (keyword) corresponding to our freshly created record
       fields: the fields of our record
       parents: a seq of other typetags that our type belongs to
       & impls: optional generic implementations for the defined type"

      ([{:as spec
         :keys [tag parents impls fields childs class-sym]}]
       (let [class-sym (or class-sym (symbol (clojure.string/capitalize (name tag))))
             spec (update spec :childs (fnil conj []) class-sym)]
         `(do (defrecord ~class-sym ~fields)
              (tag+ ~spec))))
      ([tag fields]
       `(type+ ~tag ~fields []))
      ([tag fields parents & impls]
       `(type+ ~{:tag tag :parents parents :impls (vec impls) :fields fields})))

    (comment

      (tag+ :fop [:vec :set] [:hash])

      (type+ :pouet [iop foo] nil
             #_(g1 [x] "g1foo"))

      (type+ {:tag :pouet
              :fields [iop foo]
              :class-sym POUUUUUET
              #_(g1 [x] "g1foo")})

      (map macroexpand (macroexpand '(type+ :pouet [iop foo] [:hash]
                                            (g1 [x] "g1foo"))))

      (clojure.walk/macroexpand-all '(type+ :pouet [iop foo] [:hash]
                                            (g1 [x] "g1foo")))

      (asparagus.boot.generics/g1 (Pouet. 1 2)))

    )

;; tuto -----------------------------------------------

(comment

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
 (prim+ :char                ;; the introduced typetag 
        [java.lang.Character] ;; the classes that belongs to it
        [:prim :atom]         ;; the groups it belongs to
        )

 ;; enriching or declaring a group
 (group+ :hash      ;; the introduced or enriched typetag
         [:map :set] ;; the members that belongs to it
         )

 ;; inspection utilities

 (childs :hash) ;;=> (:set :map)

 (childof :set :hash) ;;=> :set
 (childof :vec :hash) ;;=> nil

 ;; >= behaves like childof but is also true if the two given typetag are equals
 (<= :hash :hash) ;;=> :hash
 (<= :map :hash)  ;;=> :map
 (<= :vec :hash)  ;;=> nil

 (parents :map) ;;=> (:prim :coll :hash)

 (parentof :hash :map) ;;=> :hash
 (parentof :hash :vec) ;;=> nil

 ;; >= behaves like parentof but is also true if the two given typetag are equals
 (>= :hash :hash) ;;=> :hash
 (>= :hash :map)  ;;=> :hash
 (>= :hash :vec)  ;;=> nil

 ;; you can list all classes that belongs to a typetag

 (classes :word) ;;=> (clojure.lang.Keyword clojure.lang.Symbol java.lang.String)

 (all-types)
 ;; #{:num :fun :hash :vec :key :coll :sym
 ;;   :str :line :word :nil :seq :set :atom
 ;;   :map :prim :char :any}

 ;; isa lets you test if something belongs to a typetag
 (isa :vec [])   ;;=> true
 (isa :vec "aze") ;;=> false

 ;; isa is kinf of slow, it is enough for non critical application (like compile time stuff)
 ;; if you want fast typechecks you can use those, for any registry update (prim+, group+) it is recomputed

 (let [coll? (:coll guards)]
   (coll? [1 2]) ;;=> [1 2]
   (coll? "yo")  ;;=> nil
   ))








