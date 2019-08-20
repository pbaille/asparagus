(ns asparagus.boot2.generics
  (:require [asparagus.boot2.prelude :as p :refer [$ $vals sym pp]]
            [asparagus.boot2.types :as t]
            [asparagus.boot2.state :refer [state]]))

;; generic function
;; based on clojure protocols with some extra features

;; generics can be variadics
;; generics can be extended by arity
;; (in regular protocols, you have to implement all arity when extending your proto to your type
;; even if a parent class already implement those...)
;; there is some shortcuts when 2 or more types share an implementation
;; via the set syntax litteral e.g #{:type1 :type2}
;; it works with asparagus.boot.types (which is a thin layer on top of clojure class hierarchy)

(do :state

    (swap! state assoc :fns {})

    (defn get-reg []
      (:fns @state))

    (defn get-spec! [name]
      (p/assert (get-in @state [:fns name])
                (str "Cannot find spec " name))))

(do :impl

  (defn arities [cases]
    (->> cases (map first) (map count) set))

  (defn with-ns [ns sym]
    (symbol ns (str sym)))

  (defn derive-name [n]
    {:name n
     :pname (sym 'I n)
     :mname (sym 'p_ n)
     :ns (str *ns*)})

  (defn arify-name [n a]
    (sym n '_ (str a)))

  (defn variadic-argv? [x]
    ((set x) '&))

  (defn split-cases [xs]
    (let [{fixed nil
           variadic '&}
          (group-by (comp variadic-argv? first) xs)]
      (assert (#{0 1} (count variadic)))
      {:fixed fixed
       :variadic (first variadic)}))

  (defn format-variadic-case
    [[argv & members]]
    (cons (-> argv butlast butlast
            vec (conj (last argv)))
      members))

  (defn exprmap [xs]
    (p/redh
     (fn [a [t e]]
       (merge a (zipmap (t/classes t) (repeat e))))
     (reverse (partition 2 xs))))

  (defn casemap [cases]
    (p/redh
      (fn [a [argv & decls]]
        (let [arity (count argv)]
          (reduce
            (fn [a [c e]]
              (update a c assoc arity (list argv e)))
            a (exprmap decls))))
      cases))

  #_(casemap (:cases (@reg 'g2)))

  (defn type-extensions
    [{:keys [cases ns pname mname name]}]
    (map (fn [[c arities]]
           `(extend-type ~c
              ~@(mapcat (fn [[arity impl]]
                          [(with-ns ns (arify-name pname arity))
                           (cons (arify-name mname arity) impl)])
                  arities)))
         (casemap cases)))

  #_(type-extensions (@reg 'g2))

  (defn emit-impls [name cases]
    (mapcat
      (fn [[c m]]
        [c (cons name (vals m))])
      (casemap cases)))

  (defn variadify-argv [v]
    (vec (concat (butlast v) ['& (last v)])))

  (defn parse-case [x]
    (if (even? (count x))
      (concat (butlast x) [:any (last x)])
      x))

  (defn parse-cases [xs]
    (map parse-case
         (if (vector? (first xs))
           (list xs)
           xs)))

  (p/asserts
   (= (parse-cases '([a] :any 1))
      (parse-cases '(([a] 1))))
   (= (parse-cases '([a] :vec 1 1))
      (parse-cases '(([a] :vec 1 :any 1))))))

(do :parts

  (defn generic-spec [name body]

    (let [doc (when (string? (first body)) (first body))
          cases' (if doc (rest body) body)
          cases (parse-cases cases')
          {:keys [fixed variadic]} (split-cases cases)
          variadic (some-> variadic format-variadic-case)
          variadic-arity (some-> variadic first count)
          argvs (concat (map first fixed) (some-> variadic first vector))
          arities (set (map count argvs))
          cases (if-not variadic fixed (concat fixed [variadic]))]

      (assert (if variadic (= variadic-arity (apply max arities)) true)
        "arity error, fixed arity > variadic arity")
      (merge
        (derive-name name)
        {:variadic? (boolean variadic)
         :arities arities
         :sigs (map p/argv-litt arities)
         :cases cases
         :doc doc})))

  (defn registering-form [spec]
    `(swap! state assoc-in [:fns '~(:name spec)] '~spec))

  (defn extend-spec
    [spec extension-spec]
    (p/assert (every? (:arities spec) (:arities extension-spec))
              "unknown arity")
    (update spec :cases
            concat (:cases extension-spec)))

  (defn protocol-declaration-form
    [{:keys [pname mname sigs ns]}]
    `(do ~@(map (fn [argv]
                  (let [arity (count argv)]
                    `(defprotocol ~(with-ns ns (arify-name pname arity))
                       (~(arify-name mname arity) ~argv))))
                sigs)))

  (defn protocol-extension-form
    [{:keys [ns pname mname cases] :as spec}]
    `(do ~@(type-extensions spec)))

  (defn function-definition-form
    [{:keys [name pname mname
             sigs variadic? arities]}]
    (let [sigs (sort sigs)
          fixed-sigs (if variadic? (butlast sigs) sigs)
          vsig (last sigs)
          vsig-argv (variadify-argv vsig)]
      `(defn ~name
         ~@(map (fn [sig] `(~sig (~(arify-name mname (count sig)) ~@sig)))
                fixed-sigs)
         ~@(when variadic?
             [`(~vsig-argv (~(arify-name mname (count vsig)) ~@vsig))]))))

  (defn protocol-initialisation-form [spec]
    `(do ~(protocol-declaration-form spec)
         ~(protocol-extension-form spec)))

  (defn declaration-form [spec]
    `(do ~(registering-form spec)
         ~(protocol-declaration-form spec)
         ~(function-definition-form spec)
         ~(protocol-extension-form spec)
         ~(:name spec)))

  (defn extension-form [spec]
    (let [spec+ (extend-spec (get-spec! (:name spec)) spec)]
      `(do ~(registering-form spec+)
           ~(protocol-extension-form spec+))))

  )

(do :refresh

    (defn implementers
      "given a generic spec,
       returns a vector of all types that implement the corresponding generic"
      [spec]
      (->> (:cases spec)
           (map (fn [[_ & xs]]
                  (reduce (fn [a e]
                            (if (set? e) (into a e) (conj a e)))
                          #{} (map first (partition 2 xs)))))
           (reduce into #{})))

    (defn implementers-map []
      ($vals (get-reg) implementers))

    (defn sync-spec!
      "recompile and execute the spec of the given name"
      [name]
      (println "refreshing generics: " name)
      (eval (extension-form (get-spec! name))))

    (defn sync-types!
      "when type registry has been updated,
       we sometimes need to sync some generics declaration
       xs: the types that have changed
       only the generics impacted by this change will be synced"
      [xs]
      (let [sync? (partial clojure.set/intersection (set xs))]
        (doseq [[name ts] (implementers-map)]
          (when (sync? ts) (sync-spec! name)))))

    #_(sync-types! [:num :str])

    )

(do :api

    ;; user API, see concrete usage in tests below

    (defmacro generic
      "create a generic function"
      [name & cases]
      (declaration-form
       (generic-spec name cases)))

    (defmacro generic+
      "add new cases to an existant generic
       all given arities must already be known"
      [name & cases]
      (extension-form
       (generic-spec name cases)))

    (defmacro fork
      "create a new generic from an existing one
       does not alter the original"
      [parent-name name & cases]
      (let [names (derive-name name)
            parent-spec (get-spec! parent-name)
            base-spec (merge parent-spec names)
            extension-spec (generic-spec name cases)
            spec (extend-spec base-spec extension-spec)]
        (declaration-form spec)))

    (defmacro compile-all! []
      `(do ~@(map protocol-extension-form (vals (get-reg)))))

    ;; extend-type

    (defn impl-body->cases
      [tag [x1 & xs :as all]]
      (let [bodify (fn [[b1 & bs]]
                     (if-not bs b1
                             (list* 'do b1 bs)))]
        (if (vector? x1)
          [(list x1 tag (bodify xs))]
          (map (fn [[argv & bs]]
                 (list argv tag (bodify bs)))
               all))))

    #_(defn implement [tag [name & body]]
      `(generic+ ~name ~@(impl-body->cases tag body)))

    (defn implement [tag [name & body :as form]]
      (if (get (get-reg) name)
        `(generic+ ~name ~@(impl-body->cases tag body))
        (if-let [p (-> (resolve name) meta :protocol)]
          `(extend-protocol ~(symbol p)
             ~@(mapcat (fn [t] [t form]) (t/classes tag))))))

    #_(defmacro type+
      "like extend type"
      [tag & impls]
      `(do ~@(map #(implement tag %) impls)))

    (defmacro type+
      "like extend type"
      [tag & impls]
      `(do ~@(map #(implement tag %) impls)))

    
    )

(do
  :tests

   #_(pp (@reg '+))
   (generic g1 [x]
            ;; prim type impl
            :vec :g1vec
            ;; this type is a group
            ;; under the hood it implements for all collections
            :coll [:g1coll x]
            ;; group litteral can be handy
            #{:key :sym} :key-or-sym)

   (p/asserts
    (g1 [])
    (g1 #{})
    (g1 '())
    (g1 'a)
    (g1 :a))

   ;; extension
   (generic+ g1 [x]
             ;; str impl
             :str [:str x]
             ;; if a last expresion is given it extends Object
             [:unknown x])

   (get-spec! 'g1)
   (implementers (get-spec! 'g1))
   (get-reg)

   (p/asserts
    (g1 "a")
    (g1 (atom {})))

   ;; poly arity exemple
   (generic g2
            ([x y]
             :vec [:g2vec x y]
             :coll [:g2coll x y]
             :num [:g2num x y]
             :any [:g2any x y])
            ([x y z]
             :coll [:coll x y z])
            ;; variadic arity
            ([x y z & more]
             [:variadic x y z more]))

   (p/asserts
    (= (g2 [] 1)
       [:g2vec [] 1])
    (= (g2 #{} 1)
       [:g2coll #{} 1])
    (= (g2 #{} 1 2)
       [:coll #{} 1 2])
    (= (g2 "me" 1 2 3 4)
       [:variadic "me" 1 2 '(3 4)])
    (= (g2 :iop 1 2 3 4)
       [:variadic :iop 1 2 '(3 4)]))

   #_(p/error "stop")

   ;; extension of an existing generic 
   (generic+ g2
             ([a b] :vec [:g2vec2 a b])
             ([a b c & ds] :str [:variadstr a b c ds]))

   (p/asserts
    (= (g2 [] 1)
       [:g2vec2 [] 1])
    (= (g2 "me" 1 2 3 4)
       [:variadstr "me" 1 2 '(3 4)]))

   ;; fork is creating a new generic from an existing one
   ;; it inherit all impls and extend/overide it with given implementations
   (fork g2 g2+
         ([a b] :seq [:g2+seq2 a b])
         ([a b c & ds] :str [:g2+variadstr a b c ds]))

   (p/asserts

    (= (g2 () 2) [:g2coll () 2])
    (= (g2+ () 2) [:g2+seq2 () 2])

    ;; original untouched
    (= (g2+ "me" 1 2 3 4)
       [:g2+variadstr "me" 1 2 '(3 4)])

    ;; original untouched
    (= (g2 "me" 1 2 3 4)
       [:variadstr "me" 1 2 '(3 4)])
    )

   (fork g2+ g2+clone)


   ;; type+ is like extendtype
   ;; implement several generics at a time for a given type
   (type+ :fun
          (g1 [x] :g1fun)
          (g2 [x y] [:g2fun2 x y]))

   (p/asserts
    (= [:g2fun2 inc 1] (g2 inc 1))
    (= :g1fun (g1 (fn [a]))))

   ;; the implementations given to type+ does not have to be asparagus generics,
   ;; it can be regular clojure protocols functions
   ;; CAUTION: it will not reflect type hierarchy further changes as with generics

   (defprotocol Prot1 (prot1-f [x] [x y]))

   (type+ :fun
          (g1 [x] :g1fun)
          (g2 [x y] [:g2fun2 x y])
          ;; a raw protocol function
          (prot1-f ([x] "prot1-f fun")
                   ([x y] "prot1-f fun arity 2"))) ;; <- here

   ;; if childs are added to :fun, prot1-f will not be sync! so, use at your own risk...

   (p/asserts (= "prot1-f fun" (prot1-f inc))
              (= "prot1-f fun arity 2" (prot1-f inc 42)))

   (generic sip'
            ([a b]
             :vec (conj a b)
             :map (apply assoc a b)
             :set (conj a b)
             :seq (concat a [b])
             :str (str a (.toString b))
             :sym (symbol (sip' (name a) (.toString b)))
             :key (keyword (sip' (name a) (.toString b))))
            ([a b & xs]
             (apply sip' (sip' a b) xs)))

   (p/asserts
    (= (sip' [] 1 2 3)
       [1 2 3])
    (= (sip' #{} 1 2 3)
       #{1 2 3}))

   (generic valid'
            [x]
            :nil nil
            :map (when (every? valid' (vals x)) x)
            :coll (when (every? valid' x) x)
            :word :validword
            :any x)

   (p/asserts
    (not (valid' [nil 1 nil]))
    (valid' [1 2 3])
    (valid' #{1 2 3})
    (valid' {:a 1 :b 2})
    (not (valid' {:a 1 :b 2 :c nil})))

   #_(clojure.walk/macroexpand-all '(generic+ valid'
                                              [x] :key :validkey))

   (generic+ valid'
             [x] :key :validkey)

   (get-spec! 'valid')

   (p/asserts
    (= :validkey (valid' :a))
    (= :validword (valid' 'a)))


   )

#_(do :xp

    ;; build a map :: type -> fn-form
    ;; for a given generic (for each defined type)

    ;; the goal is to provide a way to use specific implementations manually in code, for performance/precision

    ;; the impl is not so pretty but it works
    ;; for instance if my-generic has an implementation for :vec
    ;; in some cases we could want to call my-generic-vec or my-generic_vec directly for performance/precision

    (defn tfnmap_t->expr [xs]
      (p/redh
       (fn [a [t e]]
         (let [ts (if (= :any t)
                    (t/all-types)
                    (conj (t/childs t) t))]
           (merge a (zipmap ts (repeat e)))))
       (reverse (partition 2 xs))))

    (defn tfnmap_t->aritymap [cases]
      (p/redh
       (fn [a [argv & decls]]
         (let [arity (count argv)]
           (reduce
            (fn [a [c e]]
              (update a c assoc arity (list argv e)))
            a (tfnmap_t->expr decls))))
       cases))

    (defn tfnmap
      [{:as spec
        :keys [variadic? cases]}]
      ($vals (tfnmap_t->aritymap cases)
             (fn [aritymap]
               (if variadic?
                 (let [[[vargv vexpr] & fixed-arities]
                       (map second (sort-by key > aritymap))]
                   (cons (list (variadify-argv vargv) vexpr)
                         fixed-arities))
                 (vals aritymap)))
             ))

    #_(tfnmap (@reg '$) 'mylamb)

    (defmacro defsubcases [nam]
      `(do
         ~@(map
            (fn [[t fnbody]]
              `(defn ~(sym nam '_ (name t)) ~@fnbody))
            (tfnmap (@reg nam)))))

                                        ; tada !
    (p/mx' (defsubcases g2)))

