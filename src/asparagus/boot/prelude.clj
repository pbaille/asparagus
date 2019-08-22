(ns asparagus.boot.prelude
  (:refer-clojure
   :exclude [assert not-empty empty or cat])
  (:require
   [clojure.core :as c]
   [clojure.string :as str]
   [clojure.set :as set]))

(def debug (atom nil))

(do :aliases

    "some really commonly used functions, with shorter names"

    (def p partial)
    (def apl apply)
    (def p* (p p apl))
    (def id identity)
    (def sym? symbol?)
    (def kw? keyword?)
    (def car first)
    (def cdr rest)
    (def lst list)
    (def vec? vector?)
    (def k constantly)
    (def hm* (p* hash-map))
    (def cat concat)
    (def cat* (p* cat))
    (def catv (comp vec concat))
    (def catv* (p* catv))
    (def mrg merge)
    (def str* (p* str)))

(do :misc

    (defn call* [xs]
      #_(pp xs)
      (apl (first xs) (rest xs)))

    (defn word? [x]
      (c/or (sym? x) (kw? x)))

    (defn holymap? [x]
      (and (not (record? x))
           (map? x)))

    (defn holycoll? [x]
      (c/or (seq? x) (vec? x)
            (set? x) (holymap? x)))

    (defn gensyms []
      (repeatedly gensym))

    (defn argv-litt [n & [prefix]]
      (vec (repeatedly n #(gensym (c/or prefix "a_")))))

    (defmacro _ [& _] nil)

    (defmacro cp [x & xs]
      `(condp #(%1 %2) ~x ~@xs))

    (_ :cp+

        (defmacro cp' [x & xs]
          (let [[cases default]
                (if (odd? (count xs))
                  ((juxt butlast last) xs)
                  [xs nil])
                cases
                (mapcat
                 (fn [[p e]]
                   (cond (vec? p) [`(fn [x#] (every? #(% x#) ~p)) e]
                         (set? p) (recur [(vec p) e])
                         (holymap? p) (recur [(vec (vals p)) e])
                         :else [p e]))
                 (partition 2 cases))]
            `(condp #(%1 %2) ~x ~@cases ~default))
          )

        (mx' (cp' "s"
                  [number? pos?] :yeah
                  string? "iop"
                  :nop))
        )

    (declare pretty-str)
    (defmacro assert [x & [m]]
      (let [s (gensym)
            v (gensym)
            m `(str ~(c/or m "assert fails:") "\n" '~x)]
        `(if-let [~s ~x ;(try ~x (catch Exception e# (println  "error during assertion:\n" (pretty-str '~x e#))))
                  ] ~s
                 (throw (new AssertionError
                             ~m #_(str ~(c/or m "assert fails:") "\n" '~x))))))

    #_(macroexpand (assert (= 0 (/ 2 0)) "by zero"))
    #_(assert (= 0 (/ 1 1)) "by zero")

    (defmacro asserts [& xs]
      `(do ~@(map (fn [x] `(assert ~x)) xs)))

    (defmacro when! [x & body]
      `(do (assert ~x)
           ~@body))

    (defmacro when-let! [[pat expr] & body]
      `(when-let [~pat (assert ~expr (str "when-let! failure:"))]
         ~@body))

    (defmacro let! [bs & xs]
      `(let ~bs (asserts ~@xs) ~(last xs)))

    (defmacro error [& xs]
      `(throw (Exception. (str ~@xs))))

    (defmacro import-macros [x y & nxt]
      `(do (def ~x (var ~y))
           (.setMacro (var ~x))
           ~(when nxt `(import-macros ~@nxt))))

    (defmacro import-fns [x y & nxt]
      `(do (defn ~x [& xs#] (apply ~y xs#))
           ~(when nxt `(import-fns ~@nxt)))))

(do :template

    (defn quote? [x]
      (and (seq? x) (= (first x) 'quote)))

    (defn unquote? [form]
      (and (seq? form)
           (= (car form)
              'clojure.core/unquote)))

    (defn unquote-splicing? [form]
      (and (seq? form)
           (= (car form)
              'clojure.core/unquote-splicing)))

    (defn quotef
      "@bbloom/backtic, simplified version"
      [form]
      (cp form
          unquote? (second form)
          unquote-splicing? (error "splice not in list")
          holycoll?
          (let [xs (if (map? form) (cat* form) form)
                parts (for [x xs]
                        (if (unquote-splicing? x)
                          (second x)
                          [(quotef x)]))
                cat (doall `(concat ~@parts))]
            (cp form
                vec? `(vec ~cat)
                map? `(apply hash-map ~cat)
                set? `(set ~cat)
                seq? `(apply list ~cat)
                (error "Unknown collection type")))
          `'~form))

    (defmacro sq
      ([x] (quotef x))
      ([x & xs] (quotef (cons x xs))))

    (_ :tries
       (mx' (sq (a b ~(+ 1 2) (sq (c b ~'~(+ 1 2 3))))))

       (let [name1 '(range 3)
             name2 'y]
         (quotef (list 'a (sq (b ~@~name1 ~'~name2 d)) 'e)))

       (quotef '(+ a b c ~(+ 1 2) ~@(range 10))))

    )

(do :$

    (defn redh [f xs]
      (reduce f {} xs))

    (def remnil
      (partial remove nil?))

    (defn mentry? [x]
      (instance? clojure.lang.MapEntry x))

    (defn empty [x]
      (cp x
          record? (apply dissoc x (keys x))
          mentry? []
          (c/empty x)))

    (defn $fn [ffn]
      (fn [x f]
        (if (seq? x)
          (ffn f x)
          (into (empty x) (ffn f x)))))

    (def shrink+ ($fn filter))
    (def shrink- ($fn remove))
    (def $! ($fn keep))
    (def $ ($fn map))

    (defn $vals [x f]
      ($ x (fn [[k v]][k (f v)])))

    (defn $keys [x f]
      ($ x (fn [[k v]][(f k) v])))

    (defn all? [x f]
      (when (= (count x)
               (count (filter f x)))
        (c/or x true)))

    )

(do :colls

    (def lst* list*)

    (defn set* [& xs] (into (set (butlast xs)) (last xs)))
    (defn set+ [& xs] (reduce into #{} xs))

    (defn vec* [& xs] (catv (butlast xs) (last xs)))
    (defn vec+ [& xs] (catv* xs))

    (def set+* (p* set+))
    (def vec+* (p* vec+))

    (_ :tests
       (set* 1 2 3 (range 34 36))
       (set+ (range 12) [45 6])
       (set+* [90 8] [(range 12) [45 6]]))

    (defn set- [& xs]
      (reduce set/difference ($ xs set)))

    (def uncs (juxt first rest))
    (def runcs (juxt butlast last))

    (defn flagmap
      ([] {})
      ([x]
       (cond
         (c/or (seq? x)(vec? x)(set? x))
         (zipmap x (repeat true))
         (nil? x) {}
         :else {x true}))
      ([x & xs] (flagmap (cons x xs))))

    (assert
     (= (flagmap :a :b)
        (flagmap (keys {:a 1 :b 2}))
        (flagmap #{:a :b})
        (flagmap (lst :a :b))
        (flagmap [:a :b])))

    (def kset
      (comp set keys))

    (defn member? [xs e]
      (contains? (set xs) e))

    (defn indexof [xs e]
      (and (member? xs e)
           (loop [i 0 [x & xs] xs]
             (if (= x e)
               i (recur (inc i) xs)))))

    (defn gat [xs i]
      (if (>= i 0)
        (cp xs
            vec? (get xs i)
            seq? (nth xs i nil)
            nil)
        (gat (reverse xs) (- i))))

    (defn not-empty [x]
      (when-not (empty? x) x))

    (defn butlasts
      [s]
      (if (seq s)
        (cons s (butlasts (butlast s)))
        '(())))

    (defn tails
      [s]
      (if (seq s)
        (cons s (tails (cdr s)))
        '(())))

    (defn gat [xs i]
     (if (>= i 0)
      (cond
       (vector? xs) (get xs i)
       (seq? xs) (first (drop i xs)))
      (gat (reverse xs) (- (inc i)))))

    #_(defn deep-merge
      "Like merge, but merges maps recursively."
      [& xs]
      (cond
        (every? #(or (holymap? %) (nil? %)) xs)
        (apply merge-with deep-merge xs)

        (every? #(or (set? %) (nil? %)) xs)
        (reduce into #{} xs)

        :else
        (last xs)))

    (defn deep-merge
      ([x y]
       (cond
         (not x) y
         (not y) x

         (every? holymap? [x y])
         (merge-with deep-merge x y)

         (every? set? [x y])
         (into x y)

         :else y))
      ([x y & ys]
       (reduce deep-merge
               x (cons y ys))))

    (defn findeep [x p]
      (cp x
          p (list x)
          coll? (mapcat #(findeep % p) x)
          ())))

(do :symbols

    (defn dot? [x]
      (= '. x))

    (defn double-dot? [x]
      (= '.. x))

    (defn fullname [x]
      (if (string? x) x
          (if-let [ns (namespace x)]
            (str ns "/" (name x))
            (name x))))

    (defn qualify-sym [x]
      (symbol (str *ns*) (name x)))

    (defn sym [& xs]
      (->> xs
           (remove nil?)
           (map fullname)
           (apply str)
           symbol))

    (asserts
     (= (symbol "") (sym))
     (= 'foobar
        (sym 'foo 'bar)
        (sym :foo "ba" 'r)
        (sym 'foo :b "ar" (sym))))

    (defn sym-split [x]
      (-> (str x)
          (clojure.string/split #"\.")
          (->> (filter seq)
               (mapv sym))))

    (asserts
     (= '[a b /] (sym-split (symbol "a.b./")))
     (= [] (sym-split (symbol "")))
     (= '[a z] (sym-split 'a.z)))

    (def sym0 (symbol ""))
    (def sym0? (p = sym0))

    (do :ns-resolution

        (defn- class-symbol [^java.lang.Class cls]
          (symbol (.getName cls)))

        (defn- namespace-name [^clojure.lang.Namespace ns]
          (name (.getName ns)))

        (defn- var-namespace [^clojure.lang.Var v]
          (name (.name (.ns v))))

        (defn- var-name [^clojure.lang.Var v]
          (name (.sym v)))

        (defn- var-symbol [^clojure.lang.Var v]
          (symbol (var-namespace v) (var-name v)))

        (defn ns-resolve-sym [sym]
          (try
            (let [x (ns-resolve *ns* sym)]
              (cond
                (instance? java.lang.Class x) (class-symbol x)
                (instance? clojure.lang.Var x) (var-symbol x)
                :else nil))
            (catch ClassNotFoundException _
              sym))))

    (_ 

     (defn dotsym
       ([] sym0)
       ([x]
        (cp x
            sym? x
            seq (apl sym (interpose '. (shrink- ($ x dotsym) sym0?)))
            sym0))
       ([x & xs]
        (dotsym (cons x xs))))

     (asserts

      (= sym0 (dotsym [] [] sym0))
      (= 'a (dotsym 'a))
      (= 'a/b (dotsym 'a/b))
      (= 'a.b (dotsym 'a 'b))
      (= 'a.b (dotsym '[a b]))
      (= 'a.b.c (dotsym '[a b] 'c))
      (= 'a.b.c.d.e.f.g.h (dotsym '[a b] 'c '[d e [f [g h]]]))
      (= (sym "a./") (dotsym (sym "a./")) (dotsym 'a '/))
      )

     (defn dotsplit [& xs]
       (sym-split (apply dotsym xs)))

     (asserts
      (dotsplit (sym "a.b./"))
      (= '[a b c]
         (dotsplit 'a.b.c)
         (dotsplit 'a 'b 'c)
         (dotsplit 'a '[b c] '[])))))

(do :parsing

    (defn parse-fn [[fst & nxt :as all]]

      (let [[name fst & nxt]
            (if (symbol? fst)
              (cons fst nxt)
              (concat [nil fst] nxt))

            [doc fst & nxt]
            (if (string? fst)
              (cons fst nxt)
              (concat [nil fst] nxt))

            [opts fst & nxt]
            (if (map? fst)
              (cons fst nxt)
              (concat [{} fst] nxt))

            impls
            (if (vector? fst)
              {fst (vec nxt)}
              (into {}
                    (map
                     (c/fn [[args & body]]
                       [args (vec body)])
                     (cons fst nxt))))

            ]

        (assoc opts
               :name name
               :doc doc
               :impls impls))))

(do :expand
    (def mx macroexpand)
    (def mx1 macroexpand-1)
    (def mx* clojure.walk/macroexpand-all)
    (defmacro mx' [x] `(mx '~x))
    (defmacro mx1' [x] `(mx1 '~x))
    (defmacro mx*' [x] `(mx* '~x)))

(do :shadowing

    (let [h (fn me [x]
              (cp x
                  sym? [x]
                  coll? (mapcat me x)
                  []))]

      (def all-syms (comp set h)))

    (defn shadows
      "give a binding form as the one that fn use for its args
           it return a set of shadowed syms"
      [binding-form]
      (->> (destructure [binding-form '_])
           (take-nth 2) set
           (clojure.set/intersection (all-syms binding-form))))

    )

(do :print

    (defn pp [& xs] (mapv clojure.pprint/pprint xs) nil)
    (defn pretty-str [& xs] (with-out-str (apply pp xs)))
    (defn prob [& xs] (mapv println xs) (last xs))
    (defn pprob [& xs] (mapv pp xs) (last xs))
    (defn dbg [& xs] (when @debug (apply println xs)))

    (defmethod clojure.pprint/simple-dispatch clojure.lang.AFunction [x]
      (clojure.pprint/simple-dispatch 'λ)))

(do :macros

    (defmacro let-dbg [bs & bod]
      `(let ~(vec (mapcat (fn [[p e]] [p `(prob '~p ~e)]) (partition 2 bs)))
         (println "\n\n")
         ~@bod))

    (do :or

        (defn deep-truth [x]
          (if (coll? x)
            (all? x deep-truth)
            x))

        (asserts
         (deep-truth [1 2 [3 5 {:a 1 :b 2}]])
         (not (deep-truth [1 2 [3 5 {:a nil :b 2}]])))

        #_(defmacro or
          ([x] x)
          ([x & xs]
           `(c/or
             ~(cp x
                  seq? x
                  coll? `(deep-truth ~x)
                  x)
             (or ~@xs))))

        (defmacro or
          ([& xs]
           `(c/or
             ~@(map #(cp % seq? %
                         holycoll? `(deep-truth ~%)
                         %) xs)
             nil)))

        (asserts
         (= 1 (or 1 2 3))
         (= true (or (not nil) 2 3))
         (= 2 (or {:a 1 :b nil} 2 3))
         (= :yo (or [1 nil] :yo (do (println 45) 1)))
         (nil? (or (pos? -1) false))
         (= :yo
            (or [1 nil]
                [1 2 [3 5 {:a nil :b 2}]]
                :yo)))

        

        )

    (do :cs

        (defn generated-binding-sym? [x]
          (re-matches #"^((vec)|(seq)|(first)|(map))__[0-9]+$"
                      (name x)))

        (asserts
         (nil? (generated-binding-sym? 'aze))
         (nil? (generated-binding-sym? (gensym "yop")))
         (generated-binding-sym? 'vec__1234)
         (generated-binding-sym? 'seq__1234)
         (generated-binding-sym? 'map__1234)
         (generated-binding-sym? 'first__1234))

        (defn cs-case
          [[b1 b2 & bs] e]
          `(~(if (or (generated-binding-sym? b1)
                     (= \_ (first (name b1))))
               `let `when-let)
            [~b1 ~b2]
            ~(if bs (cs-case bs e)
                 ;; this wrapping is nescessary for the case e eval to nil
                 [e])))

        (defn cs-form [[x e & xs]]
          (let [bs (if (vector? x) x [(gensym) x])
                form (cs-case (destructure bs) e)]
            (cond
              (not (seq xs)) form
              (not (next xs)) `(c/or ~form [~(first xs)]) ;; same thing here
              :else `(c/or ~form ~(cs-form xs)))))

        (defmacro cs [& xs]
          `(first ~(cs-form xs)))

        (_ :flat-cs-emitted-or-form

            (defn or-expr? [x]
              (and (seq? x) (= `c/or (first x))))

            (defn remove-useless-ors [x]
              (cp x
                  or-expr?
                  (cons `c/or
                        (mapcat (fn [y]
                                  (mapv remove-useless-ors
                                        (if (or-expr? y) (rest y) [y])))
                                (rest x)))
                  holycoll?
                  ($ x remove-useless-ors)
                  x))

            (defmacro cs [& xs]
              `(first ~(remove-useless-ors (cs-form xs))))

            (_ (let [a 0] ;; feel free to change the value and reevaluate
                 (macroexpand '(cs
                                (pos? a) :pos
                                (neg? a) :neg
                                :zero)))))

        (_ :cs-tuto

            ;; like a normal let
            (cs [a 1 b 2] (+ a b))

            ;; but shorts on nil bindings
            (cs [a (pos? -1) ;; this line binds 'a to nil,
                 ;; this will shortcircuit the rest of the binding form
                 ;; and jump to the second expression of the body
                 _ (println "never printed")]

                ;; evaluated only in case of successful bindings
                (println "never evaluated")

                ;; evaluated when binding form has been shortcircuited
                (do (println "failure branch taken")
                    :pouet))

            ;; you can bind symbols starting with an underscore to nil without failing
            (cs [a 1 b 2
                 _neg-a (neg? a) ;; this bind _neg-a to nil without shortcirsuiting
                 a (if _neg-a (- a) a)]
                (+ a b)) ;;=> 3

            ;; you can chain several couples of binding-form expression
            (defn cs_1 [a]
              ;; the ? symbol has no special meaning here
              (cs [? (number? a)] {:number a}
                  [? (string? a)] {:string a}
                  [? (coll? a)]
                  (cs [? (empty? a)] :empty
                      [? (seq? a)] {:seq a}
                      {:coll a})))

            (cs_1 1)
            (cs_1 "a")
            (cs_1 ())
            (cs_1 '(1 2))
            (cs_1 [1 2])

            ;; cs_1 works as intended but clearly can be done more concisely with a good old cond
            ;; but wait, cs macro can also be used like cond!

            ;; if you need only to check something but do not need the return value
            ;; like we seen in cs_1,  e.g [? (test? ...)]
            ;; it seems kind of tiring to do so, so we've introduce a shorthand for this case

            (let [a -42]

              (=
               ;; normal syntax
               (cs [? (pos? a)] a :negative)
               ;; shorthand syntax (condishpatible)
               (cs (pos? a) a :negative)))

            ;; as we see it can be use like 'if
            (cs (pos? -1) :pos :not-pos)

            ;; or when (with only one expression body)
            (cs (pos? 1) :pos)

            ;; or cond (without the need for the :else thing)
            (let [a 0] ;; feel free to change the value and reevaluate
              (cs
               (pos? a) :pos
               (neg? a) :neg
               :zero))

            ;; this kind of unification of if and cond came from arc-lisp,
            ;; i cannot find a solid argument against it

            ;; we can redefine cs_1 in a more clean way
            (defn cs_2 [a]
              (cs (number? a) {:number a}
                  (string? a) {:string a}
                  (coll? a)
                  (cs (empty? a) :empty
                      (seq? a) {:seq a}
                      {:coll a})))

            ;; the thing is that now you can mix condish syntax and condletish syntax

            (defn cs_3 [a]
              (cs (number? a) [:num a]
                  (symbol? a) [:sym a]
                  (string? a) [:str a]
                  [? (sequential? a)
                   sa (seq a)]
                  (into
                   [(cs (vector? a) :vec
                        (list? a) :lst
                        :seq)]
                   (map cs_3 sa))
                  [(type a) a]))

            (cs_3 [1 2 "aze" 'rt '(42 :iop a) {:a 1}])

            )

        #_(destructure '[[x y z & xs] y])
        #_(mx*' (cs [[x & xs] (range 1)] [x xs] :nop))
        )

    (_ :nsub

       (defmacro nsub [name & body]
         (let [parent-sym (symbol (str *ns*))
               fullname (sym parent-sym '. name)
               [ns-body decls] (split-with (comp keyword? first) body)]
           `(do (ns ~fullname ~@ns-body)
                (~'use '~parent-sym)
                ~@decls
                (in-ns '~parent-sym)
                (require ['~fullname :as '~name]))))

       (mx' (nsub aze
                  (:require [mud.boot.curried :as c])
                  (:use mud.boot.data)
                  (c/defcurf add [x y] (+ x y))))

       (aze/add 1)

       (pp *ns*)))

(do :xp

    (defmacro use!
      "the purpose of this is to be able to 'use' this ns without do the :refer-clojure :exclude boilerplate
       but this does not works :)"
      []
      `(~'ns ~(symbol (str *ns*))
        (:refer-clojure :exclude ~'[assert not-empty empty or cat])
        (:use mad.boot.prelude))))
