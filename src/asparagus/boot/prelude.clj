(ns asparagus.boot.prelude
  (:refer-clojure
   :exclude [assert not-empty empty or cat])
  (:require
   [clojure.core :as c]
   [clojure.string :as str]
   [clojure.set :as set]))

(def debug (atom nil))

(defmacro use! []
  `(~'ns ~(symbol (str *ns*))
     (:refer-clojure :exclude ~'[assert not-empty empty or cat])
     (:use mad.boot.prelude)))

(do :aliases
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
      (vec (repeatedly n #(gensym (c/or prefix "a")))))

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
            m (str (c/or m "assert fails:") "\n" (seq x))]
        `(if-let [~s ~x ;(try ~x (catch Exception e# (println  "error during assertion:\n" (pretty-str '~x e#))))
                  ] ~s
                 (throw (new AssertionError
                             ~m #_(str ~(c/or m "assert fails:") "\n" '~x))))))

    #_(assert (= 0 (/ 2 0)))
    #_(assert (= 0 (/ 0 1)))

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
      ([x]
       (if (c/or (seq? x)(vec? x)(set? x))
         (zipmap x (repeat true))
         {x true}))
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

      (let [default-name (gensym "fn_")
            [name fst & nxt]
            (if (symbol? fst)
              (cons fst nxt)
              (concat [default-name fst] nxt))

            [doc fst & nxt]
            (if (string? fst)
              (cons fst nxt)
              (concat ["" fst] nxt))

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

            name-key
            (if (= default-name name)
              :default-name
              :name)]

        (assoc opts
               name-key name
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

        (defmacro or
          ([x] x)
          ([x & xs]
           `(c/or
             ~(cp x
                  seq? x
                  coll? `(deep-truth ~x)
                  x)
             (or ~@xs))))

        (asserts
         (= 1 (or 1 2 3))
         (= true (or (not nil) 2 3))
         (= 2 (or {:a 1 :b nil} 2 3))
         (= :yo (or [1 nil] :yo (do (println 45) 1)))
         (= :yo
            (or [1 nil]
                [1 2 [3 5 {:a nil :b 2}]]
                :yo))))

    (do :cs

        (defn cs-case
          [[b1 b2 & bs] e]
          `(~(if (= \_ (first (name b1))) `let `when-let)
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
          `(first ~(cs-form xs))))

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
