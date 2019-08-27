(ns asparagus.core
  (:refer-clojure
   :exclude [eval resolve])
  (:require
   [clojure.core :as c]
   [clojure.string :as str]
   [clojure.set :as set]
   [criterium.core :as cr]
   [asparagus.boot.types :as t]
   [asparagus.boot.generics :as g]
   [asparagus.boot.state :as state]
   [asparagus.boot.prelude :as p
    :refer
    [;; macros
     cs cp pp _ error asserts
     ;; mapping
     $ $vals $keys shrink+ shrink-
     ;; base
     car cdr apl p p* id k indexof gat
     ;; preds
     vec? word? sym? dot? holycoll? holymap?
     quote? unquote? unquote-splicing?
     ;; build
     str* pretty-str sym
     lst lst* catv cat*
     mrg deep-merge flagmap
     ;; symbols
     gensyms ns-resolve-sym shadows
     ;;misc
     call*]]))

;; there is some uncommon code practice in this file that may surprise the reader
;; I'm heavily using 'do blocks to group related peaces of functionality, which i usually start with a descriptive keyword
;; it has no incidence on execution (as far as i know) but let you fold code in a meaningful and convenient way
;; this practice has led me to increase the average size of my namespaces (I don't really like to constantly jump to other files...)
;; so for a better experience, the reader should use an editor that can fold groups easily and possibly gradually

;; I also heavily use two macros that i like a lot,
;; cs: is like a fusion of if when cond cond-let (see the little tutorial at asparagus.boot.prelude:591)
;; cp: a shortcut for (condp #(%1 %2) ...)

;; I also use some collection type preserving versions of map and filter e.g: $, $keys, $vals, shrink+, shrink-
;; all those really basic building blocks that i tend to use everywhere are defined in asparagus.boot.prelude

;; keep in mind that two other important namespaces on top of which asparagus is built are asparagus.boot.types and asparagus.boot.generics
;; they contain examples and documentation so feel free to study them to

;;dev
;; ------------------------------------------------------------------------------

;; a convenience that will reset the asparagus global environment and do some cleaning when the file is reloaded
(when-let [! (c/resolve 'rEset!)] (!))

;;env
;; ------------------------------------------------------------------------------

(do :path

    ;; the Path abstraction will serve to represent a position in an environment
    ;; it consist of three parts
    ;; rel: nil for an absolute path | an integer indicating parent level (0 for current location, 1 for direct parent, 2 for grandparent etc...)
    ;; xs: a sequence of symbols representing a nested path in the environment (exemple: '[foo bar baz])
    ;; mkey: (short for meta-key) for instance :doc or :val (more on this later)

    ;; this will not be used directly, for building paths, we will use further defined functions (see build section)
    ;; typically, we will use the 'path function:

    ;; simple path
    ;; (path 'foo) <=> (Path. nil ['foo] nil)

    ;; we use dots to indicate nested path
    ;; (path 'foo.bar) <=> (Path. nil ['foo 'bar] nil)

    ;; a path can be prepended with any number of dots indicating its relative position
    ;; in this case, 2 dots indicates the parent level
    ;; (path '..foo.bar) <=> (Path. 1 ['foo 'bar] nil)
    ;; here the path is relative to the current position
    ;; (path '.foo.bar) <=> (Path. 0 ['foo 'bar] nil)

    ;; a keyword can be appended to a path symbol to indicate its mkey (meta-key)
    ;; (path '.foo:val) <=> (Path. 0 ['foo] :val)

    ;; a path will be used in conjonction with a positioned environment
    ;; relative path will be resolved relativelly to the current position of the evaluating environment

    #_(defrecord Path [rel xs mkey])

    (t/type+ :path [rel xs mkey])

    (do :base

        (defn path? [x] (instance? Path x))
        (def path0 (Path. 0 [] nil))
        (def root-path (Path. nil [] nil)))

    (do :print

        (defn path->str
          [p]
          (str (cs [l (.rel p)] (str* (repeat (inc l) '.)))
               (clojure.string/join "." (.xs p))
               (cs [mk (.mkey p)] (str ":" (name mk)))))

        (defn path->sym
          [p]
          (symbol (path->str p)))

        (defmethod clojure.pprint/simple-dispatch
          Path [x]
          (.write *out* (path->str x)))

        (defmethod print-method
          Path [p w]
          (print-method (symbol (path->str p)) w)))

    (do :build

        (defn split-path-name
          [x] #_(pp "split path name " x)
          (when-not (namespace x)
            (re-find #"^(\.*)([^:]*)(.*)$" (str x))))

        (defn path-name?
          [x]
          (cs (p/word? x)
              (split-path-name x)))

        (defn name->path
          [x]
          (cs [[_ dots body mkey] (split-path-name x)
               _rel (when (seq dots) (dec (count dots)))
               xs (p/sym-split body)
               _mkey (when (seq mkey) (keyword (apply str (rest mkey))))]
              (Path. _rel xs _mkey)))

        (defn path+
          "add two paths together"
          ([{:as x xxs :xs xrel :rel}
            {:as y yxs :xs yrel :rel ymk :mkey}]
           (cs yrel
               (cs (>= (count xxs) yrel)
                   (Path. xrel (p/catv (drop-last yrel xxs) yxs) ymk)
                   [rel+ (- yrel (count xxs))]
                   (Path. (+ (or xrel 0) rel+) yxs ymk))
               (or y x)))
          ([x y & xs]
           (reduce path+ x (cons y xs))))

        (declare rpath)

        (defn path
          ([x]
           (cp x
               path? x
               path-name? (name->path x)
               nil))
          ([x & xs]
           (cs [p (path x)]
               (apl path+ p ($ xs rpath)))))

        (def path* (p* path))

        (defn path! [& xs]
          (or (path* xs)
              (throw (Exception. (str* "not pathable " xs)))))

        (defn rpath
          "make a path relative, accepts symbols as well as paths"
          [p]
          (cs [p (path p)]
              (update p :rel #(or % 0))))

        (defn apath
          "make a path absolute, accepts symbols as well as paths"
          [p]
          (cs [p (path p)]
              (assoc p :rel nil)))

        (defn ppath
          "make a path primary (without mkey), accepts symbols as well as paths"
          [p]
          (cs [p (path p)]
              (assoc p :mkey nil)))

        (asserts

         (path->str (name->path '..a.b.c:val))

         (path 'a.b.c)
         (path '..a.b.c)
         (path '..:val)
         (path 'a.b.c:val)
         (path 'a '.b.c)

         (= (path+ (name->path '..a.b.c)
                   (name->path '.d.e))

            (path '..a.b.c
                  '.d.e)

            (path '..a.b.c.d.e))

         (= (Path. 1 nil nil)
            (Path. 1 nil nil))

         (= (path 'e)
            (path 'a.b
                  '..c
                  '...e))

         (= (path 'a.b.d.e)
            (path 'a.b.c
                  '..d.e))

         (= (path '..d.e)
            (path (symbol "") '..d.e))

         (= (path '.. '..d.e)
            (path '...d.e))))

    (do :preds

        (defn pathable?
          [x]
          (or (path? x) (path-name? x)))

        (defn mkey=
          [x k]
          (cs (and (path? x)
                   (= k (.mkey x)))
              x))

        (defn rpath?
          "is x a relative path ?"
          [x] (cs (path? x) (.rel x)))
        (defn apath?
          "is x an absolute path ?"
          [x] (cs (path? x) (not (.rel x))))
        (defn mpath?
          "is x a meta path ? (leaf path, a path that has an mkey)"
          [x] (cs (path? x) (.mkey x)))
        (defn ppath?
          "is x a primary path ? (does not have mkey)"
          [x] (cs (path? x) (cs (not (.mkey x)) x)))
        (defn dotpath?
          "is x a dotpath ? (a relative path with empty xs and no mkey)"
          [x] (cs (rpath? x)
                  (and (not (mpath? x))
                       (empty? (.xs x)))))
        (defn pure-mpath? [p]
          (and (mpath? p)
               (not (rpath? p))
               (not (seq (.xs p)))))
        )

    (do :misc

        (defn path-segments [p]
          (catv (.xs p)
                (cs [mk (.mkey p)] [mk])))

        (defn path-head [p]
          (let [mk (.mkey p)
                ps (path-segments (ppath p))]
            (path (last ps) mk)))

        (defn path-prefix [p]
          (cs (apath? p)
              (car (.xs p))))

        (defn path-unprefix [p]
          (cs (apath? p)
              (update p :xs (comp vec rest))))

        (defn bubbling-path
          [p]
          #_(pp 'bubling-path p)
          (cs [xs (seq (.xs p))]
              (assoc p :xs (-> xs butlast vec))))

        (defn bubbling-paths
          [p]
          (cs [bp (bubbling-path p)]
              (cons bp (bubbling-paths bp))))

        (defn parent-path
          [p]
          (cs (mpath? p)
              (ppath p)
              (bubbling-path p)))

        (defn parent-paths
          [p]
          (cs [pp (parent-path p)]
              (cons pp (bubbling-paths pp))))

        (defn nth-parent-path [p n]
          (nth (cons p (parent-paths p)) n nil))

        (defn parent-path?
          [x y]
          ((set (parent-paths y)) x))

        (defn child-path?
          [x y]
          (parent-path? y x))

        (defn inner-paths
          "find all paths presents in a clojure collection"
          [x]
          (into #{} (p/findeep x path?)))

        (defn path-diff [p1 p2]
          (cs (= p1 p2) root-path
              (parent-path? p2 p1)
              (assoc p1 :xs
                     (-> p2 :xs count
                         (drop (:xs p1)) vec))))

        (asserts
         (parent-path (path 'a.b))
         (parent-paths (path '..a.b.c:val))
         (bubbling-paths (path '..a.b.c:val))
         (parent-path? (path 'a.b) (path 'a.b:val))
         (parent-path? (path 'a.b) (path 'a.b.c))
         (path-diff (path 'a.b.c) (path 'a))
         (nil? (parent-path? (path 'a.b) (path 'a.b)))
         (nil? (parent-path? (path 'a.b) (path 'a:val))))))

(do :env

    (def echeck? (atom true))

    ;; the Env record represent a positioned environment
    ;; at: a Path representing current position
    ;; members: a nested map holding environment bindings

    #_(defrecord Env [at members])

    (t/type+ :env [at members])

    (do :base

        (defn env?
          [x] (when (instance? Env x) x))

        (def env0
          (Env. root-path
                {:links {'_ root-path}}))

        (defmacro env!
          [x]
          `(let [x# ~x]
             (p/assert (env? x#)
                     (str "\n\nNot a t:\n\n"
                          (pretty-str x#)))
             x#)))

    (do :print

        (def env-print-mode (atom :mini))

        (defmethod clojure.pprint/simple-dispatch Env [e]
          (clojure.pprint/simple-dispatch
           (condp = @env-print-mode
                   :mini 'E
                   :members (into {} e)
                   :verbose (into {} e)))))

    (do :defne

        ;; a lambda that takes an env as first argument
        ;; and throw if @echeck? is true and first arg is not an Environment
        ;; maybe just a dev purpose device, if @echeck? is false it compile to regular clojure lambda

        (defn arg1-sym [[a1 & _]]
          (cp a1
              sym? a1
              map? (or (:as a1) (throw "tfn arg1 has no symbol! use :as"))))

        (defmacro fne [& form]
          (let [{:keys [impls name default-name] :as met} (p/parse-fn form)]
            `(fn ~(or name default-name)
               ~@(map (fn [[argv body]]
                        (list* argv (when @echeck? `(env! ~(arg1-sym argv))) body))
                      impls))))

        (defmacro defne [nam & xs]
          `(def ~nam (fne ~nam ~@xs))))

    (do :setters

        ;; position related setters and updates

        (defne root
          [x] (assoc x :at root-path))

        (defne cd
          [x & xs]
          (assoc x :at (path* xs)))

        (defne mv
          [x & xs]
          (update x :at #(path* % xs))))

    (do :getters

        ;; inspection and resolution

        (defne loc
          [e] (.at e))

        (defne root?
          [e] (= root-path (loc e)))

        ;; getting environment members

        ;; in each of those functions
        ;; e is an environment
        ;; p is a path

        (defne env-get
          "find what is at path p in environment e"
          [e p]
          (cs (apath? p)
              (get-in (.members e)
                      (path-segments p))))

        ;; the following find operations are inspired by core/find
        ;; given an env e and a path p
        ;; if it finds something at p in e, returns a tuple containing p and the found value
        ;; else return nil

        (defne env-absfind
          "absolute find"
          [e p]
          (cs [found (env-get e p)]
              [p found]))

        (defne env-relfind
          "relative find"
          [e p]
          (env-absfind e (path (loc e) p)))

        (defne linked-path
          "if the given path's prefix is linked "
          [e p]
          (cs [? (apath? p) ;; only absolute paths can be linked
               ;;xs (.xs p)
               ;;pref (car xs)
               ;;_xs (rest xs)
               [prefix & _xs] (.xs p)
               ;; we check if a link exists for this prefix
               target (get (env-get e (path (loc e) :links)) prefix)]
              ;; if yes we replace the prefix by it
              (path (path* target _xs) (.mkey p))))

        (defne env-linkedfind [e p]
          (cs [p' (linked-path e p)]
              (env-absfind e p')))

        (defne env-find [e p]
          (or (env-relfind e p)
              (env-linkedfind e p)))

        (defne bubfind
          "bubbling find, the main resolution function
           which is used by the compilation process"
          [e p]
          (cs

           ;; if path consists only of a dot, we ignore it
           (dotpath? p) nil

           #_(root-pathsym? p)
           #_(env-absfind e (path-unprefix p))

           ;; p is relative
           [plvl (.rel p)
            loc' (nth-parent-path (ppath (loc e)) plvl)]
           (env-find (cd e loc') (apath p))

           ;; p is absolute, potentially linked
           [ret (env-find e p)] ret

           ;; if nothing was found, we bubble up one level, if not already at root
           (not (root? e))
           (bubfind (mv e '..) p)))

        (defne bubget
          "behaves like bubfind, but returns only the found value
           more permissive that bubfind it accept symbols as well as paths"
          [e p]
          (cs (sym? p) (bubget e (path p))
              [[_ v] (bubfind e p)] v))

        (defne qualsym
          "try to qualify a symbol into its corresponding path,
           using same bubling strategy than bubfind"
          [e s]
          (cs [p (path s)]
              (cs (mpath? p)
                  (car (bubfind e p))
                  [[p v] (bubfind e p)]
                  (cp v
                      :sub (path p :sub)
                      :val (path p :val)
                      nil))))

        (_ :tries

           (def e1
              (Env.
               root-path
               #_{}
               '{:links {ab a.b}
                 a {:val :a
                    b {:val :ab
                       c {:val :abc
                          :sub :abc_sub}
                       d {:val :abd}}}
                 b {b1 {:val :b1}
                    b2 {:val :b2}
                    a {:val :ba}}}))

            (asserts
             ;; direct
             (bubfind e1 (path 'a))
             (bubfind e1 (path 'a:val))
             ;; linked
             (bubfind e1 (path 'ab.c))
             ;; relative
             (bubfind (cd e1 'foo) (path '..a.b))
             ;; relative linked
             (bubfind (cd e1 'foo) (path '..ab))
             ;; bubbling
             (bubfind (cd e1 'foo.bar) (path 'a))
             ;; bubling linked
             (bubfind (cd e1 'foo) (path 'ab))
             ;; bubling first found
             (bubfind (cd e1 'b.c.d) (path 'a:val))
             ;; root-pathsym
             #_(bubfind (cd e1 'b.c.d) (path '_.a:val))

             (not (bubfind e1 (path '.iop)))
             (not (bubfind (cd e1 'foo) (path '.a.b)))
             (not (bubfind (cd e1 'foo) (path '...a.b)))
             )

            (env-linkedfind e1 (path 'ab))
            (qualsym e1 (path 'ab.c))
            (qualsym (cd e1 'li) (path 'abc.c))
            (qualsym (cd e1 'foo) (path 'a))
            (qualsym (cd e1 'foo) (path '.a))
            (qualsym (cd e1 'foo) (path 'ab.c))
            (qualsym (cd e1 'foo) (path '.ab.c))
            (qualsym (cd e1 'c.d.e) (path 'ab.c))
            )

        )

    )

(do :state

    ;; the global envirnoment atom
    ;; will not be used directly by the user
    (def E (atom env0))

    ;; if this flag is on
    ;; all defined env members will be assigned to vars
    ;; resulting in much better performances (20x faster)
    (def varmode (atom true))

    ;; holds env members-symbols that can be used at ns level
    ;; this atom will be populated by main asparagus macros
    (def top-forms (atom #{}))

    (do :vars

        ;; the varmode business

        (def PATHVAR_PREFIX 'ENV_)

        (def PATHVAR_PATTERN
          (re-pattern (str ".*" (name PATHVAR_PREFIX) ".*")))

        (defn pathvar-sym? [x]
          (re-matches PATHVAR_PATTERN
                      (name x)))

        (defn path->varsym [p]
          (sym PATHVAR_PREFIX
               (str/join "_" (.xs p))
               '__ (or (.mkey p) 'val)))

        (defn init-pathvar! [p]
          (c/eval `(declare ~(path->varsym p))))

        (defn set-pathvar! [p v]
          (c/eval `(def ~(path->varsym p) ~v)))

        (defn deep-merge-pathvar! [p v]
          (c/eval `(alter-var-root (var ~(path->varsym p)) deep-merge ~v))))

    (do :clean

        ;; if we are using vars, we need to keep things clean

        (defn clean-pathvars! []
          (doseq [s (shrink+ (keys (ns-publics *ns*)) pathvar-sym?)]
            (ns-unmap *ns* s)))

        (defn clean-member-vars! [p]
          (let [prefix
                (-> (ppath p)
                    path->varsym str
                    (str/split #"__")
                    first)]
            (doseq [s (shrink+ (keys (ns-publics 'asparagus.core)) pathvar-sym?)]
              (when (re-find (re-pattern prefix) (str s))
                #_(println 'removing-member-var  s)
                (ns-unmap 'asparagus.core s)))))

        (defn clean-members-vars! [xs]
          (doseq [x xs]
           (clean-member-vars! (path x))))

        (defn clean-top-forms! []
          (doseq [s @top-forms]
            (ns-unmap *ns* s))
          (reset! top-forms #{}))

        (defn clean-Evars! []
          (clean-pathvars!)
          (clean-top-forms!))

        (defn rEset!
          "will clear the global env and clean all vars that have been defined by it"
          []
          (reset! E env0)
          (clean-Evars!)))

    (do :access

        (defn unbound-error [p]
          (throw (ex-info (str "Unbound path: " (path->str p))
                          {:type :unbound
                           :path p})))

        (defn unfound-error [p]
          (throw (ex-info (str "Unfound path: " (path->str p))
                          {:type :unfound
                           :path p})))

        (defn env-access
          "arity1 : getting the value at the given path p inside the global env
           arity 2 : getting the value at the given path p inside the given environment e"
          ([p] (env-access @E p))
          ([e p]
           (cs [found (env-get e p)]
               (cs (= ::unbound found)
                   (unbound-error p)
                   found)
               (unfound-error p))))

        (defn env-access?
          "is x an env-access form ?"
          [x]
          (and (seq? x)
               (or (indexof x '`env-access) ;; this handles quoted env-access forms
                   (indexof x `env-access))))

        ))

(do :steps

    ;; in asparagus, compilation occurs in 3 main steps
    ;; qualify : will replace all resolvable symbols by their concrete Path representation (using 'qualsym (see [:env :getters] section
    ;; expand : will execute compile time behaviors sush as macros and symbolic substitutions
    ;; resolve : will replace all Path with their corresponding vars

    (do :help

        (defn valpath? [x] (mkey= x :val))
        (defn subpath? [x] (mkey= x :sub))
        (defn macpath? [x] (mkey= x :mac))

        (defn mark-exp [x]
          (vary-meta x assoc :expansion true))

        (defn mcall? [x]
          (some-> x meta :expansion))

        (defmacro defexpansion [name [esym xsym] expr]
          `(defn ~name ~[esym xsym]
             (try ~expr
                  (catch Exception err#
                    (error "--- Expansion error ---\n\n"
                           (pretty-str ~xsym)
                           "\nctx:\n"
                           (str* ($ (:exp-ctx ~esym) pretty-str))
                           "\nerror:\n"
                           (.getMessage err#))))))

        (defexpansion expand-mcall [e x] ((env-access e (car x)) e (cdr x)))
        (defexpansion expand-subpath [e x] ((env-access e x) e))
        )

    (defne qualify

      "takes an environment e and an expression x
       it will turn all resolvable symbols into paths, using bubbling resolution
       (see bubfind and qualsym in the [:env :getters] section)
       macro calls will be handled differently, the verb will be qualified, the expression marked, and the arguments left as is"

      [e x]

      (cp x

          ;; for symbol, we try to qualify it or leave it as is
          sym?
          (or (qualsym e x) x)

          ;; fullquote we skip
          quote? x

          seq?
          (cs
           ;; if x is a macro call, we qualify the verb (the macro identifier)
           ;; and mark the expression as expansion
           [p (-> x car path)
            ? (or (ppath? p) (macpath? p))
            p (qualsym e (path p :mac))]
           (mark-exp (cons p (cdr x)))
           ;; else we just map qualify over the expression
           ($ x (p qualify e)))

          ;; for clojure's collection, we map qualify
          holycoll?
          ($ x (p qualify e))

          ;; else do nothing
          x))

    (defne expand

      "takes an environment e and an expression x
       will handle substitutions and macro calls"

      [e x]
      #_(pp 'expand x)
      (cp x
          ;; subsitution
          subpath? (expand-subpath e x)
          ;; macro call
          mcall? (expand-mcall e x)
          ;; clojure collection
          holycoll? ($ x (p expand e))
          ;; anything else
          x))

    (defne resolve

      "takes an environment e and an expression x
       in varmode it will replace paths by corresponding var symbols
       in normal mode (wich is far slower, and maybe will never be used) it replace paths by environment access expression"

      [e x]
      #_(pp 'resolve x (path? x))
      (cp x
          env-access? x ;(prob 'env-access x)
          path? (if @varmode (path->varsym x) `(env-access ~x))
          holycoll? ($ x (p resolve e))
          x))

    (defne exp

      "the composition of qualify and expand,
       in most case that's what you want to use (in macro implementations for instance)
       "

      [e x]
      #_(pp "exp " x)
      (let [;; this expansion context thing is an attempt to provide better error messages on expansion failure
            ;; used by expand-mcall and expand-subpath via defexpansion
            e (update e :exp-ctx (fnil conj []) x)]
        (->> x (qualify e) (expand e))))
<
    (defne res
      "the whole compilation process, qualify -> expand -> resolve"
      [e x]
      (->> x (qualify e) (expand e) (resolve e)))

    (defne eval
      "compile x in e and eval the resulting expression"
      [e x]
      (c/eval (res e x)))

    )

(do :extension

    ;; this section will define helper functions related to environment extension
    ;; those functions should not be used directly
    ;; but will be used by the next section (API)

    (do :transformations

        (defn env-member-path [p]
          (cons :members (path-segments p)))

        (defn env-meta-path [p]
          (cons :meta (path-segments p)))

        (defn env-add-member [e p x]
          (update-in e (env-member-path p) deep-merge x))

        (defn env-rem-member [e p]
          (let [segments (env-member-path p)]
            (update-in e (butlast segments) dissoc (last segments))))

        (defn env-merge-members [e xs]
          (reduce (p* env-add-member) e xs))

        (defn env-add-meta [e p x]
          (update-in e (env-member-path (ppath p))
                     vary-meta deep-merge {(.mkey p) x}))

        (defn env-merge-metas
          ([e xs]
           (reduce (p* env-add-meta) e xs))
          ([e at xs]
           (env-merge-metas
            e ($keys (eval (cd e at) xs)
                     (fn [p] (path at p))))))

        (defn env-relmerge-metas [e xs]
          (env-merge-metas e (loc e) xs))

        (defn env-declare-member [e p]
          (let [known? (c/resolve (path->varsym p))]
            (when @varmode (init-pathvar! p))
            (if (and (not known?) (mpath? p))
              (env-add-member e p ::unbound)
              e)))

        (defn env-detailed-steps

          "given an environment and an expr,
           perform the 3 asparagus compilation steps,
           and return a map holding all intermediate values,
           the expression and the environment position"

          [e expr]
          (let [at (loc e)
                qualified (qualify e expr)
                expanded (expand e qualified)
                ;expanded (exp e expr)
                resolved (resolve e expanded)]
            {:at at
             :expr expr
             :qualified qualified
             :expanded expanded
             :resolved resolved}))

        (defn env-member-add-compiled

          "add a member to the given environment e, at the given path p, compiling the given expression x,
           holding intermediate compilation steps in environment metadatas
           returns the updated environment

           if varmode is on, do the bad things ;)"

          [e p x]
          (let [{:as m r :resolved at :at}
                (env-detailed-steps (mv e p) x)
                evaluated (c/eval r)]
            ;; TODO this thing cannot be here...
            (when @varmode (deep-merge-pathvar! p r)
                  #_(set-pathvar! p r))
            (-> e
                (env-add-member at evaluated)
                (env-add-meta at m))))

        ;; removing members ---

        (defn env-clear-member! [p]
          (let [p (path p)]
            (when @varmode (clean-member-vars! p))
            (swap! E env-rem-member p)))

        (defn E-minus [at [x1 & rxs :as xs]]
          (when xs
            (concat
             (cp x1
                 symbol? [`(env-clear-member! (path '~(path->sym (path at x1))))]
                 holymap? (c/mapcat (fn [[k v]] (pp 'iop) (E-minus (path at k) [v])) x1)
                 vector? (c/mapcat (fn [x] (pp 'o x) (E-minus at [x])) x1))
             (E-minus at rxs))))

        (_
         #_(E-minus 'Env root-path '[iop foo])
         #_(E-minus 'Env root-path '[iop foo [bar baz] {hey {bob [a z e] bae {iop [i o p]}}}])
         #_(macroexpand '(E- iop foo))
         #_(macroexpand '(E- iop foo [bar baz] {hey {bob [a z e] bae {iop [i o p]}}}))))

    (do :updates

        ;; an asparagus update is a description of an environment mutation
        ;; those functions will be used by the main environment extension form: E+
        ;; i've written a gentle introduction to asparagus updates in ./tutorial.clj

        (defn env-upd_prepend-declarations [u]
          #_(pp 'will-prep-decl (doall u))
          (let [ps
                (-> (shrink+ u #(= :def (car %)))
                    ($ #(vector :declare (second %)))
                    (shrink- (set u)))]
            (doall (concat ps u))))

        (defn env-upd_sort [u]
          #_(pp 'will-sort-u u)
          (let [filtype
                (fn [t] (shrink+ u #(= t (car %))))]
            (doall (mapcat filtype [:link :declare :clj :fx :def]))))

        (defn env-upd_upd-expr? [e x]
          (cs [? (seq? x)
               p (path (car x) :upd)]
              (bubfind e (path (car x) :upd))))

        (defn env-upd_split
          "handle vectors and map literals semantics of the E+ macro
               one level only, the recursion will be eventually done by env-upds"
          [[x1 x2 & rs :as xs]]
          (cs

           ;; done!
           (not (seq xs)) []
           (word? x1)

           ;; primary path and vector
           (cs (and (vec? x2) (ppath? (path x1)))
               (concat ($ (env-upd_split x2) (p hash-map x1))
                       (env-upd_split rs))
               (cons {x1 x2} (env-upd_split rs)))

           ;; map
           (holymap? x1)
           (cons x1 (env-upd_split (rest xs)))

           ;; vec
           (vec? x1)
           (concat (env-upd_split x1)
                   (env-upd_split (rest xs)))

           ;; else
           (cons {root-path x1} (env-upd_split (rest xs)))
           ))

        (asserts
         (= (env-upd_split '[a 1 b 2]) (list '{a 1} '{b 2}))
         (= (env-upd_split '[42 foo "iop"]) (list {root-path 42} '{foo "iop"})))

        (defn env-upds

          "destructure an update as taken by the E+ macro into a serie of base operations
               (not intended to be used directly)

               there is 4 base operations:
               :declare : declare a member, in order to make it available eventually before it is bound (analog to clojure's declare semantics) [:declare path]
               :def : evaluate the given expression and  put the result at the given path. [:def path expression]
               :link : define a link from a path to another [:link path1 path2]
               :fx : a side effect [:fx path expression]
              "

          ([e x]
           (env-upds e x root-path))
          ([e x from]
           #_(println 'top x from)
           (cs (ppath? from)
               (cp x

                   holymap?
                   (env-upd_sort
                    (env-upd_prepend-declarations
                     (mapcat (fn [[k v]] (env-upds e v (path from k))) x)))

                   vec?
                   (mapcat #(env-upds e % from) (env-upd_split x))

                   string?
                   [[:def (path from :doc) x]]

                   (p env-upd_upd-expr? (mv e from))
                   (let [e' (mv e from)
                         [_ updf] (env-upd_upd-expr? e' x)]
                     (env-upds e (updf e' (cdr x)) from))

                   [[:def (path from :val) x]])

               [epath (p path (loc e))]
               [(condp = (.mkey from)
                  :links [:link (epath (ppath from)) x]
                  :fx [:fx (epath (ppath from)) x]
                  :clj [:clj nil x]
                  [:def (epath from) x])])))

        (_ :tries

           (env-upds
            @E '[iop (generic [x] :vec 'veciop)
                 a 1
                 b [c 1 {d 3 b:op 56}]])

           (defmacro E+ [& xs]
             `(do ~@(map (fn [u]
                           `(env-upd_exe @E (env-upds2 @E '~u)))
                         (env-upd2_split xs))))


           (env-upds2 @E '{op {baba 12}})

           (_ :error
              (rEset!)
              (map (p env-upds2 @E)
                   (env-upd2_split
                    '[op [yupd:upd (fn [_ xs] {'pouet:val (vec xs)})
                          baba (op.yupd 1 2 3)]]))

              (E+2 op [yupd:upd (fn [_ xs] {'pouet:val (c/vec xs)})
                       baba (op.yupd 1 2 3)])

              (!! op.baba.pouet)))

        (defn env-upd_exe
          "takes a serie of base operation litterals (as returned by env-upds, refer to its docstring for details)
           and performs them sequencially on the global environment"
          [e u]
          (doseq [[verb at x] u]
            #_(pp [verb at x])
            (swap! E
                   (fn [e]
                     (try
                       (condp = verb
                         :link (env-add-member e (path at :links) x)
                         :declare (env-declare-member e at)
                         :fx (do (eval (mv e at) x) e)
                         :clj (do (c/eval x) e)
                         :def (env-member-add-compiled e at x))
                       (catch Exception err
                         (error "\nenv-upd error compiling:\n"
                                (pretty-str [verb at x])
                                "\n" (.getMessage err)))))))
          @E)))

(do :API

    (defmacro E+
      "the main way to extend and update the asparagus global environment
       for implementation details refer to the previous section (:extension)
       for usage, see ./tutorial.clj"
      [& xs]
      `(do ~(when (symbol? (first xs))
              `(println "E+ " '~(first xs)))
           ~@(map (fn [u]
                      `(env-upd_exe @E (env-upds @E '~u)))
                  (env-upd_split xs))))

    (defmacro E-
      "removes given member-paths|coresponding-symbols from the global env
       ex:
       (E- iop foo)
       (E- {hey {bob [a z e] bae {iop [i o p]}}})
       "

      [& xs]
      (cons 'do (E-minus root-path xs)))

    (defmacro !!
      "a little convenience, that is mainly useful for development
       takes an expression and compile it at macro expansion time, resulting in an evaluable clojure form"
      [x]
      (->> x
           (qualify @E)
           (expand @E)
           (resolve @E)))

    (defn env-inspect
      "resolve the given symbol in the global env
       and print a detailed description of it (compilation intermediate values included)"
      [s]
      (let [[p x] (bubfind @E (path s))]
        (println "at: " p)
        (println "\ndata:")
        (pp x)
        (println "\nmeta:")
        (pp (meta x))
        (println)))

    (defmacro ppenv
      "a thin wrapper around env-inspect, let you pass an unquoted symbol"
      [s]
      `(env-inspect '~s))

    (defmacro updxp
      "expand the given update call, debug purposes"
      [[v & args]]
      `(!! (~(sym v ":upd") @E '~(vec args))))

    (defmacro ppdoc [s]
      (c/let [[p v] (bubfind @E (path s :doc))
              ;; this is emacs related stuff... (the identation of strings in emacs is weird)
              indent-first-line? (not (re-find #"^\n.*" v))
              indent-size (count (re-find #"\n\s*" v))
              vs (if indent-first-line?
                   (str (apply str (repeat (dec indent-size) " ")) v)
                   v)]
        `(do (println '~p "\n")
             (println ~vs)
             (println))))

    (defn source
      "find the original source code for the given symbol"
      [s]
      (c/let [p (path s)
              mkey (.mkey p)
              [p x] (bubfind @E (ppath s))]
        (if mkey
          (get-in (meta x) [mkey :expr])
          (reduce (fn [a k] (assoc a k (source (path p k))))
                  {} (remove #(= :doc %) (keys x))))))

    (defn top-form-decl
      "bring an asparagus macro into the top level,
       making it available as a regular clojure macro in the current ns"
      [s]
      (p/assert (env-find @E (path s))
                "unknown path")
      `(defmacro ~s [~'& xs#]
         (res @E (list* '~s xs#))))

    (defmacro init-top-forms
      "bring several asparagus macros into the top level,
       making them available as regular clojure macros in the current ns"
      [& xs]
      (swap! top-forms into xs)
      `(do ~@($ xs top-form-decl))))

;; from this point asparagus is built in itself (using E+ macro)
;; for a gentle introduction to E+, please refer to ./tutorial.clj
;; ------------------------------------------------------------------------------

(do :asparagus

    (rEset!)

    (do :base

        "in this section we will define the most basic forms that asparagus will build upon
         let, lambda, loop, cs (same semantics as asparagus.boot/cs)
         those version will be hygienic in the sense that they will replace all introduced binding symbols with gensyms"

        (E+

         env
         [exp:val asparagus.core/exp

          add-sub
          ["add a substitution to an environment at current location
            it is used for shadowing bindings in let and lambda"
           (c/fn [e [s1 s2]]
             (c/let [at (path (loc e) s1)]
               (env-merge-members
                e (c/merge
                   {(path at :sub) (k s2)
                    (path at :local) true}
                   (when (bubfind e (path s1 :mac))
                     {(path at :mac) (c/fn [e form] ($ (cons s1 form) (p exp e)))})))))]

          add-subs
          (c/fn [e submap]
            (c/reduce add-sub e submap))]

         :links {exp env.exp}

         hygiene
         {shadow
          [ ;; helpers
           expand-keys-pattern
           ["transform the clojure :keys syntax into regular map pattern
             because all symbol will be made uniq and it will no longer work as is"
            (c/fn rec [pat]
              (cp pat
                  vec? ($ pat rec)
                  map?
                  (cs [ks (:keys pat)]
                      (merge ($keys (dissoc pat :keys) rec)
                             (zipmap ks ($ ks keyword)))
                      ($keys pat rec))
                  pat))]

           gensym?
           (c/fn [x]
             (and (symbol? x)
                  (re-matches #"^.*_+[0-9]+#*$" (name x))))

           pat->submap
           ["turn a clojure binding pattern into a substitution map
             from symbol to uniq symbol"
            (c/fn [pat]
              (c/let [syms (shrink- (p/findeep pat symbol?) (p = '&))]
                (->> (set syms)
                     (c/map (fn [s] [s #_(gensym (sym s '_))
                                    (if (gensym? s) s (gensym (sym s '_)))]))
                     (into {}))))]

           ;;main
           "takes an environment and a binding pattern
            return a tuple [e p] where
            e: new environment with uniq symbols substitutions
            p: the pattern with all syms made uniq"
           (c/fn [e pat]
             (c/let [pat (.expand-keys-pattern pat)
                     shadenv (env.add-subs e (.pat->submap pat))]
               [shadenv (exp shadenv pat)]))]}

         primitives
         [" 'hygienic' versions of lambda and let (with all binding symbols made uniq)"

          fn
          {:doc
           "we will use this version of lambda to build asparagus,
            it will ultimatly be replaced by the 'f macro (and friends)"

           parse
           (c/fn [[fst & nxt :as all]]
             (c/let [[name b1 & bs]
                     (if (word? fst)
                       (cons (sym fst) nxt)
                       (concat [nil fst] nxt))

                     impls
                     (if (vector? b1)
                       {b1 (c/vec bs)}
                       (into {}
                             (c/map
                              (c/fn [[args & body]]
                                [args (c/vec body)])
                              (cons b1 bs))))]

               {:name name
                :impls impls}))

           expand-case
           (c/fn [e [argv body]]
             (c/let [[e argv] (hygiene.shadow e argv)]
               (cons argv ($ body (p exp e)))))

           expand
           (c/fn [e {:keys [name impls]}]
             (c/let [name (or name 'rec)
                     [e n] (hygiene.shadow e name)]
               (lst* `fn n
                     ($ (seq impls)
                        (p ..expand-case e)))))

           :mac
           (c/fn [e form]
             (.expand
              e (.parse form)))

           :demo
           '(do
              (exp @E '(fn [a b] (add a b)))
              ;;=>
              '(clojure.core/fn rec_179091
                 ([a_179092 b_179093]
                  (add:val a_179092 b_179093)))

              ;; as you can see, all symbols are qualified, and introduced bindings made uniqs

              (exp @E '(fn [{:keys [a b] :as m}] [a b m]))
              ;;=>
              '(clojure.core/fn rec_179123
                 ([{:as m_179125, a_179124 :a, b_179126 :b}]
                  [a_179124 b_179126 m_179125]))

              (exp @E '(fn me [a b] (me a b)))
              ;;=>
              '(clojure.core/fn me_179161
                 ([a_179162 b_179163]
                  (me_179161 a_179162 b_179163)))

              (exp @E '(fn [a b] (rec a b)))
              ;;=>
              '(clojure.core/fn rec_179193
                 ([a_179194 b_179195] (rec_179193 a_179194 b_179195)))

              (exp @E '(fn
                         ([a] (rec a a))
                         ([a b] (add a b))
                         ([a b & xs] (reduce rec (rec a b) xs))))
              ;;=>
              '(clojure.core/fn rec_179360
                 ([a_179361] (rec_179360 a_179361 a_179361))
                 ([a_179362 b_179363] (add:val a_179362 b_179363))
                 ([a_179364 b_179366 & xs_179365]
                  (clojure.core/reduce rec_179360 (rec_179360 a_179364 b_179366) xs_179365)))
              )}

          let
          {parse
           (fn [[bs & body]]
             {:bindings (partition 2 bs)
              :body body
              :monobody (= 1 (count body))})

           expand
           (fn [e {:keys [bindings body monobody]}]
             (c/loop [e e ret [] [[p1 e1] & pes] bindings]
               (if p1
                 (c/let [[e' pat] (hygiene.shadow e p1)]
                   (recur e' (conj ret pat (exp e e1)) pes))
                 (lst `let ret (exp e (if monobody (car body) (cons 'do body)))))))
           :mac
           (fn [e form]
             (.expand
              e (.parse form)))

           :demo
           '(do

              ;; like primitives.lambda, it qualify symbols and makes uniq introduced binding symbols

              (exp @E '(primitives.let [a 1] a))
              ;;=>
              '(clojure.core/let [a_179450 1] a_179450)

              (exp @E '(primitives.let [{:keys [a b] :as m} {}] [a b m]))
              ;;=>
              '(clojure.core/let
                   [{:as m_179459, a_179458 :a, b_179460 :b} {}]
                   [a_179458 b_179460 m_179459])
              )}

          loop:mac
          (fn [e xs]
            `(loop ~@(cdr (exp e (lst* 'primitives.let xs)))))

          cs
          [generated-binding-sym?
           (fn [x]
             (re-matches #"^((vec)|(seq)|(first)|(map))__[0-9]+$"
                         (name x)))

           ?let:mac
           (fn [e [[s x] r]]
             (let [s' (gensym)]
               (exp e (lst 'primitives.let [s' x]
                           (lst `when s' (lst 'primitives.let [s s'] r))))))

           case
           (fn
             [[b1 b2 & bs] e]
             (lst (if (or (generated-binding-sym? b1)
                          (= \_ (first (name b1))))
                    'primitives.let 'primitives.cs.?let)
                  [b1 b2]
                  (if bs (case bs e)
                      ;; this wrapping is nescessary for the case e eval to nil
                      [e])))

           form
           (fn [[x e & xs]]
             (let [bs (if (vector? x) x [(gensym) x])
                   frm (case (destructure bs) e)]
               (cond
                 (not (seq xs)) frm
                 (not (next xs)) (lst `c/or frm [(first xs)]) ;; same thing here
                 :else (lst `c/or frm (form xs)))))

           :mac
           (fn [e xs]
             (exp e (lst `first (.form xs))))

           optimized
           ["try to collapse cs emitted form into a more compact one
             grouping let levels when possible and substituting instead of binding sym to sym"

            or-expr?
            (fn [x]
              (and (seq? x) (= `c/or (car x))))

            remove-useless-ors
            (fn  [x]
              (cp x
                  or-expr?
                  (cons `c/or
                        (mapcat (fn [y]
                                  (mapv remove-useless-ors (if (or-expr? y) (cdr y) [y])))
                                (cdr x)))
                  holycoll?
                  ($ x rec)
                  x))

            substitutable-sym?
            (fn [x]
              (and (sym? x)
                   (not (p/generated-binding-sym? x))))

            let-expr?
            (fn [x]
              (when (and (seq? x) (= `let (car x)))
                x))

            collapse
            (fn [x]
              (cs [[_ bs expr] (let-expr? x)
                   [_ bs' expr'] (let-expr? expr)]
                  (rec `(let ~(catv bs bs') ~expr'))
                  (seq? x)
                  ($ x rec)
                  x))

            substitute
            (fn [e x]
              (cs [[_ [b1 b2] expr] (let-expr? x)
                   ? (and (substitutable-sym? b1) (substitutable-sym? b2))]
                  (rec e (exp (env.add-sub e [b1 b2]) expr))

                  (seq? x)
                  ($ x (p rec e))

                  x))

            optimize
            (fn [e x]
              (collapse
               (substitute e (exp e x))))

            :mac
            (fn [e xs]
              (remove-useless-ors
               (optimize e (lst `first (form xs)))))

            :tries
            '(do (exp @E '(cs [[x & _xs] 1] 1 :nop))
                 )]

           :demo
           '(do

              ;; for complete overview of the semantics and usage  of cs,
              ;; please refer to asparagus.boot.prelude (:macros section)

              (exp @E
                   (let [x 1 y nil z 2]
                     '(cs [a ~x b ~y] (add a b)
                          [a ~x b ~z] (sub a b)
                          [_a ~y b ~z] (if _a (mul _a b) b)
                          :fail)))

              ;;=>
              '(clojure.core/first
                (clojure.core/or
                 (clojure.core/let
                     [G__293545 1]
                     (clojure.core/when
                         G__293545
                         (clojure.core/let
                             [G__293550 nil]
                             (clojure.core/when G__293550 [(add:val G__293545 G__293550)]))))
                 (clojure.core/let
                     [G__293546 1]
                     (clojure.core/when
                         G__293546
                         (clojure.core/let
                             [G__293553 2]
                             (clojure.core/when G__293553 [(sub:val G__293546 G__293553)]))))
                 (clojure.core/or
                  (clojure.core/let
                      [_a_293547 nil G__293548 2]
                      (clojure.core/when
                          G__293548
                          [(if _a_293547 (mul:val _a_293547 G__293548) G__293548)]))
                  [:fail])))
              )]]

         :links {fn primitives.fn
                 let primitives.let
                 loop primitives.loop
                 cs primitives.cs.optimized}

         env
         [get
          [
           "getting things inside an environment"
           (fn [e s]
             (second (env-absfind e (path s))))

           val (fn [e s] (env.get e (sym s ":val")))
           mac (fn [e s] (env.get e (sym s ':mac)))
           sub (fn [e s] (env.get e (sym s ':sub)))]

          put
          [
           "putting things inside an environment"
           (fn [e s v & svs]
             (let [e' (assoc-in e (env-member-path (path s)) v)
                   #_(env-add-member e (path s) v)]
               (if svs (apl rec e' svs) e')))

           val (fn [e s v] (env.put e (sym s :val) v))
           mac (fn [e s v] (env.put e (sym s :mac) v))
           sub (fn [e s v] (env.put e (sym s :sub) v))]

          upd
          [
           "updating things inside an environment"
           (fn [e s f & sfs]
             (let [e' (env.put e s (f (env.get e s)))]
               (if sfs (apl rec e' sfs) e')))]

          qualify
          [
           "takes an environment e and an expression x
           it will turn all resolvable symbols into paths, using bubbling resolution
           (see bubfind and qualsym in the [:env :getters] section)
           macro calls will be handled differently, the verb will be qualified, the expression marked, and the arguments left as is"

           fail
           ["the function that is called when a symbol cannot be qualified"
            (fn [x] x)]

           gensym?
           (fn [x]
             (and (symbol? x)
                  (re-matches #"^.*_[0-9]+#*$" (name x))))

           unqualifiable-symbols:val
           '#{if do recur fn* let* try catch quote _ . .. &}

           unqualifiable?
           (fn [x]
             (or (unqualifiable-symbols x)
                 (gensym? x)))

           (fn [e x]

             (cp x

                 ;; symbols
                 sym?
                 (cs [qs (qualsym e x)] qs ;; x is qualifiable in e
                     (unqualifiable? x) x ;; unqualifiable symbol (gensyms or clojure special forms) left as is
                     (c/resolve x) (ns-resolve-sym x) ;; else we use clojure/resolve
                     (env.get e 'env.qualify:strict) (error "unqualifiable symbol: " x)
                     ((env.get.val e 'env.qualify.fail) x)) ;; else we use qualify.fail on x

                 ;; fullquote we do nothing
                 p/quote? x

                 seq?
                 (cs
                  ;; if x is a macro call, we qualify the verb (the macro identifier)
                  ;; and mark the expression as expansion
                  [p (-> x car path)
                   ? (or (ppath? p) (macpath? p))
                   p (qualsym e (path p :mac))]
                  (mark-exp (cons p (cdr x)))
                  ;; else we just map
                  (p/$ x (p rec e)))

                 ;; for clojure's collection, we map qualify
                 holycoll?
                 (p/$ x (p rec e))

                 ;; else do nothing
                 x))

           :strict true

           strict
           (fn [e x]
             (qualify
              (env.put.val
               e 'env.qualify.fail
               #(error "not resolvable: " %))
              x))]]

         :links {qualify env.qualify}

         error
         ["throw an error printing some context (not so great, to improve!)"
          :mac
          (fn [e xs]
            (lst* `p/error
                  "at: " (path->str (loc e))
                  "\nctx:\n"
                  (pretty-str (first (:exp-ctx e)))
                  "\n"
                  (exp e xs)))]

         __
         ["the comment macro"
          :mac (fn [_ _])]))

    (E+ composite

        ["the composite module contains some utily to handle composite litterals
          e.g: datastructures litterals with dot notation, for instance [a b . c] or {:a 1 . x}"

         dot (symbol ".")
         dotdot (symbol "..")
         dot? (fn [x] (= dot x))
         dotdot? (fn [x] (= dotdot x))

         dotted?
         (fn [x]
           (cp x
               map? (contains? x dot)
               sequential? (indexof x dot)
               nil))

         composed?
         ["x contains some composition operators
           (. and/or ..)"
          (fn [x]
            (cp x
                quote? nil
                map? (or (contains? x dot) (contains? x dotdot))
                sequential? (or (indexof x dot) (indexof x dotdot))
                nil))]

         not-composed?
         (fn [x] (not (composed? x)))

         single-dotted?
         ["x has only one dot (useful in bind)"
          (fn [x]
            (and (dotted? x)
                 (cp x
                     map? (not (contains? x dotdot))
                     sequential?
                     (and (not (indexof x dotdot))
                          (= 1 (count (filter dot? x)))))))]

         seq-parts
         (fn [s]
           (loop [[fs ss & rs] s ret []]
             (cp fs
                 not ret
                 dot? (recur rs (conj ret ss))
                 dotdot? (catv (conj ret ss) rs)
                 (recur (cons ss (or rs ()))
                        (if (vector? (last ret))
                          (conj (vec (butlast ret)) (conj (last ret) fs))
                          (conj ret [fs]))))))

         expand
         (fn [x]
           (cp x

               composed?
               (cp x
                   vec? (lst `vec (lst* `concat (map rec (seq-parts x))))
                   seq? (lst `p/call* (lst* `concat (map rec (seq-parts x))))
                   map? (lst* `merge
                              (rec (dissoc x dot dotdot))
                              (rec (get x dot))
                              (map rec (get x dotdot))))
               holycoll?
               ($ x rec)

               x))

         :demo
         (__

          ;; vecs --------

          (!! (composite.expand (quot [a b . c])))
          ;;=>
          '(clojure.core/vec (clojure.core/concat [a b] c))

          (!! (composite.expand (quot [a b . c . d])))
          ;;=>
          '(clojure.core/vec (clojure.core/concat [a b] c d))

          (!! (composite.expand (quot [a b .. c d])))
          ;;=>
          '(clojure.core/vec (clojure.core/concat [a b] c d))

          ;; maps --------

          (!! (composite.expand (quot {:a 1 . b})))
          ;;=>
          '(clojure.core/merge {:a 1} b)

          (!! (composite.expand (quot {:a 1 .. [b c d]})))
          ;;=>
          '(clojure.core/merge {:a 1} b c d)

          ;; lists -------

          (!! (composite.expand (quot (fun a b . c))))
          ;;=>
          '(asparagus.boot.prelude/call*
            (clojure.core/concat [fun a b] c))
          ;; which is equivalent to
          '(apply fun (clojure.core/concat [a b] c))

          (!! (composite.expand (quot (fun a b . c . d))))
          ;;=>
          '(asparagus.boot.prelude/call*
            (clojure.core/concat [fun a b] c d))

          (!! (composite.expand (quot (fun a b .. c d e))))
          ;;=>
          '(asparagus.boot.prelude/call*
            (clojure.core/concat [fun a b] c d e))
          )
         ]
        )

    (E+ quotes
        [:links {cp composite}

         wrap
         (fn [x] (list 'quote x))
         rootsym
         (fn [p] (path->sym (path '_ p)))

         quote?
         (fn [e x]
           (and (seq? x)
                (sym? (car x))
                (#{(path 'quotes.sq:mac)
                   (path 'quotes.qq:mac)
                   (path 'quotes.qq!:mac)}
                 (or (qualsym e (car x))
                     (qualsym e (sym (car x) ":mac"))))))

         unquote-quote?
         (fn [x]
           (and (unquote? x)
                (p/quote? (second x))))

         mk
         (fn [{:keys [strict qualified]}]
           (fn [e lvl form]
             #_(pp "mk2 in" lvl form "_")
             (cp form

                 ;; we do not touch dots
                 ;; they will be handled via composite.expand after quoting
                 cp.dot? cp.dot
                 cp.dotdot? cp.dotdot

                 ;; if quote-unquote we strip a lvl
                 quotes.unquote-quote?
                 (rec e (dec lvl) (second (second form)))

                 ;; if unquote we perform expansion with e
                 unquote?
                 (cs (zero? lvl)
                     (exp e (second form))
                     (list `list (quotes.wrap `unquote) (rec e (dec lvl) (second form))))

                 ;; if nested quote
                 (p quotes.quote? e)
                 (list `list (quotes.wrap (car form)) (rec e (inc lvl) (second form)))

                 ;; handle collections
                 seq? (cons `list ($ form (p rec e lvl)))
                 holycoll? (p/$ form (p rec e lvl))

                 symbol?
                 (cs (not qualified)
                     (quotes.wrap form)

                     [pth (path form)
                      [p v] (bubfind e pth)]
                     (cs (get v :local)
                         (quotes.wrap (exp e form))
                         (quotes.wrap (quotes.rootsym p)))

                     [s (ns-resolve-sym form)]
                     (quotes.wrap s)

                     (cs strict
                         (error "unqualifiable symbol: " form)
                         (quotes.wrap form)))

                 ;; else we quote wathever it is
                 (quotes.wrap form))))

         sq
         ["syntax-quote, does handle unquote and splicing, but do not qualifies symbols"
          :mac (fn [e [x]] (let [f (quotes.mk {})] (exp e (f e 0 x))))]
         qq
         ["qualified quote, like sq (syntax-quote), but qualifies symbols"
          :mac (fn [e [x]] (let [f (quotes.mk {:qualified true})] (exp e (f e 0 x))))]
         qq!
         ["strictly qualified quote, a strict version of qq (qualified-quote) that throws on unqualifiable symbols"
          :mac (fn [e [x]] (let [f (quotes.mk {:qualified true :strict true})] (exp e (f e 0 x))))]

         :demo
         (__
          (!! (sq (add 1 2 x ~(+ [] (lst 1 2)))))
          (!! (qq (add 1 2 a ~(sip [] . (lst 1 2)))))
          #_(!! (throws (qq! (add 1 2 a ~(+ [] (lst 1 2))))))
          (qq (add 1 2 a ~(sip [] . (lst 1 2))
                   (qq (a b c ~'(add 1 2 . ~(lst 3 4)))))))
         ]

        {:links {sq quotes.sq
                 qq quotes.qq
                 qq! quotes.qq!}}

        )

    (E+ env
        {
         expand
         [
          "expansion compiler step"

          (fn [e x]
            ((env.expand.mk-fn e) x))

          mk-fn
          (fn [e]
            (let [expand-module (env.get e 'env.expand)
                  get-expanding-step
                  (fn [k]
                    (if (c/get-in expand-module [:flags k])
                      (p (env.get.val e (sym 'env.expand. (sym k))) e)
                      (fn [x] #_(pp k 'no-op) x)))]
              (c/comp
               #_(p p/prob 4)
               (get-expanding-step :composite)
               #_(p p/prob 3)
               (get-expanding-step :method-calls)
               #_(p p/prob 2)
               (p env.expand.raw e)
               #_(p p/prob 1)
               (get-expanding-step :top-lvl-unquotes)
               #_(p p/prob 0)
               )))

          raw
          [
           "takes an environment e and an expression x
            will handle substitutions and macro calls"
           (fn [e x]
             (cp x
                 p/quote? x
                 ;; subsitution
                 subpath? (expand-subpath e x)
                 ;; macro call
                 mcall? (expand-mcall e x)
                 ;; clojure collection
                 holycoll? ($ x (p rec e))
                 ;; anything else
                 x))]

          :flags
          {:composite true
           :top-lvl-unquotes true
           :method-calls true}

          ;; impls

          composite
          (fn [_ x] #_(pp 'composite.expand x)
            (composite.expand x))

          top-lvl-unquotes
          (fn

            ([e x] (rec e 0 x))

            ([e lvl x]
             (cp x

                 p/quote? x

                 (p quotes.quote? e)
                 (rec e (inc lvl) (second x))

                 unquote?
                 (cs (zero? lvl)
                     (c/eval (res e (exp e (second x))))
                     (rec e (dec lvl) (second x)))

                 mcall? x

                 holycoll?
                 ($ x (p rec e lvl))

                 x)))

          method-calls
          [
           "this compilation step will insert § in front of sexpr starting with a litteral vec or map
            and will handle object oriented syntax (sexpr starting with a keyword) like the janet language do"

           (fn [e x]
             (cp x

                 p/quote? x

                 seq?
                 (cs [x1 (car x)
                      ? (keyword? x1)
                      [o & _xs] (cdr x)
                      o (rec e o)]
                     (lst* (env.exp e (qq §))
                           (env.exp e (qq (or (c/get ~o ~x1) (.method-not-found ~o ~x1))))
                           o (when _xs ($ _xs (p rec e))))
                     ($ x (p rec e)))

                 holycoll?
                 ($ x (p rec e))

                 x))

           method-not-found
           (fn [o k]
             (error "object:\n" o "\nhas no "
                    k " implementation"))]]

         exp
         ["qualify and expand x with e"
          (fn [e x & [opts]]
            #_(pp 'exp x)
            (let [e
                  (env.upd e
                           'env.expand:flags (fn [v] (c/merge v opts))
                           'env.qualify:strict #(c/get opts :strict %))]
              (env.expand e (env.qualify e x))))]}

        env.exp:mac
        (fn [e xs]
          (if (= 2 (count xs))
            (env.exp:val e (qq (exp:val .~xs)))
            (qq '~(env.exp:val e (car xs)))))

        :links {exp env.exp})

    ;; misc
    (E+

     ;; importing math basics ops
     ;; because some of those symbols will be rebound
     add c/+
     sub c/-
     div c//
     mul c/*
     eq c/= ;; eq will be rebound later, but is needed for check blocks

     and:mac
     (fn [e xs]
       (let [xs' ($ xs (p exp e))]
         (qq (c/when (c/and .~(c/butlast xs')) ~(c/last xs')))))

     or
     ["same as the clojure or macro but returns nil when failing
       (the clojure's or macro can return false, which may conflict with some nil based shortcircuiting forms)"
      :mac
      (fn [e xs]
        (qq (c/or .~($ xs (p exp e)) nil)))]

     check
     {
      :doc
      "a quick way to assert things"

      :mac
      (fn [e xs]
        (exp e (qq (p/asserts . ~xs))))

      thunk
      {:mac
       (fn [e xs]
         (exp e (qq (fn [] (p/asserts . ~xs)))))
       :upd
       (fn [e xs] {:check (qq (check.thunk . ~xs))})}

      :demo
      (__
       ;; simple
       (check (pos? 1) (neg? -1))
       ;; thunk form
       (check.thunk (pos? 1) (neg? -1))
       ;; is equivalent to
       (fn [] (check (pos? 1) (neg? -1)))
       ;; update version (the thunk will be put under the :check attr)
       (E+ foo (check.thunk (pos? 1) (neg? -1)))
       (!! (foo:check)) ;; will call the thunk
       )}

     is:mac
     (fn [e xs]
       (exp e (qq (check (~'eq .~xs)))))

     isnt:mac
     (fn [e xs]
       (exp e (qq (check .~($ xs (fn [x] (lst `nil? x)))))))

     upd.mk
     ["an update that evaluate the given expression in order to produce another update"
      :upd (fn [e [x]] (eval e x))]

     group
     ["an update to spread common attrs to several identifiers"
      :upd
      (fn [e [metas & xs]]
        ($ (env-upd_split xs)
           ($vals #(vector metas %))))

      :demo
      (__
       (E+ (group {:myattr :pouet} foo 1 bar 2))
       ;; is equivalent to
       (E+ foo {:myattr :pouet :val 1}
           bar {:myattr :pouet :val 2}))]

     tagged:upd
     ["an update to spread tags to several identifiers"
      (fn [e [t & xs]]
        (let [ts (if (c/keyword? t) #{t} (c/set t))]
          (c/vec
           (mapcat
            (fn [x] [($ x (fn [[k _]] [(path k :tags) ts])) x])
            (env-upd_split xs)))))

      :demo
      (__
       (E+ (tagged :iop foo 1 bar 2))
       ;; is equivalent to
       (E+ foo {:tags #{:iop} :val 1}
           bar {:tags #{:iop} :val 2})

       ;; it also accept set litterals
       (E+ (tagged #{:iop :pouet} foo 1 bar 2))
       ;; is equivalent to
       (E+ foo {:tags #{:iop :pouet} :val 1}
           bar {:tags #{:iop :pouet} :val 2})
       )])

    (init-top-forms check is isnt)

    (E+ import
        {:doc
         "link the given paths at current location"

         build-upd
         ["produce a vector of :links updates"

          prefix-members
          (fn [e pref xs]
            (cs
             (= :all xs)
             (let [[_ v] (bubfind e (path pref))
                   xs (shrink+ (keys v) symbol?)]
               (rec e pref (vec xs)))

             (holymap? xs)
             (let [xs (mapcat (p* rec e) xs)]
               (rec e pref (vec xs)))

             (vec? xs)
             ($ xs (p path pref))))

          link-update
          (fn [xs]
            (let [ps ($ xs path)]
              (assert (c/every? ppath? ps)
                      "cannot only import primary paths")
              {:links
               (zipmap ($ ps (comp path->sym path-head))
                       ps)}))

          (fn
            ([e x]
             (link-update (prefix-members e root-path x)))
            ([e x & xs]
             (rec e (if (vec? x)
                      (apply hash-map root-path x xs)
                      (apply hash-map x xs)))))]

         :upd
         (fn [e xs]
           #_(pp "import.upd" (apl build-upd xs))
           (apl build-upd e xs))

         :demo
         (__

          ;; basic usage --------

          (E+
           ;; declaring some random stuff to work with
           foo.bar {a ["foobar a" (fn [] 'foobara)]}
           foo.qux 42

           ;; we will use import at the top level (env root)
           ;; this will make foo.bar available as bar, and foo.qux available as qux
           (import foo [bar qux])

           ;; import can be used nested (in this example at the 'here path)
           ;; this will make foo.bar available as here.bar, and foo.qux available as here.qux
           here [(import foo [bar qux])
                 ;; we use the fresh links to give 'here a :val
                 (lst (.bar.a) (add 1 .qux))])

          (!! bar.a:doc)    ;;=> "foobar a"
          (!! qux:val)      ;;=> 42
          (!! here:val)     ;;=> '(foobara 43)

          ;; the :all syntax ----

          ;; first we define some random stuff
          (E+ foot {foota 1 footb 2 footc {n 4}})
          ;; then we import them all
          (E+ (import foot :all))
          ;; then use it
          (!! (add foota footb footc.n)) ;;=> 7

          ;; chain --------------

          (E+ mymodule
              [ ;; import several things at once
               (import foo [bar qux]
                       foot :all)
               ;; we use the fresh links to bind mymodule:val
               (add qux foota footb footc.n)])
          (!! mymodule:val) ;;=> 49

          )})

    (E+ generic
        {:doc
         "an update to define a generic function
          and its related inspection and extension capabilities
          it is a wrapper around asparagus.boot.generics functionalities
          please refer directly asparagus.boot.generics source file for documentation and examples"

         :upd
         (fn [e body]
           (let [gsym (generic.symbol (loc e))]
             (assoc (generic.module gsym)
                    :fx (qq (generic.init ~gsym ~body)))))

         reduced:upd
         (fn [e [argv & decls]]
           (let [[arg1 varg] (p/gensyms)]
             (qq (generic
                  ([~arg1] ~arg1)
                  (~argv . ~decls)
                  (~(conj argv '& varg)
                   .~(c/mapcat
                      (fn [[t i]]
                        [t (qq (reduce (fn ~argv ~i) ~i ~varg))])
                      (c/partition 2 decls)))))))

         lambda-wrapper (qq fn)

         lambda-case-compiler
         (fn [e]
           (fn [case]
             (nth (exp e (lst* lambda-wrapper case)) 2)))

         spec
         (fn [e gsym bod]
           (g/compile-cases
            (g/generic-spec gsym bod)
            (lambda-case-compiler e)))

         module
         (fn [gsym]
           (qq {:val ~gsym
                inspect:val
                (fn [] ((g/get-reg) '~gsym))
                ;; this was previously handled directly in the extend:upd but needs to wait for expansion time
                extension-form:mac
                (fn [e bod]
                  (g/extension-form
                   (spec e '~gsym bod)))
                ;; now entend:upd just emit a macro call that will wait expansion time
                extend:upd
                (fn [e bod]
                  {:fx (lst* (qq extension-form) bod)})

                }))

         symbol
         (fn [n]
           (path->varsym (path n :generic)))

         init:mac
         (fn [e [n body]]
           (g/declaration-form
            (spec e n body)))

         type+
         {:doc
          "lets you implement one or several generics for a type.
           analog to extend-type"

          :upd
          (fn [e [type & body]]
            ($ (c/vec body)
               (fn [[n & xs]]
                 #_(pp :gentyp+ xs (g/impl-body->cases type xs))
                 (lst* (p/sym n ".extend")
                       (g/impl-body->cases type xs)))))}

         :demo
         (__

          ;; generic ----------------

          ;; defines a pul+ generic function
          ;; with 3 implementations for: strings, symbols and numbers
          (E+ pul+
              (generic [a b]
                       :str (str a b)
                       :sym (pul+ (str a) (str b))
                       :num (add a b)))

          ;; inspect
          (!! (pul+.inspect))

          ;; use
          (!! (pul+ 1 2))
          (!! (pul+ "a" 2))

          ;; extend
          ;; implement pul+ for vectors
          (E+ (pul+.extend
               [x y]
               :vec (catv x y)))

          ;; use the new impl
          (!! (pul+ [1] [7]))
          (!! (pul+.inspect))

          ;; generic.reduced ---------

          ;; let you define a binary generic function
          ;; and use reduce for calls with more than 2 arguments
          (E+ pil+
              (generic.reduced [a b]
                               :str (str a b)
                               :num (+ a b)))

          (!! (pil+.inspect))
          (!! (pil+ 7 9 7))

          ;; type extension

          (E+ (generic.type+
               :key
               (pul+ [x y] :keypul+)
               (pil+ [x y] :keypil+)))

          (!! (eq (pul+ :aze 1) :keypul+))
          (!! (eq (pil+ :aze 1) :keypil+))

          ;; scope checks ------------

          ;; here we are just checking that generic implementation have access to the local scope
          (E+ foo.bar.fortytwo:sub (fn [_] 42))
          (E+ foo.bar.g (generic [x] :num (+ ..fortytwo x)))
          (!! (foo.bar.g 1)))})

    (E+ fn&

        ["the idea is to be able to declare concisely variadic functions, while mitigating the performance cost
          exemple:
          (fn& [a b] (add a b ...))
          a and b are mandatory arguments, the minimum arity is therefore 2
          but if we feed it more arguments, they will take place where the ... appears in the code.
          ... (which we call ellipsis) can appears several times in the function body.

          with fn&.expansion-size set to 3, the compiled function looks roughly like this
          (fn
           ([a b] (add a b))
           ([a b G__1] (add a b G__1))
           ([a b G__1 G__2] (add a b G__1 G__2))
           ([a b G__1 G__2 G__3] (add a b G__1 G__2 G__3))
           ([a b G__1 G__2 G__3 & G__4]
            (apply add a b G__1 G__2 G__3 G__4)))

          it would be absolutly painful to write such things by hand, and we gain performance on arity 1..5, and use apply only for larger ones
          this should looks like a minimal gain, because apply is quite permormant, but for core functions this gain is valuable"

         expansion-size 5

         replace-ellipsis
         ["turn a single ellipted body into several bodies"

          ellipse (symbol "...")
          ellipse? (p = ellipse)

          set-expr
          (fn [xs]
            (cons `hash-set
                  (sort-by ellipse? xs)))

          (fn [expr args applied?]
            (let [k #(rec % args applied?)]
              (cp expr
                  map? ($vals expr k)
                  set? (k (set-expr expr))
                  vec?
                  (cs (ellipse? (last expr))
                      (k (cons `vector expr))
                      ($ expr k))
                  seq?
                  (c/map k
                         (cs (ellipse? (last expr))
                             (cs applied?
                                 (cons `apply (c/concat (butlast expr) args))
                                 (c/concat (butlast expr) args))
                             expr))
                  expr)))]

         cases
         (fn [[argv expr]]
           (let [argsyms (c/take expansion-size (gensyms))
                 vsym (gensym)]
             (c/concat
              ($ (range (inc expansion-size))
                 (fn [x]
                   (let [args (c/take x argsyms)]
                     (lst (catv argv args)
                          (replace-ellipsis expr args false)))))
              [(lst (catv argv argsyms ['& vsym])
                    (replace-ellipsis
                     expr (conj (c/vec argsyms) vsym) true))])))

         :mac
         (fn [e form]
           (exp e (qq (fn . ~(fn&.cases form)))))])

    (E+ joining
        ["a bunch of function for handling monoidish things"

         pure
         ["a generic function that return the identity value from a given value"

          (generic
           [_]
           :fun id
           :vec []
           :lst ()
           :map {}
           :set #{}
           :str ""
           :sym (c/symbol "")
           :key (c/keyword "")
           #{:nil :any} nil)

          (check.thunk
           (eq [] (pure [1 2 3]))
           (eq #{} (pure #{:pouet 12}))
           (eq {} (pure {:a 1}))
           (eq "" (pure "hello")))]

         sip
         ["add elements to a collection, similar to core.conj"

          (generic.reduced
           [a b]
           :lst (c/concat a [b])
           #{:set :vec} (c/conj a b)
           :map (c/assoc a (c/first b) (c/second b))
           :fun (c/partial a b)
           :nil (c/list b))

          :notes
          "maybe we should consider to implement sip for named
           (sipping some chars makes sense but in practice...)"

          (check.thunk
           (eq (sip [] 1 2) [1 2])
           (eq (sip [1] 2 3) [1 2 3])
           (eq (sip #{1} 2 3) #{1 2 3})
           (eq (sip {:a 1} [:b 2] [:c 3]) {:a 1 :b 2 :c 3})
           (eq ((sip add 1) 1) 2))]

         iter
         ["return a seq from something"

          (generic
           [a]
           :nil ()
           #{:sym :key} (iter (c/name a))
           :any (c/or (c/seq a) ()))

          (check.thunk
           (eq () (iter []))
           (eq () (iter nil))
           (eq () (iter ""))
           (eq '(1 2) (iter [1 2]))
           (eq '(1 2) (iter '(1 2)))
           (eq '([:a 1] [:b 2]) (iter {:a 1 :b 2}))
           (eq '(\f \o \o) (iter "foo") (iter (sym "foo")) (iter :foo)))]

         +
         ["join two things together
           similar to concat, merge..."

          (generic.reduced
           [a b]
           :fun (c/comp b a)
           :lst (c/concat a (iter b))
           :str (c/str a b #_(.toString b))
           :sym (c/symbol (c/str (c/name a) b #_(.toString b)))
           :key (c/keyword (c/str (c/name a) (c/name b)))
           :num (c/+ a b)
           :nil (iter b)
           :any (c/reduce sip a (iter b)))

          (check.thunk
           (eq (+ {:a 1} {:b 2})
               {:a 1 :b 2})
           (eq (+ '(1 2) [3 4])
               '(1 2 3 4))
           (eq (+ [1 2] '(3 4) [5 6] '(7 8))
               [1 2 3 4 5 6 7 8])
           (eq (+ #{1 2} [3 4] '(5 6))
               #{1 2 3 4 5 6})
           (eq (+ :foo :bar)
               :foobar)
           (eq (+ 'foo :bar "baz")
               'foo:barbaz)
           (eq (+ "foo" 'bar 'baz)
               "foobarbaz")
           (eq ((+ inc inc inc) 0)
               3))] ;; :fx (+:check)

         vals
         ["return the values of a collection"

          (generic [x]
                   :map (c/or (c/vals x) ())
                   :coll (iter x)
                   :any (error "vals: no impl for " x))

          (check.thunk
           (eq '(1 2 3)
               (vals '(1 2 3))
               (vals [1 2 3])
               (sort (vals {:a 1 :b 2 :c 3}))
               (sort (vals #{1 2 3}))))]

         idxs
         ["return the idxs or keys of a collection"

          (generic [x]
                   :map (c/or (c/keys x) ())
                   :set (iter x)
                   :coll (c/range (c/count x))
                   :any (error "idxs: no impl for " x))

          (check.thunk
           (eq '(0 1 2)
               (idxs '(1 2 3))
               (idxs [1 2 3]))
           (eq (lst :a :b :c)
               (sort (idxs {:a 1 :b 2 :c 3}))
               (sort (idxs #{:a :b :c}))))]

         pure?
         ["test if something is equal to its pure value"

          (generic [x]
                   :lst (when-not (seq x) ())
                   (when (c/= x (pure x)) x))

          (check.thunk
           (pure? [])
           (pure? #{})
           (pure? "")
           (pure? {})
           (not (pure? [1 2]))
           (not (pure? {:a 1}))
           (not (pure? "iop")))]

         wrap
         ["use its first argument to determine which type of structure to build
           and build it using given extra args,
           'wrap uses sip
           'wrap+ uses +
           'wrap* uses sip applied (iterable last argument)
           'wrap+* uses + applied (iterable last argument)"

          wrap (fn& [x] (sip (pure x) ...))
          wrap+ (fn& [x] (+ (pure x) ...))
          wrap* (p apl wrap)
          wrap+* (p apl wrap+)

          derive
          ["an update to derive wrap operations from your type
            given a name and and a pure value returns a sequential update
            that defines the four wrap variants for your type"

           make-upd
           (fn
             ([[n e]]
              (let [n+ (+ n "+")
                    n* (+ n "*")
                    n+* (+ n "+*")]
                (qq [~n (fn& [] (sip ~e ...))
                     ~n+ (fn& [] (+ ~e ...))
                     ~n* (fn& [x] (apl ~n x ...))
                     ~n+* (fn& [x] (apl ~n+ x ...))])))
             ([x & xs]
              (apl catv ($ (cons x xs) make-upd))))

           :upd
           (fn [_ xs]
             (apl make-upd xs))]

          (check.thunk
           (eq (wrap [1 2 3] 4 5 6)
               (wrap* [1 2 3] 4 '(5 6))
               [4 5 6])
           (eq (wrap+ '(pouet pouet) '(1 2 3) #{4} [5 6])
               (wrap+* '(pouet pouet) '(1 2 3) [#{4} [5 6]])
               '(1 2 3 4 5 6)))]

         builtins
         ["wrap declinations for builtin holy types"

          (wrap.derive
           [lst ()]
           [vec []]
           [set #{}]
           [map {}])

          ;; overides for perf
          vec c/vector
          lst c/list
          set c/hash-set

          ;; words
          str c/str
          str* (fn& [x] (apl c/str x ...))
          key (fn& [] (+ (c/keyword "") ...))
          key* (fn& [x] (apl key x ...))
          sym (fn& [x] (+ (c/symbol (c/name x)) ...))
          sym* (fn& [x] (apl sym x ...))

          (check.thunk

           (eq (vec 1 2 3)
               (vec* 1 [2 3])
               (vec+ '(1) [2 3])
               (vec+* '(1) [[2] #{3}])
               [1 2 3])

           (eq (lst 1 2 3)
               (lst* 1 [2 3])
               (lst+ '(1) [2 3])
               (lst+* '(1) [[2] #{3}])
               '(1 2 3))

           (eq (set 1 2 3)
               (set* 1 [2 3])
               (set+ '(1) [2 3])
               (set+* '(1) [[2] #{3}])
               #{1 2 3})

           (eq (map [:a 1] [:b 2] [:c 3])
               (map* [:a 1] {:b 2 :c 3})
               (map* [:a 1] #{[:b 2] [:c 3]})
               (map+ {:a 1} #{[:b 2]} {:c 3})
               {:a 1 :b 2 :c 3})

           (eq (key "iop" 'foo :bar)
               :iopfoobar)
           (eq (sym "iop" 'foo :bar)
               'iopfoo:bar)
           (eq (str "iop" 'foo :bar)
               "iopfoo:bar"))]]

        (import

         joining
         [pure pure? sip iter + vals idxs
          wrap.wrap wrap.wrap+ wrap.wrap* wrap.wrap+*]

         joining.builtins
         [vec vec+ vec* vec+*
          lst lst+ lst* lst+*
          set set+ set* set+*
          map map+ map* map+*
          str str* key key* sym sym*]))

    (E+ iterables
        ["some functions to manipulate iterable structures"

         iterg
         ["an update to define generic functions for iterables
           hiding the iter/wrap boilerplate"

          :upd
          (fn [e [[a1 :as argv] expr]]
            (qq (generic
                 ~argv
                 :lst
                 ~expr
                 (let [a ~a1
                       ~a1 (iter ~a1)]
                   (wrap* a ~expr)))))]

         car (generic [x] (c/first (iter x)))
         last (generic [x] (c/last (iter x)))

         take (iterg [x n] (c/take n x))
         drop (iterg [x n] (c/drop n x))
         takend (iterg [x n] (c/take-last n x))
         dropend (iterg [x n] (c/drop-last n x))
         butlast (iterg [x] (c/butlast x))
         cdr (iterg [x] (c/rest x))
         rev (iterg [x] (c/reverse x))

         section
         ["selection from index to index"
          (iterg [x from to]
                 (-> x
                     (take to)
                     (drop from)))]

         splat
         ["split at"
          (fn [x n]
            [(take x n) (drop x n)])]

         uncs
         ["uncons"
          (fn [x]
            [(car x) (cdr x)])]

         runcs
         ["reverse uncons"
          (fn [x]
            [(butlast x) (last x)])]

         cons
         ["like core.list*
           but preserve collection type"
          (fn [& xs]
            (let [[cars cdr] (runcs xs)]
              (+ (pure cdr) cars cdr)))]

         cons?
         (fn [x]
           (when (and (coll? x)
                      (not (pure? x)))
             x))]

        (import
         iterables
         [car cdr take drop rev
          last butlast takend dropend
          uncs runcs cons cons?
          section splat])

        ;; vector optimized impls

        (generic.type+
         :vec
         (last [x] (get x (dec (count x))))
         (take [x n] (subvec x 0 (min (count x) n)))
         (drop [x n] (subvec x (min (count x) n)))
         (takend [x n] (let [c (count x)] (subvec x (- c n) c)))
         (dropend [x n] (subvec x 0 (- (count x) n)))
         (butlast [x] (subvec x 0 (dec (count x))))
         (section [x from to] (subvec x (max 0 from) (min (count x) to)))
         (cdr [x] (subvec x 1)))

        iterables:demo
        (__

         ;; car (is like Lisp's car or clojure.core/first)
         (!! (eq 1 (car (lst 1 2))))
         (!! (eq 1 (car [1 2])))
         (!! (eq [:a 1] (car {:a 1 :b 2})))

         ;; cdr (is like clojure.core/rest but preserve collection type)
         (!! (eq (cdr [1 2 3]) [2 3]))
         (!! (eq (cdr (lst 1 2 3)) (lst 2 3)))
         (!! (eq (cdr {:a 1 :b 2 :c 3}) {:b 2 :c 3}))

         ;; last
         (!! (eq 2 (last (lst 1 2))))
         (!! (eq 2 (last [1 2])))
         (!! (eq [:b 2] (last {:a 1 :b 2})))

         ;; butlast (is like clojure.core/butlast but preserve collection type)
         (!! (eq (cdr [1 2 3]) [2 3]))
         (!! (eq (cdr (lst 1 2 3)) (lst 2 3)))
         (!! (eq (cdr {:a 1 :b 2 :c 3}) {:b 2 :c 3}))

         ;; take (like clojure.core/take with arguments reversed and preserving collection type)
         (!! (eq (take (lst 1 2 3) 2) (lst 1 2)))
         (!! (eq (take [1 2 3] 2) [1 2]))
         (!! (eq (take {:a 1 :b 2 :c 3} 2) {:a 1 :b 2}))

         ;; drop
         (!! (eq (drop (lst 1 2 3) 2) (lst 3)))
         (!! (eq (drop [1 2 3] 2) [3]))
         (!! (eq (drop {:a 1 :b 2 :c 3} 2) {:c 3}))

         ;; takend
         (!! (eq (takend (lst 1 2 3) 2) (lst 2 3)))
         (!! (eq (takend [1 2 3] 2) [2 3]))
         (!! (eq (takend {:a 1 :b 2 :c 3} 2) {:b 2 :c 3}))

         ;; dropend
         (!! (eq (dropend (lst 1 2 3) 2) (lst 1)))
         (!! (eq (dropend [1 2 3] 2) [1]))
         (!! (eq (dropend {:a 1 :b 2 :c 3} 2) {:a 1}))

         ;; rev
         (!! (eq (rev [1 2 3]) [3 2 1]))
         (!! (eq (rev (lst 1 2 3)) (lst 3 2 1)))

         ;; section (select a subsection of a sequantial data structure)
         (!! (eq (section [1 2 3 4 5 6] 2 5) [3 4 5]))
         (!! (eq (section (lst 1 2 3 4 5 6) 1 5) (lst 2 3 4 5)))

         ;; splat (split a sequential datastructure at the given index)
         (!! (eq (splat [1 2 3 4] 2) [[1 2] [3 4]]))
         (!! (eq (splat (lst 1 2 3 4) 2) [(lst 1 2) (lst 3 4)]))

         ;; uncs (uncons)
         (!! (eq (uncs [1 2 3]) [1 [2 3]]))
         (!! (eq (uncs (lst 1 2 3)) [1 (lst 2 3)]))

         ;; runcs
         (!! (eq (runcs [1 2 3]) [[1 2] 3]))
         (!! (eq (runcs (lst 1 2 3)) [(lst 1 2) 3]))

         ;; cons
         (!! (eq (cons 1 [2 3]) [1 2 3]))
         (!! (eq (cons 1 (lst 2 3)) (lst 1 2 3)))
         ;; it can take more arguments
         (!! (eq (cons 0 1 [2 3]) [0 1 2 3]))
         (!! (eq (cons 1 2 3 (lst)) (lst 1 2 3)))

         ;; cons?
         (!! (eq (cons? [1 2]) [1 2]))
         (!! (nil? (cons? [])))
         (!! (cons? (lst 1 2) (lst 1 2)))
         (!! (nil? (cons? (lst))))
         (!! (eq (cons? {:a 1}) {:a 1}))
         (!! (nil? (cons? {})))
         (!! (nil? (cons? #{}))))

        )

    (E+ types
        ["wrap some of the asparagus.boot.types functionalities
          still have to handle type declaration (auto guard declaration...)
          a deftype like construct should be great too"

         prims (keys t/prims)
         builtins t/builtin-types
         preds t/builtin-preds

         ;; type generic
         (upd.mk
          (let [arg1 (gensym)]
            {'type
             (qq (generic
                  [~arg1]
                  .~(c/interleave prims prims)
                  :any (c/type ~arg1)))}))]

        (import types [type]))

    (E+ guards
        ["guards are like predicates
          but returns their first argument for success
          otherwise nil"

         guard
         ["building guards utilities"

          template
          (fn [arity]
            (let [arg1 (gensym)
                  argv
                  (if arity
                    (vec+ (take (gensyms) arity))
                    [(gensym) '& (gensym)])
                  test
                  (if arity
                    (lst* arg1 argv)
                    (lst `apply arg1 (car argv) (nth argv 2)))
                  form
                  (qq (fn [~arg1] (fn ~argv (when ~test ~(car argv)))))]
              form))

          builder:mac
          (fn [e [arity]]
            (exp e (guard.template arity)))

          unary (builder 1)
          binary (builder 2)
          ternary (builder 3)
          variadic (builder nil)]

         import
         {:doc
          "an update to wrap clojure predicates into guards"

          :upd
          (fn [_ xs]
            (apl build-upd xs))

          build-upd
          (fn
            ([[name arity original-name]]
             {name (qq (~(guard.template arity)
                        ~(ns-resolve-sym (or original-name name))))})
            ([x & xs]
             ($ (vec* x xs) rec)))

          types:upd
          (fn [e _]
            (let [ts (seq (disj types.builtins :nil))]
              (zipmap
               ($ ts #(sym % "?"))
               ($ ts #(lst `(@state/state :guards) %)))))}

         builtins
         [(guards.import
           [neg? 1]
           [pos? 1]
           [zero? 1]
           [empty? 1]
           [gt 2 c/>]
           [lt 2 c/<]
           [gte 2 c/>=]
           [lte 2 c/<=]
           [eq 2 c/=]
           [neq 2 c/not=])

          (guards.import.types)]

         :tries
         (__

          (res @E '(guard.types-definition nil nil))

          (!! (map? []))
          (!! (nil? nil))
          (!! (vec? []))

          (res @E '(guard.declare-types-guards))

          (res @E '(guard.import neg? 1))
          (env-upd? (exp @E '(guard.imports [neg? 1]
                                            [pos? 1]
                                            [gt 2 >])))

          (!! (line? [1 2]))

          (E+ (guard.import neg? 1))
          (E+ (guard.imports [neg? 1]
                             [pos? 1]
                             [gt 2 >]))
          (!! (neg? -2))
          ((!! (guard.unary pos?)) 2)
          ((!! (guard.variadic =)) 2 (+ 1 1) (- 4 2))
          (res @E '(guard.template nil)))]

        (import guards [guard]
                guards.builtins :all))

    (E+ testing
        ["the testing module is an attempt to describe tests as data"

         throws:mac
         (fn [e [x m]]
           (let [expr (c/str x)]
             (qq (c/or
                  (c/get
                   (try ~(exp e x)
                        (catch Exception err {::catched err}))
                   ::catched)
                  (p/error ~(or m "this should throws")
                           ":\n" ~expr)))))

         assertion
         ["a generic to turn something into an assertion expression"

          ;; helpers

          message?
          (fn [x]
            (or (keyword? x) (string? x)))

          message+
          (fn [m1 m2]
            (cs (not m1) m2
                (not m2) m1
                (if (vec? m1)
                  (conj m1 m2)
                  [m1 m2])))

          error-str
          (fn [x m]
            (str (or m "assertion fail") ":\n" x))

          mapdo:mac
          (fn [e [x f]]
            (exp e (qq (lst* 'do (c/map ~f ~x)))))

          vec-split
          (fn [x]
            (let [[[p1] :as parts] (partition-by message? x)
                  parts (if (message? p1) parts (cons nil parts))]
              (c/map (p* catv) (partition 2 parts))))

          ;; generic

          (generic
           [x m]
           :vec
           (mapdo (vec-split x)
                  (fn [[m' & xs]]
                    (mapdo xs (fn [a] (assertion a (message+ m m'))))))

           :map
           (sip (mapdo
                 x (fn [[k v]]
                     (assertion v (message+ m k))))
                ::ok)

           :nil
           (qq (p/error (error-str "nil" ~m)))

           :string nil

           :any
           (qq (or ~x (p/error (error-str '~x ~m))))
           )]

         assert
         {:mac
          (fn [e [x m]]
            (exp e (assertion x m)))
          :upd
          (fn [_ xs]
            {:fx (qq (assert .~xs))})}

         eq!:mac
         (fn [e xs]
           (exp e (qq (assert (eq .~xs) "must be equal"))))

         tests:upd
         (fn [e xs]
           (let [xs (vec* (assertion.vec-split xs))]
             {'tests
              {:form (qq '~xs)
               :do (qq (fn [] .~($ xs (fn [x] (qq (assert ~x [~(path->str (loc e)) '~x]))))
                         #_(assert ~(vec* xs) ~(path->str (loc e)))))}}))

         run-all:upd
         (fn [e xs]
           )

         ;; testing this module with itself
         (assert
          {:errors
           [(throws (assert (pos? -1) "not pos!"))
            (throws (assert [(pos? 1) (neg? 1)] "fail!"))
            (throws
             (assert
              {:pos1 (pos? 1)
               :gt3 [(pos? 2) (c/> 2 3)]}
              "map assertion fail!"))
            (throws
             (assert
              ["my assert"
               (pos? 1)
               (gt 3 2)
               [:A1
                (neg? -1)
                {"case 1" (nil? 1)
                 :b true}]]))]
           :ok
           [(eq 1 (assert (pos? 1) "not pos!"))
            (eq -1 (assert [(pos? 1) (neg? -1)] "not pos!"))
            (let [x 4]
              (eq ::ok (assert {:pos (pos? x)
                                :gt3 (c/> x 3)}
                               "map assertion fail!")))]})
         ]

        (import testing [assert throws tests eq!]))

    (E+ bindings
        {:val
         (fn [xs]
           (mapcat (p* bind) (partition 2 xs)))

         :links {cp composite}

         bind
         ["bind is like clojure's 'destructure:
           it produces a vector of bindings, that can be used as first argument of a let form
           any type can implement the bind generic function"

          (generic
           [x y]
           :sym [x y]
           :vec (bind.vec x y)
           :map (bind.map x y)
           :lst (bind.seq x y)
           [(gensym "?!match") (lst 'eq x y)])

          vec
          {:val
           (fn [x y]
             (if (cp.single-dotted? x)
               (.dotted x y)
               (.raw x y)))

           body
           (fn [x y]
             (mapcat
              (fn [v i] (bind v (qq (c/nth ~y ~i nil))))
              x (range)))

           raw
           (fn [x y]
             (let [ysym (gensym)]
               (+
                [ysym y
                 (gensym "?!linecheck") (qq (line? ~ysym))]
                (bind.vec.body x ysym))))

           dotted
           (fn [x y]
             (let [doti (indexof x cp.dot)
                   cars (take x doti)
                   [eli queue] (uncs (drop x (inc doti)))
                   qcnt (count queue)
                   [ysym qsym cdr' cars'] (gensyms)]
               (+
                [ysym y
                 (gensym "?!linecheck") (qq (line? ~ysym))]
                (bind eli (qq (drop ~ysym ~doti)))
                (bind.vec.body cars ysym)
                (when-not (zero? qcnt)
                  (+ [cdr' eli]
                     (bind eli (qq (dropend ~cdr' ~qcnt)))
                     [qsym (qq (takend ~cdr' ~qcnt))]
                     (bind.vec.body queue qsym))))))}

          map
          {:val
           (fn [x y]
             (if (cp.single-dotted? x)
               (.dotted x y)
               (.raw x y)))

           keys
           (fn [x y]
             (mapcat
              (fn [[k v]]
                (bind v (qq (get ~y ~k))))
              x))

           raw
           (fn [x y]
             (let [ysym (gensym)]
               (+
                [ysym y
                 (gensym "?!mapcheck") (qq (map? ~ysym))]
                (bind.map.keys x ysym))))

           dotted
           (fn [x y]
             (let [rs (get x cp.dot)
                   m (dissoc x cp.dot)
                   ks (c/keys m)
                   msym (gensym)]
               (+
                [msym y]
                (bind.map.keys m msym)
                (bind rs (qq (c/dissoc ~msym . ~ks))))))}

          seq
          (fn [[v & args] y]
            (cs [k (and (sym? v) (keyword v))
                 op (c/get bind.ops k)]
                (op args y)
                (bind.ops.default (cons v args) y)))

          ops
          {:doc

           "the binding operation table
            can be extended via the bind.op+ update"

           :val
           {:&
            (fn [xs y]
              (let [ysym (gensym)]
                (cat* [ysym y]
                      ($ xs #(bind % ysym)))))

            :ks
            (fn [xs y]
              (bind (zipmap ($ xs keyword) xs) y))

            :ks-opt
            (fn [xs y]
              (let [keys ($ xs keyword)
                    opt-syms ($ xs (p sym "_"))]
                (+ (bind (zipmap keys opt-syms) y)
                   (interleave xs opt-syms))))

            :ks-or
            (fn [xs y]
              (let [keys (take-nth 2 xs)
                    or-exprs ($ (partition 2 xs) (fn [[k v]] `(or ~k ~v)))]
                (+ ((get ops :ks-opt) keys y)
                   (interleave keys or-exprs))))

            #_:cons
            #_(fn [[a b] y]
              (let [ysym (gensym)]
                (+ [ysym y]
                   (bind a (qq (car ~ysym)))
                   (bind b (qq (cdr ~ysym))))))

            :tup
            (fn [xs y]
              (let [xs (vec* xs)
                    [ysym] (gensyms)]
                (+
                 [ysym y
                  (gensym "?!line") (qq (line? ~ysym))
                  (gensym "?!countable") (qq (c/counted? ~ysym))
                  (gensym "?!countcheck") (qq (= ~(count xs) (count ~ysym)))]
                 (bind.vec.body xs ysym))))

            :!
            (fn [[f & [p]] y]
              (bind (or p (gensym)) (lst f y)))}

           default
           [
            "the default operation, used for unknown binding operations"

            (fn [[v s & args] y]
              (cs (sym? s)
                  (cs (key? v)
                      [s y
                       (gensym "?!typecheck")
                       (cs [pred (t/get-guard v)]
                           (qq (~(sym v "?") ~s))
                           (qq (or (eq (type ~s) ~v)
                                   (t/>= ~v (type ~s)))))]
                      [s (lst* v y args)]
                      ;; considering to let s be any pattern... (does it makes sense? maybe for pure guards)
                      ;;(bind s (lst* v y args))
                      )
                  (error "guard binding takes a symbol as first argument"
                         (lst* v s args))))]}


          op+
          [
           "let the user add some new binding operations
            that will be available for further usages of bind"

           :upd
           (fn [e [name args expr]]
             ['bindings.bind.ops:val
              {(key name)
               (qq (f ~args ~expr))}])

           :demo
           (__
            ;; as an exemple we are redefining the & operation
            (E+ (bind.op+ & [xs seed] ;; xs are the arguments passed to the operation, y is the expr we are binding
                          (bind (zipmap ($ xs keyword) xs) seed)))

            ;; when this operation is used
            '(let [(ks a b) x] ...)

            ;; at compile time the implementation is called with args: '(a b) and seed: 'x
            ;; =>
            (bind {:a 'a :b 'b} 'x) ;; we are using the map impl of bind
            ;; =>
            '[G__244129 x
              G__244128 (_.guards.builtins.map? G__244129)
              a (clojure.core/get G__244129 :a)
              b (clojure.core/get G__244129 :b)]

            ;; finally it is substituted in the original form
            (let [G__244129 x
                  G__244128 (_.guards.builtins.map? G__244129)
                  a (clojure.core/get G__244129 :a)
                  b (clojure.core/get G__244129 :b)]
              ...))]

          ]

         unified
         (fn [xs]
           (loop [ret [] seen #{}
                  [a b & nxt] (bindings xs)]
             (if a
               (if (seen a)
                 (recur (conj ret (gensym) (qq (eq ~a ~b))) seen nxt)
                 (recur (conj ret a b) (conj seen a) nxt))
               ret)))

         bodify
         (fn [x]
           (if (= 1 (count x))
             (first x) (c/cons 'do x)))

         let
         {parse
          (fn [[x & xs :as form]]
            (let [[nam [x & xs]]
                  (if (keyword? x) [(sym x) xs] [nil (cons x xs)])
                  [bs body]
                  (cp x
                      map? [(seq x) xs]
                      vec? [(partition 2 x) xs])
                  expr (bindings.bodify body)]

              {:name (or nam (gensym))
               :name? nam
               :bs bs
               :expr expr
               :parrallel? (map? x)
               :form form}))

          sym->mode
          (fn [s]
            (or (maybe-strict-sym? s)
                (condp = (car (str s))
                  \! :strict
                  \? :short
                  \_ :opt
                  nil)))

          maybe-strict-sym?
          ["when a symbol starts by ?! it indicates that the resulting binding mode
            cannot be :opt, if current mode is strict it will be strict else it will be :short"
           (fn [s]
             (and (= '(\? \!) (c/take 2 (str s)))
                  :maybe-strict))]

          next-mode
          (fn [m1 s]
            (let [m2 (sym->mode s)]
              (cs (= :maybe-strict m2)
                  (cs (= :opt m1) :short m1)
                  m2 m2
                  m1)))

          step-form
          (fn [s e1 e2 mode]
            (condp = mode
              :opt `(let [~s ~e1] ~e2)
              :short `(let [~s ~e1] (when ~s ~e2))
              :strict `(let [~s (or ~e1 (error "nil! " (~(sym "quote") ~e1)))] ~e2)))

          form
          (fn [e [p1 e1 & bs] expr mode]
            (cs
             ;; no more bindings we just expand the body expression
             (not p1) (exp e expr)
             ;; if both e1 is syms we can just substitute
             ;; instead of adding a binding
             ;; we also check if p1 has not a different mode (have to think of this further)
             [? (sym? e1)
              ? (or (not (sym->mode p1))
                    (eq (next-mode mode p1) (next-mode mode e1))
                    #_(eq (sym->mode p1) (sym->mode e1))
                    )
              e' (env.add-sub e [p1 (exp e e1)])]
             (form e' bs expr mode)
             ;; else we add a binding
             [ ;; step-mode (or (sym->mode p1) mode)
              [e' pat] (hygiene.shadow e p1)]
             (step-form
              pat (exp e e1)
              (form e' bs expr mode)
              (next-mode mode p1)
              #_step-mode
              )))

          compile
          {:val
           (fn [e opts]
             (if (c/get opts :name?)
               (.named e opts)
               (.anonymous e opts)))

           anonymous
           (fn [e {:as opts :keys [unified bs expr]}]
             (bindings.let.form
              e (if unified
                  (bindings.unified (cat* bs))
                  (bindings (cat* bs)))
              expr
              (cp opts
                  :strict :strict
                  :short :short
                  :opt)))

           named ::unbound
           }

          compiler
          (fn [& flags]
            (fn [e form]
              (compile
               e (mrg (flagmap flags)
                      (parse form)))))

          }
         }

        bindings.let.builtins
        [let:mac  (compiler)
         ?let:mac (compiler :short)
         !let:mac (compiler :strict)
         lut:mac (compiler :unified :short)
         !lut:mac (compiler :unified :strict)

         let
         (check.thunk

          (eq (let [a 1] a)
              1)

          (eq (let [a 1 b 2] (add a b))
              3)

          ;; refer earlier binding

          (eq (let [a 1 b a] (add a b))
              2)

          ;; sequential bindings

          (eq (let [[x . xs] (range 5)] [x xs])
              [0 (range 1 5)])

          ;; preserve collection type

          (eq (let [[x . xs] (vec 1 2 3)] [x xs])
              [1 [2 3]])

          ;; post rest pattern

          (eq (let [[x . xs lastx] (range 6)] [x xs lastx])
              [0 (range 1 5) 5])

          ;; maps
          (eq (let [{:a aval :b bval} {:a 1 :b 2 :c 3}] [aval bval])
              [1 2])

          ;; maps have rest patterns to
          (eq (let [{:a aval . xs} {:a 1 :b 2 :c 3}] [aval xs])
              [1 {:b 2 :c 3}])

          ;; ks (similar to :keys)
          (eq (let [(ks a b) {:a 1 :b 2 :c 3}] (add a b))
              3)

          ;; & (parrallel bindings)
          (eq (let [(& mymap (ks a b)) {:a 1 :b 2 :c 3}] [mymap a b])
              [{:a 1 :b 2 :c 3} 1 2])

          ;; guards
          (eq (let [(pos? a) 1 (neg? b) -1] (add a b))
              0)

          ;; short-circuiting bindings
          ;; when a symbol is prepended with ? it shorts if bound to nil

          (nil? (let [?a nil b (error "never evaluated")] 42))

          ;; strict bindings
          ;; binding symbol's prepended by ! must bind to non nil value
          (eq :catched
              (try (let [!a (pos? -1)] :never)
                   (catch Exception _ :catched)))

          ;; you can use ? and ! prefixes in guard patterns but...
          ;; it is not so pretty... see '?let section

          (nil? (let [(pos? a) 1 (neg? ?b) 0] (div a ?b)))

          ;; type guards
          (eq [1 2] (let [(:vec v) [1 2]] v))
          (nil? (let [(:map v) [1 2] _ (error "never")] v))

          (eq :ok (let [:yo (key "yo")] :ok))
          (nil? (let [:yo 42] :ok))
          (eq :ok (let [a (inc 2) 3 a] :ok))
          (eq :ok (let [a 1 b 2 3 (add a b)] :ok))

          )

         :fx (let:check)

         ?let
         (tests

          :accumulate-bindings
          (eq (?let [a 1 b a] (add a b))
              2)

          :guards
          "with guards ?let make sense"
          (nil? (?let [(pos? a) -1] (error "never touched")))

          :bang-prefix
          "in ?let ! behaves the same as in let"
          (throws
           (?let [!a (pos? -1)] :never))

          :underscore-prefix
          "if you want to allow some binding to be nil in a ?let form use the _ prefix"
          (eq (?let [a 1 _b nil] (add a (or _b 0)))
              1))

         :fx (?let.tests:do)

         lut
         (check.thunk
          (eq (lut [a 1 a 1] (add a a))
              2)

          ;; this shorts because the second binding of a does not unify with the previous one
          (nil? (lut [a 1 a (inc a)] (error "never touched"))))

         :fx (lut:check)

         !lut
         (check.thunk

          (eq (!lut [a 1 a 1] (add a a))
              2)

          ;; on failing unification it throws
          (eq :catched
              (try (!lut [a 1 a (inc a)] :never)
                   (catch Exception _ :catched))))

         :fx (!lut:check)
         ]

        (import bindings.let.builtins
                [let ?let !let lut !lut])

        bindings.let.cased
        {parse
         (fn [xs]
           (let [cases (vec* (partition 2 2 nil xs))
                 default? (= 1 (count (last cases)))]
             {:cases
              (if default?
                (conj (butlast cases)
                      (cons [] (last cases)))
                cases)}))

         compile
         (fn [e {:keys [unified cases strict]}]
           (let [cs
                 ($ cases
                    (fn [[h e]]
                      (if (vec? h)
                        (lst (if unified (qq lut) (qq ?let))
                             h {::return e})
                        (lst `when h {::return e}))))

                 retform
                 (qq (get ~(exp e (lst* `or cs)) ::return))]
             (if strict
               #_'(assert ~retform "clet no match!")
               (qq (c/or ~retform ~(exp e (qq (error "clet no match!")))))
               retform)))

         compiler
         (fn [& flags]
           (fn [e form]
             (compile
              e (mrg (flagmap flags)
                     (parse form)))))}

        bindings.let.builtins
        [clet:mac (cased.compiler)
         clut:mac (cased.compiler :unified)
         !clet:mac (cased.compiler :strict)
         !clut:mac (cased.compiler :unified :strict)

         clet
         (tests

          :simple
          (eq (clet [x (pos? -1)] {:pos x}
                    [x (neg? -1)] {:neg x})
              {:neg -1})

          :simple2
          "each binding block can have several cases"
          (let [f (fn [seed]
                    (clet [x (num? seed) x++ (inc x)] x++
                          [x (str? seed) xbang (+ x "!")] xbang))]
            (and (eq 2 (f 1))
                 (eq "yo!" (f "yo"))
                 (nil? (f :pop))))

          :default-case
          (eq (clet [x (pos? 0) n (error "never touched")] :pos
                    [x (neg? 0) n (error "never touched")] :neg
                    :nomatch)
              :nomatch)

          :strict-throws
          (throws
           (!clet [x (pos? 0)] :pos
                  [x (neg? 0)] :neg))


          :clut-simple
          "unfied version of clet"
          (let [f (fn [seed]
                    (clut [[a a] seed] :eq
                          [[a b] seed] :neq))]
            (and (eq :eq (f [1 1]))
                 (eq :neq (f [1 2]))))

          :!clut-throws
          (let [x [:tup [1 2]]]
            (throws
             (!clut [[:wat a] x] :nop
                    [(:vec vx) x [:tup [a a]] vx] :yep)))

          (let [p [:point 0 2]]
            (clet [[:point x 0] p] :y0
                  [[:point 0 y] p] :x0
                  [[:point x y] p] [x y]))
          )

         :fx (clet.tests:do)
         ]

        (import bindings.let.builtins
                [clet clut !clet !clut]
                bindings
                [bind])
        )

    (E+ lambda
        {:links {cp composite}

         parse
         (fn [[x & xs :as form]]
           (let [[nam (& [x . xs] body)]
                 (if (keyword? x)
                   [(sym x) xs]
                   [nil form])

                 [doc [pat . body]]
                 (if (string? x)
                   [x xs]
                   [nil body])

                 arity
                 (when (and (vec? pat)
                            (not (cp.dotted? pat)))
                   (count pat))]

             {:name nam
              :doc doc
              :pat pat
              :arity arity
              :body (bindings.bodify body)
              :form form}))

         binding-verb
         ["given compile opts, determine the needed verb for inner bindings"
          (fn [{:keys [unified strict short]}]
            ({[nil nil nil] (qq let)
              [nil true nil] (qq ?let)
              [true nil nil] (qq !let)
              [nil true true] (qq lut)
              [true nil true] (qq !lut)}
             [strict short unified]))]

         compile
         (fn [e opts]
           (let
               [
                (ks arity pat name body unary) opts

                ;; fresh syms for emited lambda's argv
                (& [fs] ss) (take (gensyms) 20)

                ;; if the given binding form (pat) is a vector (not dotted)
                ;; we will compile into fixed arity lambda for performance
                fixed-arity (and (not unary) arity)

                ;; depending on fixed-arity
                ;; destructure given pattern and take as freshsyms as needed
                [pats seeds]
                (if fixed-arity
                  [pat (take ss fixed-arity)]
                  [[pat] [(car ss)]])

                ;; inner binding form
                ;; [pat seed pat2 seed2 ...]
                bs (vec* (interleave pats seeds))

                ;; the binding form of the emitted lambda
                binding-form
                (cond unary [fs]
                      fixed-arity (vec* seeds)
                      :else ['& fs])]

               (exp e
                    (lst (qq primitives.fn) . (if name [name] [])
                         binding-form
                         (lst (binding-verb opts) bs body)))
               ))

         compiler
         (fn [& flags]
           (fn [e form]
             (compile
              e (mrg (flagmap flags)
                     (parse form)))))

         cased
         {parse
          ["cased lambda parser"

           case
           ["parse one case"
            (fn [[pat & body]]
              (let [arity
                    (when (not (cp.dotted? pat))
                      (count pat))

                    [pat-prefix rest-pat]
                    (split-with (complement #{cp.dot cp.dotdot}) pat)

                    rest-pat
                    (if (= 2 (count rest-pat))
                      (second rest-pat) (vec* rest-pat))]

                (merge
                 {:pat pat
                  :body (bindings.bodify body)}
                 (if arity
                   {:arity arity}
                   {:variadic true
                    :pat-prefix (vec* pat-prefix)
                    :rest-pat rest-pat
                    :min-arity (count pat-prefix)}))

                ))]

           check-variadic-sigs
           ["ensure that all variadic cases have the same min arity"
            (fn [xs]
              (assert (apl eq ($ xs #(c/get % :min-arity)))
                      (str* "variadic arities count mismatch\n"
                            (interleave ($ xs #(c/get % :pat)) (repeat "\n")))))]

           ;; main
           (fn [[fst & nxt :as form]]
             #_(pp "cased-lambda-parse" form)
             (let [[name . cases]
                   (if (word? fst)
                     (cons fst nxt)
                     (concat [nil fst] nxt))

                   cases
                   (partition 2 cases)

                   parsed-cases
                   ($ cases parse.case)

                   variadic-cases (seq (filter :variadic parsed-cases))
                   variadic (boolean variadic-cases)
                   monadic (and (not variadic) (apl c/= ($ cases (comp count car))))
                   polyadic (not monadic)

                   arities
                   (reduce
                    (fn [r {:keys [arity min-arity] :as c}]
                      (if arity
                        (update r arity (fnil conj []) c)
                        (update r :& (fnil conj []) c)))
                    {} parsed-cases)]


               (when variadic
                 (parse.check-variadic-sigs (c/get arities :&)))

               {:name name
                :monadic monadic
                :variadic variadic
                :polyadic polyadic
                :arity-map arities
                :cases cases}))]

          compile
          {:val
           (fn [e {:as opts :keys [arity-map name]}]
             (exp e (qq (fn .~(when name [name])
                          .~($ (iter arity-map) (p .arity opts))))))
           arity
           {:val
            (fn [opts [n cases]]
              (let [verb (verb opts cases)]
                (if (num? n)
                  (.fixed verb n cases)
                  (.variadic verb cases))))

            verb
            (fn [{:keys [unified short strict]} cases]
              (cs (not (next cases))
                  (cs (and unified strict) (qq !lut)
                      unified (qq lut)
                      short (qq ?let)
                      strict (qq !let)
                      (qq let))
                  (cs (and unified strict) (qq !clut)
                      unified (qq clut)
                      strict (qq !clet)
                      (qq clet))))

            fixed
            (fn [verb arity cases]
              (let [argv (vec* (take (gensyms) arity))
                    clet-cases
                    (mapcat
                     (fn [{:keys [pat body]}]
                       [(vec* (interleave pat argv)) body])
                     cases)]
                (lst argv (lst* verb clet-cases))))

            variadic
            (fn [verb cases]
              (let [vsym (gensym)
                    prefcnt (c/get (car cases) :min-arity)
                    argv-prefix (take (gensyms) prefcnt)
                    argv (vec+ argv-prefix ['& vsym])
                    clet-cases
                    (mapcat
                     (fn [{:keys [pat-prefix rest-pat body]}]
                       [(vec+ (interleave pat-prefix argv-prefix) [rest-pat vsym])
                        body])
                     cases)]
                (lst argv (lst* verb clet-cases))))}}

          compiler
          (fn [& flags]
            (fn [e form]
              (compile
               e (mrg (flagmap flags)
                      (parse form)))))
          }

         builtins
         [f:mac (compiler)
          !f:mac  (compiler :strict)
          ?f:mac  (compiler :short)
          !fu:mac (compiler :unified :strict)
          fu:mac  (compiler :unified :short)

          f1:mac   (compiler :unary)
          !f1:mac  (compiler :unary :strict)
          ?f1:mac  (compiler :unary :short)
          !fu1:mac (compiler :unary :unified :strict)
          fu1:mac  (compiler :unary :unified :short)

          f_:mac   (f [e xs] (exp e (lst* 'f1 '_ xs)))
          ?f_:mac  (f [e xs] (exp e (lst* '?f1 '_ xs)))
          !f_:mac  (f [e xs] (exp e (lst* '!f1 '_ xs)))
          !fu_:mac (f [e xs] (exp e (lst* '!fu1 '_ xs)))
          fu_:mac  (f [e xs] (exp e (lst* 'fu1 '_ xs)))

          cf:mac   (cased.compiler)
          !cf:mac  (cased.compiler :strict)
          ?cf:mac  (cased.compiler :short)
          cfu:mac  (cased.compiler :unified)
          !cfu:mac (cased.compiler :unified :strict)]}

        (import lambda.builtins :all)
        generic.lambda-wrapper (qq f)

        bindings.let.compile.named
        (fn [e {:as opts :keys [bs name expr]}]
          (lst* (lambda.compile
                 e (mrg opts
                        {:pat (mapv car bs)
                         :arity (count bs)
                         :body expr
                         :name name}))
                (exp e ($ bs second)))))

    (do :guard-macro
        (E+ guard:mac
            (f [e (& form [argv . _])]
               (let [(& parsed (ks pat body)) (lambda.parse form)
                     body (qq (when ~body ~(car pat)))]
                 (lambda.compile e (assoc parsed :body body))))
            guard:fn
            (f [g] (fn& [x] (when (g x ...) x))))

        (check ((guard:fn c/>) 2 1 0)
               ((guard [x y] (c/> x y)) 3 2)))

    (do :invoc-apply-map-walk

        (E+

         callables
         {wrapper:mac
          (f [e [builder]]
             (let [[[a1 [e1]] . cs]
                   (fn&.cases (qq ([x] ((~builder x) ...))))]
               (exp e (qq (fn (~a1 (p ~e1)) . ~cs)))))}

         invocation
         (generic
          [x]
          :fun x
          (k x))

         § (callables.wrapper invocation)

         application
         (generic
          [x]
          :fun (p apl x)
          (p apl (invocation x)))

         * (callables.wrapper application)

         >
         ["thread the first argument thru the given functions
           (> 1 inc inc inc) ;=> 4"
          (f [x . fs]
             ((* + ($ fs §)) x))]

         $
         (generic.reduced
          [x f]
          :map (c/into {} (c/map (fn [[k v]] [k (§ f v)]) x))
          :set (c/set (c/map (§ f) x))
          :vec (c/mapv (§ f) x)
          :lst (c/map (§ f) x)
          :any (error "not mappable" x))

         ;; $ indexed
         $i
         (generic.reduced
          [x f]
          :map (c/into {} (c/map (fn [[k v]] [k (§ f k v)]) x))
          :set (c/set (c/vals ($i (c/zipmap x x) f)))
          :vec (c/vec (c/map-indexed (§ f) x))
          :lst (c/map-indexed (§ f) x)
          :any (error "not mappable" x))

         ;; the following walking function are highly inspired by clojure.walk

         walk
         (f [x in out]
            (if (coll? x)
              (out ($ x in))
              (out x)))

         dfwalk
         ["depth first walk"
          (f
           [x f]
           (walk x #(dfwalk % f) f))]

         bfwalk
         ["breadth first walk"
          (f
           [x f]
           (walk (f x) #(bfwalk % f) id))]

         walk?
         (f
          [x ? f]
          (cs [nxt (? x)]
              ($ nxt #(walk? % ? f))
              (f x))))

        (_ :tries
           (!! ($+.inspect))
           (!! #_($ [1 2 3] inc inc)
               ($+ [[1 2 3][5 6 7]] id range)))

        (E+ (invocation.extend
             [x]
             :vec
             (fn& []
                  (c/mapv *
                          (c/concat x (c/repeat #(c/last %&)))
                          (c/map vector ...)))
             :map
             (fn& []
                  (let [yks (c/set (c/mapcat c/keys [...]))
                        y0 (c/zipmap yks (c/repeat []))
                        y (c/merge-with c/conj y0 ...)
                        x (c/merge (c/zipmap yks (c/repeat #(c/last %&))) x)]
                    (c/merge-with * x y)))
             ;;:key
             ;;(fn& [o] ((get o x) o ...))
             ))

        (_ :tries

           (!! (invocation.inspect))
           (qbench (!! (§ {:a add :b sub} {:a 2 :b 1 :c 56} {:a 3 :b 5})))
           (qbench (!! (§ [add sub add] [1 2 3] [1 2 3] [1 2 3])))
           (qbench (!! (c/mapv (fn [f args] (apl f args))
                               [add sub add]
                               (mapv list [1 2 3] [1 2 3] [1 2 3]))))
           (qbench (!! (* [add sub add] (list [1 2 3] [1 2 3] [1 2 3]))))))

    (E+ subjectify
        {:doc
         "in asparagus, many functions takes what we can call the object as first argument
          I mean, the thing we are working on, for instance, in the expression (assoc mymap :a 1 :b 2), mymap is what we call the object
          the subjectify function will help to turn this kind of function into a one that takes only the arguments (in the previous exemple: :a 1 :b 2)
          and return a function that takes only the target object, and return the result.
          (let [assoc_ (subjectify assoc)
                assoc-a-and-b (assoc_ :a 1 :b 2)]
             (assoc-a-and-b {})) ;=> {:a 1 :b 2}

          many of the asparagus functions of this form, have their subjectified version with the same name suffixed with _
          this is handy, for instance, to create chains of 1 argument functions
          (> myseq (take_ 3) (dropend_ 2)) will thread 'myseq thru 2 functions, the semantics is analog to core/-> but it is a function
          the '> function is defined in the :invocation-application-mapping section (the previous one)
          (>_ (take_ 3) (dropend_ 2)) ;; will return a function that wait for its first argument ('myseq in the previous example)

          the idea behind this is to ease function composition, the preference for guards over predicates is also a step in this direction
          the further 'flow section will introduce some useful functional constructs that go even further (in conjunction with this and guards)"
         :val
         (f1 f
             (fn& [] (f1 s (f s ...))))
         :mac
         (f [e [g]]
            (exp e (qq (fn& [] (f1 s (~g s ...))))))
         definitions:upd
         (f [e xs]
            (zipmap
             ($ xs #(sym % '_))
             ($ xs (p lst (qq subjectify)))))}

        ;; here we are defining subjectified versions of some important functions
        (subjectify.definitions
         take takend drop dropend section
         sip + * § $ $i >
         eq neq gt gte lt lte
         add mul div sub
         walk dfwalk bfwalk walk?))

    (do :$related

        (E+

         zip
         ["core/map(ish)"
          (f
           [f . xs]
           (* c/map f ($ xs iter)))]

         red
         ["reduce with seed (init) as first argument
           and variadic seq(s) argument (like map does)"
          (fn
            ([x f xs]
             (c/reduce (§ f) x xs))
            ([x f y & ys]
             (red x (* f)
                  (* zip vec y ys))
             #_(if (c/every? cons? xs)
                 (* red
                    (* f x ($ xs car))
                    f ($ xs cdr))
                 x)))]

         $+
         ["$+ is to $ what c/mapcat is to c/map"
          (generic.reduced
           [x f]
           :any
           (* + #_(pure x) ($ x f)))]

         $i+
         ["$i+ is to $i what c/mapcat is to c/map"
          (generic.reduced
           [x f]
           :any
           (* + ($i x f)))]

         zip+
         ["core/mapcat(ish)"
          (f
           [f . xs]
           (cs [ret (c/seq (* zip f xs))]
               (* + ret) ()))]

         scan
         ["similar to core/partition"
          (f
           [x size step]
           (let [[pre post] (splat x size)]
             (if (cons? post)
               (cons pre (scan (drop x step) size step))
               (if (cons? pre)
                 (sip (pure x) pre)
                 (pure x)))))]

         chunk
         ["split an iterable 'x by chunk of size 'size"
          (f [x size]
             (scan x size size))]

         nths
         ["like core/take-nths"
          (f [x n]
             ($ (scan x n n) car))]

         braid
         ["like core/interleave"
          (p zip+ (p sip ()))]

         (subjectify.definitions
          red $+ scan chunk nths))

        (check
         ;; $+
         ($+ [5 6 7]
             (p c/range 0)
             (c/juxt id (p c/+ 10)))

         ;; zip
         (eq '(3 6 9)
             (zip c/+ [1 2 3] [1 2 3] [1 2 3]))

         ;; zip+
         (eq [1 1 1 2 2 2 3 3 3]
             (zip+ (p sip []) [1 2 3] [1 2 3] [1 2 3]))

         ;; scan
         (eq [[1 2] [3 4]]
             (scan [1 2 3 4] 2 2))
         (eq [[1 2] [2 3] [3 4]]
             (scan [1 2 3 4] 2 1))
         (eq '((0 1 2 3) (2 3 4))
             (scan (c/range 5) 4 2))

         ;; chunk
         (eq [[1 2] [3]]
             (chunk [1 2 3] 2))
         (eq []
             (chunk [] 2))

         ;; braid
         (eq '(1 4 2 5 3 6)
             (braid [1 2 3] [4 5 6]))
         (eq '(1 4 2 5)
             (braid [1 2 3] [4 5])))

        )

    (E+ df
        ["data function,
          create a function from a data structure that
          apply all functions contained in it (deeply) to further args.
          preserve original structure"

         (f1 data
             (f xs
                (walk? data
                       (f1 node (or (vec? node) (map? node)))
                       (f1 leaf (* leaf xs))))) ;; don't forget that * is application ;)

         :demo
         (__
          ;; you can use vectors and maps to compose the resulting function
          (!! (df [inc
                   dec
                   {:doubled (f_ (mul 2 _))
                    :halfed (f_ (div _ 2))}]))
          ;; <fn>

          ;; invoc it
          (let [f (df [inc dec
                       {:doubled (f_ (mul 2 _))
                        :halfed (f_ (div _ 2))}])]
            (f 1)) ;;=> [2 0 {:doubled 2 :halfed 1/2}]

          ;; is equivalent to write
          ((f1 a [(inc a) (dec a)
                  {:doubled (mul 2 a)
                   :halfed (div a 2)}])
           1)

          ;; any invocable can serve as a leaf
          ;; don't know if you remember, but in asparagus almost everything is invocable,
          ;; in particular constant values like 42 or :foo return themselves
          ;; to demonstrate that df can handle any invocable we will use some of those
          (let [f (df [inc dec :foo 42])]
            (f 1)) ;;=> [2 0 :foo 42]

          ;; can take several arguments
          (let [f (df [add sub])] (f 1 2 3)) ;;=> [6 -4]

          ;; you can deeply mix maps and vecs to compose your function
          (let [f (df {:addsub [add sub]
                       :average (f xs (div (* add xs) (count xs)))})]
            (f 1 2 3))
          ;;=> {:addsub [6 -4], :average 2}

          ;; maybe you are wondering about our vec and map invocation behavior
          ;; this is prevented here because vecs and maps mean something else in this context
          ;; but you can use the § function to state that a leaf that is a map or a vec has to be treated as an invocable
          (let [f (df [concat
                       (§ [add sub mul]) ;; here
                       ])]
            (f [1 2 3] [4 5 6]))
          ;;=> [(1 2 3 4 5 6) [5 -3 18]]

          )])

    (do :compare

        (E+

         eq
         (generic

          ([x] #(eq % x))

          ([x y]

           :map
           (and (map? y)
                (c/= (count y) (count x))
                (loop [[[k1 v1] & es] x]
                  (when (eq (get y k1) v1)
                    (if (c/seq es) (recur es) x))))

           :line
           (and (c/= (type x) (type y))
                (c/= (count y) (count x))
                (c/every? id (c/map eq x y))
                x)

           :nil (nil? y)
           :any (when (c/= x y) x))

          ([x y & xs]
           (* eq (eq x y) xs)))

         neq
         (guard [x y]
                (nil? (eq x y))))

        ;; a major concern about eq is that sets will use core/= instead of it...
        ;; don't know how to change that... (and don't know if I want to)

        (check
         ;; in clojure this would be considered equal
         (nil? (eq [1 2] '(1 2)))
         ;; eq is a guard and return its first arg
         (c/= [1 2] (eq [1 2] [1 2]))
         ;; neq is a guard too
         (c/= [1 2] (neq [1 2] '(1 2)))
         ;; neq failure
         (nil? (neq [1 2] [1 2]))
         ;; variadic arity
         (eq [1 2] [1 2] [1 2])
         ;; nested
         (nil? (eq {:a [{}]} {:a (lst {})}))
         (eq {:a [{}]} {:a [{}]})
         (nil? (eq {:a [{}]} {:a [{}] :b 1}))
         ))

    (do :linear-accesses

        (E+ linear-accesses
            {mk
             (f1 size
                 (loop [s size ret [['a 'd]]]
                   (if (eq 1 s)
                     (* + (cdr ret))
                     (recur (dec s)
                            (sip ret
                                 ($+ (last ret)
                                     #($ ['a 'd] (p + %))))))))
             definitions:upd
             (f [e _]
                ($ (linear-accesses.mk 5)
                   (f_ {(sym 'c _ 'r)
                        (qq (+ . ~($ (> _ iter rev)
                                     (p c/get {\a (qq car) \d (qq cdr)}))))})))}

            (linear-accesses.definitions))

        (check
         (eq :io
             (cadr [1 :io])
             (caddr [1 2 :io])
             (caadr [1 [:io 2] 3])
             (cadadr [1 [2 :io]]))))

    (do :dive

        (E+ dive

            [

             "
             the dive generic function, let you get something inside something else
             its first argument represent the address of what you want to get
             the second is the thing in which you want to find it

             it is like core/get but with arguments reversed, and being a generic function, it can be extended.

             Also, we can mention that it is a concrete exemple of something that is a function and a macro at the same time
             here we use a technique that is analog to the one we used in bind (binding operators)
             the dive module holds a map of operations implementations in dive.ops
             At expansion time, if the first argument to dive is an sexpr, the verb will be searched in dive.ops
             if an implementation is found, it will be executed (at expansion time) and the return value will take the place of the original expression

             as an exemple, we use the 'ks operation
             (dive (ks a b) {:a 1 :b 2 :c 2})
             ks is resolved in dive.ops and applied to the given args (here :a and :b), producing this form
             (dive (fn [y] (select-keys y [:a :b]))
                   {:a 1 :b 2 :c 2})

             functions implement dive so the expansion time work is done, the form will now ready for runtime

             As you may have deduced by yourself, dive.ops can be extended with new operations
             Keep in mind that it will not alter all previous call to dive, which are already compiled. (this is a good thing :))"

             (generic
              [x y]

              ;; vec perform a deep dive, analog to what core/get-in do,
              ;; but can be populated with anything that is a valid dive 1st argument
              :vec
              (c/reduce #(dive %2 %1) y x)

              ;; symbols and keywords performs a simple get
              #{:key :sym}
              (c/when (hash? y)
                (c/get y x))

              ;; numbers do an index search, supporting negative indexes (indexing from the end of a sequential datastructure)
              :num
              (gat y x)

              ;; maps let you group several dive together
              ;; (dive {:one diving-address1 :two diving-address2} diving-target)
              ;; => {:one wathever-was-at-address1 :two wathever-was-at-address1}
              :map
              ($ x #(dive % y))

              ;; sets behave like maps
              ;; #{diving-address1 diving-address2}
              ;; <=> {diving-address1 diving-address1
              ;;      diving-address2 diving-address2}
              :set
              (dive (+ {} ($ (iter x) #(vec % %))) y)

              ;; function simply applied themselves to the diving-target
              :fun (§ x y)

              ;; nil do nothing, it returns the diving-target unchanged
              :nil y)]

            dive
            {:mac
             (f [e [x y]]
                (exp e
                     (lst* (qq dive:val)
                           (clet [[(:sym v) . args] (lst? x)
                                  impl (get .ops (key v))]
                                 [(impl args) y]
                                 [x y]))))
             ops:val
             {:ks
              (f [xs]
                 (qq (fn [y] (select-keys y ~(vec* ($ xs key))))))}

             op+:upd
             (f [e [name argv expr]]
                ['dive.ops:val
                 {(key name)
                  (qq (f1 ~argv ~expr))}])
             }))

    (do :flow

        (E+

         ;; guard based constructs
         ;; guard composition, filtering, control flow etc...

         $?
         ["check if all values of a datastructure are not nil (see 'vals)"
          (guard [x] (c/every? id (vals x)))]

         ?$
         ["like $ but succeed only if the result does pass $?"
          (+ $ $?)]

         ?zip
         ["?zip is to zip what ?$ is to $"
          (+ zip $?)]

         :fx
         (check
          ;; this fails because it contains nil
          (nil? (?$ [1 2 nil]))
          ;; fails because -1 is not pos?
          (nil? (?$ [1 2 -1] pos?))
          ;; like $ it can take several functions
          ;; thread subject checking at each step for nils
          (?$ [1 2 0] inc pos?)
          ;; maps: checks values
          (?$ {:a 1 :b 2})
          ;; fails
          (nil? (?$ {:a 1 :b nil}))
          ;; ?$ is an error for non coll subject
          (nil? (comment :err (?$ 1)))
          ;; ?zip
          (?zip #(pos? (add %1 %2)) [1 2 3] [1 2 3])
          (nil? (?zip #(pos? (add %1 %2)) [1 2 3] [1 2 -3])))

         ;; help to define 'filt and 'rem
         shrink
         (f [x g keep?]
            (red (pure x)
                 (f [x v e]
                    (if (keep? (§ g v)) (sip x e) x))
                 (vals x) x))

         filt
         ["is to $ what core/filter is to core/map"
          (generic.reduced [x f] :any (shrink x f id))]

         rem
         ["is to $ what core/remove is to core/map"
          (generic.reduced [x f] :any (shrink x f not))]

         :fx
         (check
          (eq [1 2 3]  (filt [1 2 -1 -2 3] pos?))
          (eq [-1 -2] (rem [1 2 -1 -2 3] pos?)))

         ?$i
         ["like ?$ but the given functions will receive indexes as well as values"
          (+ $i $?)]

         :fx
         (check
          (eq [1 2 3] (filt [1 2 -1 -2 3] pos?))
          (eq [-1 -2] (rem [1 2 -1 -2 3] pos?))
          ;; success because no element is equal to its idx
          (?$i [1 2 3] neq)
          ;; fail because 2 element is equal to its idx
          (nil? (?$i [1 1 3] neq)))

         ?deep
         ["the deep version of ?$"
          (guard
           [x]
           (if (coll? x)
             (?$ x ?deep)
             x))]

         :fx
         (check
          (nil? (?deep {:a {:b 1 :c [1 2 nil]}}))
          (nil? (?deep {:a {:b 1 :c [1 2 3 {:d nil}]}}))
          ;; succeed
          (?deep {:a {:b 1 :c [1 2 3]}}))

         ?>
         ["thread x thru guards shorting on first nil resul"
          (f
           [x f . fs]
           (?let [x (c/and x (§ f x))]
                 (if (cons? fs) (* ?> x fs) x)))]

         ?<
         ["trying all given guards against x until first non nil result"
          (f
           [x f . fs]
           (c/or (§ f x)
                 (c/when (cons? fs) (* ?< x fs))))]

         (subjectify.definitions
          ?$ ?$i filt rem ?> ?<)

         :fx
         (check
          ;; success
          (eq 1 (?> 1 num? pos?))
          ;; failure
          (nil? (?> 1 num? neg?))
          ;; shorts after str? (else it would be an error)
          (nil? (?> 1 str? (p + "aze")))
          ;; more exemples
          (eq 3 (?> [1 2 3] (guard:fn (+ c/count c/odd?)) last))
          (nil? (?> [1 2] (guard [x] ((+ c/count c/odd?) x)) last))
          ;; more composed exemple
          ;; use § under the hood,
          ;; applicable data structure can be used
          (eq {:-a 3 :a -3}
              (?> -1
                  num?
                  (c/juxt (p add -2) (?>_ (p add 2) pos?))
                  car
                  neg?
                  #(do {:a % :-a (mul % -1)})
                  #_{:-a pos?} ;; no map application for now
                  ))

          ;; build a guard
          ;; that succeed for numbers or strings
          (let [f (?<_ num? str?)]
            (eq [1 "a" nil]
                [(f 1) (f "a") (f :a)]))

          ;; basic composition with ?< and ?>_
          (eq 42
              (?< 44
                  str?
                  (?>_ num? (gt_ 10) dec dec)))

          )

         ?c>
         ["a scheme-cond(ish) function"
          (f
           [x . bs]
           (* ?< x ($ bs (* ?>_))))]

         ?c
         ["a clojure-cond(ish) function"
          (f
           [x . cs]
           (* ?c> x (chunk cs 2)))]

         ?><
         ["at least one guard of each branch have to succeed
           last branch's first success returned.
           this one is a bit convoluted... maybe not so useful"
          (f
           [x . bs]
           (* ?> x ($ bs (* ?<_))))]

         (subjectify.definitions
          ?c> ?c ?><)

         :fx
         (check

          (eq 2
              (?c 1
                  ;; like clojure cond
                  ;; works by couples
                  str? :pouet ;; if str? succeeds, :pouet is called
                  pos? inc
                  neg? dec))

          (eq 10
              (?c 10
                  num? (lt_ 3) ;; if the second pred fail, we go to next couple
                  num? (gt_ 7) ;; this line succeed
                  ))

          ;; (non function values act as constant functions)
          (eq :pouet
              (?c "a"
                  str? :pouet
                  pos? inc
                  neg? dec))

          ;; same with ?c_
          (eq -2
              ((?c_
                str? :pouet
                pos? inc
                neg? dec)
               -1))

          (eq -8
              (?c> -2
                   ;; like scheme cond
                   ;; several vecs of guards
                   [str? :pouet]
                   [pos? inc inc inc]
                   [neg? dec dec (p mul 2)]))

          (?c> 1
               ;; here too, if the line does not succeed entirely,
               ;; skip to the next line
               [pos? dec pos? :gt1]
               [pos? :1])

          (eq 5
              ((?c>_
                [str? :pouet]
                [pos? inc inc inc]
                [neg? dec dec (p mul 2)])
               2))
          )))

    (do :mlet-mac-at

        (E+ mlet
            ["let you bind local macros"
             :mac
             (fn [e [ms & bod]]
               (let [local-path #(path (loc e) % :mac)
                     ;; this should be abstracted (local env extension)
                     e (red e (f [e [s x]] (env-add-member e (local-path s) (eval e x)))
                            (chunk ms 2))]
                 (exp e (bindings.bodify bod))))])

        (_ :tries
           (!! (mlet [fi (fn [e [p f t]] (exp e (lst 'if p t f)))]
                     (fi (vec? [])
                         :novec
                         ;; this one is just to check expansion occurs well
                         (lut [a 1 a 1] :vec))))

           (exp @E '(mlet [fi (fn [e [p f t]] (exp e (lst 'if p t f)))]
                          (fi (vec? []) :novec (lut [a 1 a 2] :vec)))))

        (E+ let-syms
            ["let you introduce a bunch of gensyms easily
              (let-syms [a b c] (list a b c))
              ;=> (G__198743 G__198744 G__198745)"
             :mac
             (fn [e [xs & bod]]
               (exp e (qq (let [~xs (gensyms)] . ~bod))))])

        (E+ mac
            ["let you define a macro that behave as regular lisps ones do
              e.g: no need to manually thread expansion"
             :mac
             (f [e [pat . body]]
                (exp e (qq (f [e' ~pat] (exp e' ~(lst* 'do body))))))])

        (_ :tries
           (exp @E '(mac [p f t] (lst 'if p t f)))
           (E+ fi:mac (mac [p f t] (lst 'if p t f)))
           (exp @E '(fi true (lut [a 1 a 2] :pouet) (fn& [] (add ...)))))

        (E+ at
            ["like dive but with arguments reversed"
             :mac (mac [x y] (lst 'dive y x))]

            at_
            ["subjectified version of at"
             :mac (mac [x] (qq (f_ (at _ ~x))))])

        (_ :tries
           (!! (at {:a 1 :b 2 :c 2} (ks :a :b)))
           (!! ((at_ (ks :a :b)) {:a 1 :b 2 :c 2}))))

    (E+ bindings.case
        ["the case form and its variants casu, !case, and !casu"

         form
         (f [verb symseed xs]
            (if (even? (count xs))
              (lst verb .(* + ($ (chunk xs 2) (f1 [p e] [[p symseed] e]))))
              (rec verb symseed [.(butlast xs) '_ (last xs)])))

         verb
         (f1 (ks strict unified)
             (cs (and strict unified) (qq !clut)
                 strict (qq !clet)
                 unified (qq clut)
                 (qq clet)))

         compile
         (f [e [seed . xs] opts]
            (let-syms [symseed]
              (exp e
                   (qq (c/let ~[symseed seed]
                         ~(form (verb opts) symseed xs))))))

         def:upd
         (f [_e [name . flags]]
            (let [mac-sym (sym name :mac)
                  smac-sym (sym name "_" :mac)]
              {mac-sym
               (qq (f [e bod]
                      (compile e bod ~(flagmap . flags))))
               smac-sym
               (qq (f [e xs]
                      (exp e (lst (qq f_) (lst* '~name '_ xs)))))
               }))

         builtins
         [(case.def case)
          (case.def casu :unified)
          (case.def !case :strict)
          (case.def !casu :unified :strict)]]

        (import bindings.case.builtins
                [case casu !case !casu
                 case_ casu_ !case_ !casu_])

        :fx
        (check

         (let [t (case_
                  [:point x 0] :y0
                  [:point 0 y] :x0
                  [:point (:num x) (:num y)] [x y]
                  :pouet)]
           (and
            (eq :y0 (t [:point 1 0]))
            (eq :x0 (t [:point 0 1]))
            (eq [1 2] (t [:point 1 2]))
            (eq :pouet (t [:point 1 "io"]))))

         (let [t (casu_
                  [:point x 0] :y0
                  [:point 0 y] :x0
                  [:point (:num x) (:num x)] :twin
                  [:point (:num x) (:num y)] [x y]
                  :pouet)]
           (and
            (eq :y0 (t [:point 1 0]))
            (eq :x0 (t [:point 0 1]))
            (eq :twin (t [:point 1 1]))
            (eq [1 2] (t [:point 1 2]))
            (eq :pouet (t [:point 1 "io"]))))

         (let [t (!casu_
                  [:point x 0] :y0
                  [:point 0 y] :x0
                  [:point (:num x) (:num x)] :twin
                  [:point (:num x) (:num y)] [x y])]
           (and
            (eq :y0 (t [:point 1 0]))
            (eq :x0 (t [:point 0 1]))
            (eq :twin (t [:point 1 1]))
            (eq [1 2] (t [:point 1 2]))
            (throws (t [:point 1 "io"]))))
         ))

    (E+ tack

        [
         "not intended to be used directly
          prefer using put and upd
          semantically similar to assoc with different arg order
          like in dive the first argument is the address (and is used to dispatch)"

         (generic [k x v]

                  :vec
                  (clet [[k . _ks] k]
                        (tack k x
                              (tack  _ks (dive k x) v))
                        v)

                  :num
                  (when (assert (or (line? x) (nil? x)))

                    (let [cnt (c/count x)
                          negidx (and (neg? k) (c/- k))
                          inbound (gt cnt (or negidx k))]

                      (cs

                       inbound
                       (cs negidx

                           ;; I could recur here but, take is low level, had to check perfs
                           (let [idx (sub cnt negidx)]
                             (cp x
                                 seq? (c/concat (take x idx) (cons v (drop x (inc idx))))
                                 vec? (c/assoc x idx v)))

                           (cp x
                               seq? (c/concat (take x k) (cons v (drop x (inc k))))
                               vec? (c/assoc x k v)))

                       (nil? x)
                       (sip (vec* (c/repeat k nil)) v)

                       ;; we fill the missing idxs with nils
                       (+ x (tack (sub k (c/count x)) nil v)))))

                  :any
                  (when (assert (or (map? x) (c/record? x) (nil? x)))
                    (c/assoc (or x {}) k v)))]

        tack
        {:mac
         (f [e [x y z]]
            (exp e
                 (lst* (qq tack:val)
                       (clet [[(:sym v) . args] (seq? x)
                              impl (get .ops (key v))]
                             [(impl args) y z]
                             [x y z]))))

         ops:val {}

         op+:upd
         (f [e [name argv expr]]
            ['tack.ops:val
             {(key name)
              (qq (f1 ~argv ~expr))}])}

        put
        ["analog to assoc, but uses 'tack"
         (f [x . xs]
            (red x
                 (f [x [k v]] (tack k x v))
                 (chunk xs 2)))]

        upd
        ["analog to update but uses 'tack"
         (f [x . xs]
            (red x
                 (f [x [k f]]
                    (tack k x (f (dive k x))))
                 (chunk xs 2)))]

        (subjectify.definitions put upd)

        :fx
        (check

         ;; tack

         (eq [[[nil nil 42]]]
             (tack [0 0 2] nil 42))

         (eq {:a {:b 1}}
             (tack [:a :b] {} 1))

         (tack :a {:a 1} 2)

         ;; put

         (eq {:a 1}
             (put nil :a 1))

         (eq {:a {:b 1}}
             (put {} [:a :b] 1))

         (eq [[[nil nil 42]]]
             (put nil [0 0 2] 42))

         (eq [nil [nil nil [1]] nil 89]
             (put [] [1 2 0] 1 3 89))

         (eq {:a {:b 1, :p {:l [0 1 2]}}}
             (put {} [:a :b] 1 [:a :p :l] [0 1 2]))

         ;; upd

         (eq [0 [1 1]]
             (upd [0 [0 1]] [1 0] inc))

         (eq {:a {:b [0 2 2], :c {:d 42}}}
             (upd {:a {:b [0 1 2]}}
                  [:a :b 1] inc
                  [:a :c :d] (k 42)))

         ))

    (E+ obj+
        ["an update to declare a new object"

         :upd
         (cf

          [e [(& (ks name fields)
                 (ks-or parents []
                        impls []
                        proto {}
                        class-sym (sym (str/capitalize (c/name name)))))]]

          (let [name-sym (sym name)
                map-constructor-sym (sym "map->" name-sym)
                guard-sym (sym name-sym "?")
                proto-sym (sym name-sym ".proto:val")
                proto-val
                (red proto
                     (f [p x]
                        (clet [path (path (sym x ".proto:val"))
                               [at v] (env-relfind e path)]
                              (+ p v)
                              p))
                     parents)]

            (pp 'obj+ name fields parents impls proto class-sym)

            [ ;; clojure side effects
             :clj (qq (t/type+
                       {:tag ~name
                        :fields ~fields
                        :parents ~parents
                                        ;:class-sym ~class-sym
                        }))
             ;; constructors (positional and from hashmap)
             name-sym (qq (f ~fields (merge (~(sym "->" class-sym) .~fields) ~proto-sym)))
             proto-sym proto-val
             map-constructor-sym (qq (f_ (~(sym "map->" class-sym) (merge ~proto-sym _))))
             guard-sym (qq (f_ (and (instance? ~class-sym _) _)))

             ;; generic implementations
             (sq (generic.type+ ~name (type [x] ~name) .~impls))])

          ;; extra signatures

          [e (tup name fields (:map proto))]
          (rec e [name fields [] proto []])

          [e (tup name fields (:map proto) impls)]
          (rec e [name fields [] proto impls])

          [e (tup name fields (:vec parents) (:vec impls))]
          (rec e [name fields parents {} []])

          [e (tup name fields (:vec parents) (:map proto))]
          (rec e [name fields parents proto []])

          [e [name fields parents proto impls]]
          (rec e [{:name name
                   :fields fields
                   :parents parents
                   :proto proto
                   :impls impls}]))]

        type+

        ["an update to declare a new type"

         :upd
         (cf [e (tup name fields)]
             (obj+:upd e [{:name name :fields fields}])

             [e (tup name fields (:lst impl1))]
             (obj+:upd e [{:name name :fields fields :impls [impl1]}])

             [e [name fields (:lst impl1) . _impls]]
             (obj+:upd e [{:name name :fields fields :impls (cons impl1 _impls)}])

             [e [name fields (:vec parents) . _impls]]
             (obj+:upd e [{:name name :fields fields :impls _impls :parents parents}]))

         :demo
         (__

          (ppenv type+)
          ;; definition
          (E+ (type+ :fut [bar baz]
                     (+ [(ks bar baz) (& {:bar !barb :baz bazb} (:fut b))]
                        (do (pp "here")
                            (fut (+ bar !barb)
                                 (+ baz bazb))))))

          (!! (+.inspect))

          ;; instantiation
          (!! (fut 1 2))
          (!! (map->fut {:bar 1 :baz 2}))

          ;; guard
          (!! (fut? (fut 1 2))) ;;=> (fut 1 2)

          ;; type
          (!! (type (fut 1 2))) ;;=> :fut

          ;; using generic implmentations
          (!! (+ (fut 1 2) (fut 1 2) )))]

        )

    (pp 'will-declare-topforms)

    (init-top-forms
     let lut ?let !let !lut
     ?f !f f fu !fu
     f1 !f1 ?f1 !fu1 fu1
     f_ !f_ ?f_ !fu_ fu_
     cf ?cf !cf cfu !cfu
     clet clut !clet !clut
     case casu !case !casu
     throws
     exp sq qq qq!)

    (pp 'DONE)

    )
