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

(when-let [! (c/resolve 'rEset!)] (!))

;; env --------------

(do :path

    (defrecord Path [rel xs mkey])

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
          [x]
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
          "make a path relative"
          [p]
          (cs [p (path p)]
              (update p :rel #(or % 0))))

        (defn apath
          "make a path absolute"
          [p]
          (cs [p (path p)]
              (assoc p :rel nil)))

        (defn ppath
          "make a path primary"
          [p]
          (cs [p (path p)]
              (assoc p :mkey nil)))

        (_ asserts

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

            (path '..a '.b '.c '[.d e])

            (path '.. '[.a b c d e])

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
            (path sym0 '..d.e))

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
          [x] (cs (path? x) (.rel x)))
        (defn apath?
          [x] (cs (path? x) (not (.rel x))))
        (defn mpath?
          [x] (cs (path? x) (.mkey x)))
        (defn ppath?
          [x] (cs (path? x) (cs (not (.mkey x)) x)))
        (defn dotpath?
          [x] (cs (rpath? x)
                  (and (not (mpath? x))
                       (empty? (.xs x))))))

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

        (path-diff (path 'a.b.c) (path 'a))

        (defn pure-mpath? [p]
          (and (mpath? p)
               (not (rpath? p))
               (not (seq (.xs p)))))

        (asserts
         (parent-path (path 'a.b))
         (parent-paths (path '..a.b.c:val))
         (bubbling-paths (path '..a.b.c:val))
         (parent-path? (path 'a.b) (path 'a.b:val))
         (parent-path? (path 'a.b) (path 'a.b.c))
         (nil? (parent-path? (path 'a.b) (path 'a.b)))
         (nil? (parent-path? (path 'a.b) (path 'a:val))))))

(do :env

    (def echeck? (atom true))

    (defrecord Env [at members])

    (do :base

        (defn env?
          [x] (when (instance? Env x) x))

        (def env0
          (Env. root-path
                {:links {'_ root-path}}))

        #_(def ROOT_PREFIX 'ROOT)

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
        ;; and throw if @echeck? is true and first arg is not a thing
        ;; maybe just a dev purpose device, if @tcheck? is false it compile to regular clojure lambda

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

        (defne root
          [x] (assoc x :at root-path))

        (defne cd
          [x & xs]
          (assoc x :at (path* xs)))

        (defne mv
          [x & xs]
          (update x :at #(path* % xs))))

    (do :getters

        (defne loc
          [e] (.at e))

        (defne root?
          [e] (= root-path (loc e)))

        (defne env-get [e p]
          (cs (apath? p)
              (get-in (.members e)
                      (path-segments p))))

        (defne env-absfind [e p]
          (cs [found (env-get e p)]
              [p found]))

        (defne env-relfind [e p]
          (env-absfind e (path (loc e) p)))

        (defne linked-path [e p]
          (cs [? (apath? p)
               xs (.xs p)
               pref (car xs)
               _xs (rest xs)

               #_[pref & _xs] #_(.xs p)
               target (get (env-get e (path (loc e) :links)) pref)]
              (path (path* target _xs) (.mkey p))))

        #_(linked-path2 e1' (path 'ab))

        (defne env-linkedfind [e p]
          (cs [p' (linked-path e p)]
              (env-absfind e p')))

        (defne env-find [e p]
          (or (env-relfind e p)
              (env-linkedfind e p)))

        #_(defn root-pathsym? [p]
          (= ROOT_PREFIX (path-prefix p)))

        (defne bubfind [e p]
          (cs

           (dotpath? p) nil

           #_(root-pathsym? p)
           #_(env-absfind e (path-unprefix p))

           [plvl (.rel p) ;; p is relative
            loc' (nth-parent-path (ppath (loc e)) plvl)]
           (env-find (cd e loc') (apath p))

           [ret (env-find e p)] ret

           (not (root? e))
           (bubfind (mv e '..) p)))

        (defne qualsym [e s]
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

    (def E (atom env0))

    ;; if this flag is on
    ;; all defined env members will be assigned to vars
    ;; resulting in much better performances (2 OoM)
    (def varmode (atom true))

    ;; holds env members-symbols that can be used at ns level
    (def top-forms (atom #{}))

    (do :vars

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

        (defn clean-pathvars! []
          (doseq [s (shrink+ (keys (ns-publics *ns*)) pathvar-sym?)]
            (ns-unmap *ns* s)))

        (defn clean-top-forms! []
          (doseq [s @top-forms]
            (ns-unmap *ns* s))
          (reset! top-forms #{}))

        (defn clean-Evars! []
          (clean-pathvars!)
          (clean-top-forms!))))

(do :global-env

    (defn rEset! []
      (reset! E env0)
      (clean-Evars!))

    (defn unbound-error [p]
      (throw (ex-info (str "Unbound path: " p)
                      {:type :unbound
                       :path p})))

    (defn unfound-error [p]
      (throw (ex-info (str "Unfound path: " (path->str p))
                      {:type :unfound
                       :path p})))

    (defn env-access
      ([p] (env-access @E p))
      ([e p]
       (cs [found (env-get e p)]
           (cs (= ::unbound found)
               (unbound-error p)
               found)
           (unfound-error p))))

    (defn env-access? [x]
      (and (seq? x)
           (or (indexof x '`env-access)
               (indexof x `env-access))))

    )

(do :steps

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

      [e x]

      (cp x

          sym?
          (or (qualsym e x) x)

          seq?
          (cs [p (-> x car path)
               ? (or (ppath? p) (macpath? p))
               p (qualsym e (path p :mac))]
              (mark-exp (cons p (cdr x)))
              ($ x (p qualify e)))

          holycoll?
          ($ x (p qualify e))

          x))
    #_(defne expand
      [e x]
      #_(pp 'expand x)
      (cp x
          ;env-access? x
          subpath? ((env-access e x) e)
          ;path? x
          mcall? ((env-access e (car x)) e (cdr x))
          holycoll? ($ x (p expand e))
          x))
    (defne expand
      [e x]
      #_(pp 'expand x)
      (cp x
          subpath? (expand-subpath e x)
          mcall? (expand-mcall e x)
          holycoll? ($ x (p expand e))
          x))

    (defne resolve
      [e x]
      #_(pp 'resolve x (path? x))
      (cp x
          env-access? x ;(prob 'env-access x)
          path? (if @varmode (path->varsym x) `(env-access ~x))
          holycoll? ($ x (p resolve e))
          x))

    (defne exp
      [e x]
      (let [e (update e :exp-ctx (fnil conj []) x)]
        (->> x (qualify e) (expand e))))

    (defne res
      [e x]
      (->> x (exp e) (resolve e)))

    (defne eval [e x]
      (c/eval (res e x)))

    )

(do :extension

    (do :transformations

        (defn env-member-path [p]
          (cons :members (path-segments p)))

        (defn env-meta-path [p]
          (cons :meta (path-segments p)))

        (defn env-add-member [e p x]
          (update-in e (env-member-path p) deep-merge x))

        (defn env-merge-members [e xs]
          (reduce (p* env-add-member) e xs))

        (defn env-add-meta [e p x]
          #_(pp 'add-meta p x)
          #_(update-in e (env-meta-path p) deep-merge x)
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
            #_(ns-publics *ns*) #_(rEset!)
            #_(c/resolve (path->varsym (path 'bindings.bind:val)))
            #_(pp 'declaring p (path->varsym p) (not known?) (mpath? p))
            (when @varmode (init-pathvar! p))
            (if (and (not known?) (mpath? p))
              (env-add-member e p ::unbound)
              e)))

        (defn env-detailed-steps [e expr]
          (let [at (loc e)
                qualified (qualify e expr)
                ;expanded (expand e qualified)
                expanded (exp e expr)
                resolved (resolve e expanded)]
            {:at at
             :expr expr
             :qualified qualified
             :expanded expanded
             :resolved resolved}))

        (defn env-member-add-compiled [e p x]
          (let [{:as m r :resolved at :at}
                (env-detailed-steps (mv e p) x)
                evaluated (c/eval r)]
            #_(pp 'compiled-add at (mv e at) r)
            (when @varmode (deep-merge-pathvar! p r)
                  #_(set-pathvar! p r))
            (-> e
                (env-add-member at evaluated)
                (env-add-meta at m)))))

    (do :updates

        (defn env-upd_prepend-declarations [u]
          #_(pp 'will-prep-decl u)
          (let [ps
                (-> (shrink+ u #(= :def (car %)))
                    ($ #(vector :declare (second %)))
                    (shrink- (set u)))]
            (doall (concat ps u))))

        (defn env-upd_sort [u]
          #_(pp 'will-sort-u u)
          (let [filtype
                (fn [t] (shrink+ u #(= t (car %))))]
            (doall (mapcat filtype [:link :declare :fx :def]))))

        (defn env-upd_upd-expr? [e x]
          (cs [? (seq? x)
               p (path (car x) :upd)]
              (bubfind e (path (car x) :upd))))

        (_ :old-impl

           (defn env-upd_split [[x1 x2 & rs :as xs]]
             (cs (not (seq xs)) []
                 (word? x1) (cons {x1 x2} (env-upd_split rs))
                 (map? x1) (cons x1 (env-upd_split (rest xs)))
                 (cons {root-path x1} (env-upd_split (rest xs)))))

           (defn env-upd_path-map
             ([x]
              (env-upd_path-map x root-path))
             ([x from]
              (if (and (holymap? x) (ppath? from))
                (mapcat (fn [[k v]] (env-upd_path-map v (path from k))) x)
                [[from x]])))

           (declare env-upds)

           (defn env-upd_map->upds [e m]
             (let [epath (p path (loc e))]
               (->> (env-upd_path-map m)
                    (mapcat
                     (fn [[p x]]
                       (cs (mpath? p)
                           [(condp = (.mkey p)
                              :links [:link (epath (ppath p)) x]
                              :fx [:fx (epath (ppath p)) x]
                              [:def (epath p) x])]
                           (vec? x)
                           (env-upds (mv e p) x)
                           [[_ updf] (env-upd_upd-expr? (mv e p) x)]
                           (cs (= ::unbound updf)
                               (error "upd ::unbound error")
                               (env-upds (mv e p) (updf (mv e p) (cdr x))))
                           [[:def (epath p :val) x]])))
                    doall
                    env-upd_prepend-declarations
                    env-upd_sort)))

           (defn env-upds [e x]
             #_(pp 'env-upds e x)
             (cp x
                 holymap?
                 (env-upd_map->upds e x)
                 seq?
                 (env-upd_map->upds e {root-path x})
                 vec?
                 (doall (mapcat (p env-upds e) (env-upd_split x))))))

        (do :new-impl

            "the reason is to allow upds to be defined in sequential
             updates (vecs) and available immediately after"

            (defn env-upd_split [[x1 x2 & rs :as xs]]
              (cs (not (seq xs)) []
                  (word? x1)
                  (cs (and (vec? x2) (ppath? (path x1)))
                      (concat ($ (env-upd_split x2) (p hash-map x1))
                              (env-upd_split rs))
                      (cons {x1 x2} (env-upd_split rs)))
                  (holymap? x1) (cons x1 (env-upd_split (rest xs)))
                  (cons {root-path x1} (env-upd_split (rest xs)))))

            (defn env-upds
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
                       (let [[_ updf] (env-upd_upd-expr? (mv e from) x)]
                         (env-upds e (updf (mv e from) (cdr x)) from))

                       [[:def (path from :val) x]])

                   [epath (p path (loc e))]
                   [(condp = (.mkey from)
                      :links [:link (epath (ppath from)) x]
                      :fx [:fx (epath (ppath from)) x]
                      [:def (epath from) x])])))

            (_ :tries

               (env-upds2
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

            )

        (defn env-upd_exe [e u]
          (doseq [[verb at x] u]
            (swap! E
                   (fn [e]
                     (try
                       (condp = verb
                         :link (env-add-member e (path at :links) x)
                         :declare (env-declare-member e at)
                         :fx (do (eval (mv e at) x) e)
                         :def (env-member-add-compiled e at x))
                       (catch Exception err
                         (error "\nenv-upd error compiling:\n"
                                (pretty-str [verb at x])
                                "\n" (.getMessage err)))))))
          @E)

        )

    )

(do :API

    (defmacro E+ [& xs]
      `(do #_(pp '~(first xs))
           ~@(map (fn [u]
                      `(env-upd_exe @E (env-upds @E '~u)))
                    (env-upd_split xs))))

    (defmacro !! [x]
      (res @E x))

    (defn env-inspect [s]
      (let [[p x] (bubfind @E (path s))]
        (println "at: " p)
        (println "\ndata:")
        (pp x)
        (println "\nmeta:")
        (pp (meta x))
        (println)))

    (defmacro ppenv [s]
      `(env-inspect '~s))

    (_ :tries

       (env-upds
        @E '{a {:links {m m1} b 1} g 2
             h:mac (fn [e x] :yop)
             m1 (mod :fx (pp 'hey) i 42)})

       (rEset!)

       (mx' (E+ a 1
                mod:upd (c/fn [e xs] ($keys (apl hash-map xs) path))
                {a {:links {m m1} b 1} g 2
                 h:mac (fn [e x] :yop)
                 m1 (mod :fx (pp 'hey) i 42)}))

       (rEset!)

       (do

         (E+ a :aval)

         (!! (c/= a :aval))

         (E+ a.b 1)

         (E+ c (+ a.b 1))

         (E+ d {e (+ a.b 1)
                f (- c c a.b)} )

         (E+ c:sub (fn [_] 42))

         (E+ cc c)

         (E+ d :dval)

         (E+ d {t:val 45
                e:sub (fn [_] :desub)})

         (E+ {m1 (fn [] ..m2)
              m2 (fn [] ..m1)})

         (E+ foo 1
             {bar (+ foo 1)
              math {bardd (fn [x] (+ bar x))}}
             bardd1 (math.bardd 1))

         )

       #_(pp @E)

       #_(car (env-history @E)))

    (defn top-form-decl [s]
      (p/assert (env-find @E (path s))
                "unknown path")
      `(defmacro ~s [~'& xs#]
         (res @E (list* '~s xs#))))

    (defmacro init-top-forms [& xs]
      (swap! top-forms into xs)
      `(do ~@($ xs top-form-decl))))

(do :quoting

    (defn simple-quotf
      "simple quote function,
       unquote only"
      [form]
      (cp form
          unquote? (second form)
          seq? (cons `list ($ form simple-quotf))
          holycoll? ($ form simple-quotf)
          (list 'quote form)))

    (defn env-simple-quotf
      "env aware version of simplest-quote"
      [e form]
      (cp form
          unquote? (exp e (second form))
          seq? (cons `list ($ form (p env-simple-quotf e)))
          holycoll? ($ form (p env-simple-quotf e))
          (list 'quote form)))

    (do :quote-unquote-xp
        (defn quote-unquote? [x]
          (and (quote? x) (unquote? (second x))))

        (defn env-simple-quotf2
          "env aware version of simplest-quote"
          [e form]
          (cp form
              unquote? (exp e (second form))
              quote-unquote? (list 'quote (->> form second second (exp e)))
              seq? (cons `list ($ form (p env-simple-quotf2 e)))
              holycoll? ($ form (p env-simple-quotf2 e))
              (list 'quote form))))

    (defn env-quotf
      "@bbloom/backtic, env aware version"
      [e form]
      (cp form
          unquote? (exp e (second form))
          unquote-splicing? (error "splice not in list")
          holycoll?
          (let [xs (if (map? form) (cat* form) form)
                parts (for [x xs]
                        (if (unquote-splicing? x)
                          (exp e (second x))
                          [(env-quotf e x)]))
                cat (doall `(concat ~@parts))]
            (cp form
                vec? `(vec ~cat)
                map? `(apply hash-map ~cat)
                set? `(set ~cat)
                seq? `(list* ~cat)
                (error "Unknown collection type")))
          (list 'quote form)))

    (env-quotf @E '(1 2 3)))

(do :reboot

    (_ :upd-tests
       ;; nested vec upd test
        (E+ iop [v:val [1 2 3] i 1 o {p 1 q [:doc "iop" pop 42]}])
        ;; vec at mpath are normal vec values
        (!! iop.v)
        ;; nested vec updates works
        (!! iop.o.q:doc)

        ;; dual mac and upd test
        (E+ um
            {:mac (fn [e xs] (exp e (lst* 'add xs)))
             :upd (fn [e xs] {:um (lst* 'um xs)})
             })

        (E+ iop (um 1 2 3))
        (!! iop:um)

        ;; an upd does not occur at mpath
        (E+ iop:val (um 1 2 3))
        (!! iop)

        (E+ cq:mac
            (fn [e [x]]
              #_(pp (env-simple-quotf e x))
              (exp e (env-simple-quotf e (composite.compile-quote x)))))

        #_(call* '(+ 1 2))
        #_(!! (let [x 1] (cq (~x . xs))))
        #_(!! (cxp @E '(cq (a ~. as))))

        ;; define an upd and use it in the same block
        (E+ 
         yupd:upd (c/fn [_ xs] {'pouet (vec xs)})
         baba (yupd 1 2 3)
         )

        (_ :error
           (rEset!)
           (env-upds
            @E '[op [yupd:upd (fn [_ xs] {'pouet:val (vec xs)})
                     baba (op.yupd 1 2 3)]])

           (E+ op [yupd:upd (fn [_ xs] {'pouet (vec xs)})
                   baba (op.yupd 1 2 3)]))

        ;; for macros it works as intended
        (E+ post [fix:mac (fn [_ xs] (reverse xs))
                  n (post.fix 1 2 3 +)])
        (!! post.n)

        ;; lambda upd
        (E+ l2 (fn lamb1
                 "lamb1 doc"
                 {:some :meta
                  :demo (fn [] (eq 1 (l2 3 1)))}
                 ([x] x)
                 ([x y] (lamb1 y))))

        (!! (l2:demo))

        ;; links test
        (E+ a.boo [{:doc "ab doc"} (fn [] 'b) c 1]
            :links {boo a.boo})

        (into {} @E)

        (!! a.boo:doc)
        (!! boo:doc)

        (swap! E assoc-in [:members :links '_] root-path)

        (E+ a 1
            b {a 2 p (add _.a a)})

        (!! b.p)

        (E+ documented-id
            ["my id function"
             (fn [x] x)])

        (!! (documented-id 1))
        (!! documented-id:doc)

        )

    (rEset!)

    (do :base

        (rEset!)

        (E+

         env
         [add-sub
          ["add a substitution to an environment at current location
            it is used for shadowing bindings in let and lambda"
           (c/fn [e [s1 s2]]
             (c/let [at (path (loc e) s1)]
               (env-merge-members
                e {(path at :sub) (k s2)
                   ;; maybe should avoid when no existant macro... perf penality?
                   (path at :mac) (c/fn [e form] ($ (cons s1 form) (p exp e)))})))]

          add-subs
          (c/fn [e submap]
            (c/reduce add-sub e submap))]

         hygiene
         {shadow
          [;; helpers
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

           pat->submap
           ["turn a clojure binding pattern into a substitution map
             from symbol to uniq symbol"
            (c/fn [pat]
              (c/let [syms (shrink- (p/findeep pat symbol?) (p = '&))]
                (->> (set syms)
                     (c/map (fn [s] [s (gensym (sym s '_))]))
                     (into {}))))]

           ;;main
           "takes an environment and a binding pattern
            return a new environment with uniq symbols substitutions
            and the pattern with all syms made uniq"
           (c/fn [e pat]
             (c/let [pat (.expand-keys-pattern pat)
                     shadenv (env.add-subs e (.pat->submap pat))]
               [shadenv (exp shadenv pat)]))]}

         primitives
         ["hygienic versions of lambda and let"

          fn
          {parse
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
              e (.parse form)))}

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
              e (.parse form)))}

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
                   ? (and (sym? b1) (sym? b2))]
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
              (optimize e (lst `first (form xs))))

            :tries
            '(do (exp @E '(cs [[x & _xs] y] y :nop)) 
                 )]]]

         :links {fn primitives.fn
                 let primitives.let
                 loop primitives.loop
                 cs primitives.cs.optimized
                 }

         error:mac
         (fn [e xs]
           (lst* `p/error
                 "at: " (path->str (loc e))
                 "\nctx:\n"
                 (pretty-str (first (:exp-ctx e)))
                 "\n"
                 (exp e xs)))))

    #_(E+ a.b.c
        (fn [a] (error "iop iop: " a)))

    #_(!! (a.b.c 42))

    (do :strict-qualify

        (defn gensym? [x]
          (and (symbol? x)
               (re-matches #"^.*_[0-9]+#*$" (name x))))

        (def unqualifiable-symbols
          '#{if do recur fn* let* try catch quote _ . .. &})

        (defn unqualifiable? [x]
          (or (unqualifiable-symbols x)
              (gensym? x)))

        (defne qualify

          [e x]

          (cp x

              sym?
              (cs [qs (qualsym e x)] qs
                  (unqualifiable? x) x
                  (c/resolve x) (ns-resolve-sym x)
                  (error "not resolvable: " x))

              seq?
              (cs [p (-> x car path)
                   ? (or (ppath? p) (macpath? p))
                   p (qualsym e (path p :mac))]
                  (mark-exp (cons p (cdr x)))
                  ($ x (p qualify e)))

              holycoll?
              ($ x (p qualify e))

              x))

        #_(-> (qualify @E '(pos? 1 (hygiene.shadow.pat->submap x)))
              next next first first type)
        )

    (E+ composite
        [
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

         quotf
         ["quoting function,
           handles unquoting, does not quote dots,
           qualifies symbols if possible"

          wrap
          (fn [x] (list (symbol "quote") x))
          rootsym
          (fn [p] (path->sym (path (symbol "_") p)))

          (fn
            [e form]
            (cp form
                dot? dot
                dotdot? dotdot
                unquote? (exp e (second form))
                seq? (cons `list ($ form (p quotf e)))
                holycoll? ($ form (p quotf e))
                symbol?
                (cs [p (path form)
                     [p _] (bubfind e p)]
                    (.wrap (.rootsym p))
                    (.wrap form))
                (.wrap form)))]

         quote:mac
         (fn [e [x]]
           (expand (quotf e x)))

         cxp
         (fn [e x]
           (expand (exp e x)))]

        :links {quote composite.quote
                cxp composite.cxp})

    ;; temp misc
    (E+ add c/+
        sub c/-
        div c//
        mul c/*
        eq c/= ;;will be rebound later, needed for check blocks

        quot:mac
        (fn [_ [x]]
          (lst (sym "quote") x))

        check
        {:mac
         (fn [e xs]
           (cxp e '(p/asserts . ~xs)))
         thunk
         {:mac
          (fn [e xs]
            (cxp e '(fn [] (p/asserts . ~xs))))
          :upd
          (fn [e xs] {:check '(check.thunk . ~xs)})}}

        upd.mk:upd
        (fn [e [x]] (eval e x))

        group:upd
        (fn [e [metas & xs]]
          ($ (env-upd_split xs)
             ($vals #(vector metas %))))

        tagged:upd
        (fn [e [t & xs]]
          (let [ts (if (c/keyword? t) #{t} (c/set t))]
            (c/vec
             (mapcat
              (fn [x] [($ x (fn [[k _]] [(path k :tags) ts])) x])
              (env-upd_split xs))))))

    #_(E+ num!:mac (fn [e [x]] (when-not (number? x) (error "no num")) x)
        testsum (fn [a] (add 1 2 3 (add 6 7 (num! a)))))

    (init-top-forms check)

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

         :tries
         '(do
            (E+ foo.bar {a [:doc "foobar a" (fn [] 'foobara)]}
                foo.qux 42
                (import foo [bar qux])
                here [(import foo [bar qux]) (add qux qux)]
                )
            (!! bar.a:doc)
            (!! qux:val)
            (!! here:val)

            (E+ foot {foota 1 footb 2 footc {n 4}})
            (E+ (import foot :all))
            (!! (add foota footb footc.n)))})

    #_(ppenv import.build-upd.prefix-members)
    #_(error "_")

    (E+ generic
        {:doc
         "an update to define a generic function
          and its related inspection and extension capabilities"

         :upd
         (fn [e body]
           (let [gsym (generic.symbol (loc e))
                 e (env-add-member e (path (loc e) :val) ::unbound)]
             (assoc (generic.module gsym)
                    :fx '(generic.init ~gsym ~body) #_(generic.init e gsym body))))

         reduced:upd
         (fn [e [argv & decls]]
           (let [[arg1 varg] (p/gensyms)]
             '(generic
               ([~arg1] ~arg1)
               (~argv . ~decls)
               (~(conj argv '& varg)
                . ~(c/mapcat
                    (fn [[t i]]
                      [t '(reduce (fn ~argv ~i) ~i ~varg)])
                    (c/partition 2 decls))))))

         module
         (fn [gsym]
           '{:val ~gsym
             inspect:val
             (fn [] (@g/reg '~gsym))
             extend:upd
             (fn [e bod]
               {:fx (g/extension-form
                     (generic.spec e '~gsym bod))})
             })

         spec
         {:val
          (fn [e n body]
            (.exp-cases e (g/generic-spec n body)))

          exp-case
          (fn [e [argv & body]]
            (let [[e argv] (hygiene.shadow e argv)]
              (lst* argv (exp e body))))

          exp-cases
          (fn [e s]
            (update s :cases $ (p ..exp-case e)))}

         symbol
         (fn [n]
           (path->varsym (path n :generic)))

         #_init
         #_(fn [e n body]
           (g/declaration-form
            (..spec e n body)))

         init:mac
         (fn [e [n body]]
           (g/declaration-form
            (..spec e n body)))

         type+
         {:doc
          "lets you implement some generics for a type.
           analog to extend-type"

          :upd
          (fn [e [type & body]]
            ($ (c/vec body)
               (fn [[n & xs]]
                 '(~(p/sym n ".extend")
                   . ~(g/impl-body->cases type xs)))))}

         :tries
         '(do

            (exp @E '(fn [x & xs] :yop))

            (E+ pul+
                (generic [a b]
                         :str (str a b)
                         :sym (pul+ (str a) (str b))
                         :num (+ a b)))

            (!! (pul+.inspect))

            (!! (pul+ 1 2))
            (!! (pul+ "a" 2))

            (E+ (pul+.extend
                 [x y]
                 :vec (do (pp 'hey) (catv x y))))

            (!! (pul+ [1] [7]))
            (!! (pul+.inspect))

            (E+ pil+
                (generic.reduced [a b]
                                 :str (str a b)
                                 :num (+ a b)))

            (!! (pil+.inspect))

            (!! (pil+ 7 9 7))

            (E+ foo.bar.fortytwo:sub (fn [_] 42))
            (E+ foo.bar.g (generic [x] :num (+ ..fortytwo x)))
            (!! (foo.bar.g 1)))})

    (E+ fn&

        [expansion-size 5

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
           (exp e '(fn . ~(fn&.cases form))))

         :tries
         '(do
            (exp @E '(fn& [a b]
                          (toto (yop ...)
                                {:a [m n o ...]
                                 :b [a b c #{foo a ...}]}))))])

    (E+ joining
        ["a bunch of function for handling monoidish things"

         pure
         ["a generic function that return the identity value from a given value"

          (generic
           [_]
           :fun id
           :vec []
           :seq ()
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
           :seq (c/concat a [b])
           #{:set :vec} (c/conj a b)
           :map (c/assoc a (c/first b) (c/second b))
           :fun (c/partial a b))

          :notes
          "maybe we should consider to implement sip for named
                (sipping some chars makes sense)"

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
           (eq () nil)
           (eq () "")
           (eq '(1 2) (iter [1 2]))
           (eq '(1 2) (iter '(1 2)))
           (eq '([:a 1] [:b 2]) (iter {:a 1 :b 2}))
           (eq '(\f \o \o) (iter "foo") (iter 'foo) (iter :foo)))]

         +
         ["join two things together
           similar to concat, merge..."

          (generic.reduced
           [a b]
           :fun (c/comp b a)
           :seq (c/concat a (iter b))
           :str (c/str a b #_(.toString b))
           :sym (c/symbol (c/str (c/name a) b #_(.toString b)))
           :key (c/keyword (c/str (c/name a) (c/name b)))
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
               3))]

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
           (eq '(:a :b :c)
               (sort (idxs {:a 1 :b 2 :c 3}))
               (sort (idxs #{:a :b :c}))))]

         pure?
         ["test if something is equal to its pure value"

          (generic [x]
                   :seq (when-not (seq x) ())
                   (when (c/= x (pure x)) x)
                   ;; using identical? is suspicious but seems to work...
                   ;; core/= cannot be used here because (= [] ()) is true...
                   #_(when (c/identical? x (pure x)) x)
                   #_(when (if (holycoll? x) (not (seq x))
                               (c/identical? x (pure x)))
                       x))

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
                '[~n (fn& [] (sip ~e ...))
                  ~n+ (fn& [] (+ ~e ...))
                  ~n* (fn& [x] (apl ~n x ...))
                  ~n+* (fn& [x] (apl ~n+ x ...))]))
             ([x & xs]
              (catv ($ (cons x xs) make-upd))))

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
            '(generic
              ~argv
              :seq
              ~expr
              (let [a ~a1
                    ~a1 (iter ~a1)]
                (wrap* a ~expr))))]

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

        )

    (E+ types
        [
         prims (keys t/prims)
         builtins t/builtin-types
         preds t/builtin-preds

         ;; type generic
         (upd.mk
          (let [arg1 (gensym)]
            {(quot type)
             '(generic
               [~arg1]
               .~(c/interleave prims prims)
               :any (c/type ~arg1))}))]

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
                  '(fn [~arg1] (fn ~argv (when ~test ~(car argv))))]
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
             {name '(~(guard.template arity)
                     ~(ns-resolve-sym (or original-name name)))})
            ([x & xs]
             ($ (vec* x xs) rec)))

          types:upd
          (fn [e _]
            (let [ts (seq (disj types.builtins :nil))]
              (zipmap
               ($ ts #(sym % "?"))
               ($ ts #(lst 'types.preds %)))))}

         builtins
         [(import
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

          (import.types)]

         :tries
         '(do

            (res @E '(guard.types-definition nil nil))

            #_(!! (map? []))
            #_(!! (nil? nil))
            #_(!! (vec? []))

            (res @E '(guard.declare-types-guards))

            (res @E '(guard.import neg? 1))
            (env-upd? (exp @E '(guard.imports [neg? 1]
                                              [pos? 1]
                                              [gt 2 >])))

            #_(!! (line? [1 2]))

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
        [throws:mac
         (fn [e [x m]]
           (let [expr (c/str x)]
             '(or
               (::catched
                (try ~(exp e x)
                     (catch Exception err {::catched err})))
               (p/error ~(or m "this should throws")
                        ":\n" ~expr))))

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
            (exp e '(lst* 'do (c/map ~f ~x))))

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
           '(p/error (error-str "nil" ~m))

           :string nil

           :any
           '(or ~x (p/error (error-str '~x ~m)))
           )]

         assert
         {:mac
          (fn [e [x m]]
            (exp e (assertion x m)))
          :upd
          (fn [_ xs]
            {:fx '(assert .~xs)})}

         eq!:mac
         (fn [e xs]
           (exp e '(assert (eq .~xs) "must be equal")))

         tests:upd
         (fn [e xs]
           {(quot tests)
            {:form '(quot ~(vec* (assertion.vec-split xs)))
             :do '(fn [] (assert ~(vec* xs) ~(path->str (loc e))))}})

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

    #_(exp @E '(assert (not nil) (str "iop")))
    #_(!! (assert nil (str "iop")))

    (E+ bindings
        {:val
         (fn [xs]
           (mapcat (p* bind) (partition 2 xs)))

         :links {cp composite}

         bind
         [
          (generic
           [x y]
           :sym [x y]
           :vec (bind.vec x y)
           :map (bind.map x y)
           :seq (bind.seq x y)
           [(gensym "?match") (lst 'eq x y)])

          vec
          {:val
           (fn [x y]
             (if (cp.single-dotted? x)
               (.dotted x y)
               (.raw x y)))

           body
           (fn [x y]
             (mapcat
              (fn [v i] (bind v '(c/nth ~y ~i nil)))
              x (range)))

           raw
           (fn [x y]
             (let [[ysym checkline checkcount] (gensyms)]
               (+
                [ysym y
                 checkline '(line? ~ysym)
                 ;; checkcount '(= ~(count x) (count ~ysym))
                 ]
                (bind.vec.body x ysym))))

           dotted
           (fn [x y]
             (let [doti (indexof x cp.dot)
                   cars (take x doti)
                   [eli queue] (uncs (drop x (inc doti)))
                   qcnt (count queue)
                   [ysym qsym checkline cdr' cars'] (gensyms)]
               (+
                [ysym y
                 checkline '(line? ~ysym)]
                (bind eli '(drop ~ysym ~doti))
                (bind.vec.body cars ysym)
                (when-not (zero? qcnt)
                  (+ [cdr' eli]
                     (bind eli '(dropend ~cdr' ~qcnt))
                     [qsym '(takend ~cdr' ~qcnt)]
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
                (bind v '(get ~y ~k)))
              x))

           raw
           (fn [x y]
             (let [[checkmap ysym] (gensyms)]
               (+
                [ysym y
                 checkmap '(map? ~ysym)]
                (bind.map.keys x ysym))))

           dotted
           (fn [x y]
             (let [rs (get x cp.dot)
                   m (dissoc x cp.dot)
                   ks (c/keys m)
                   [checkmap msym] (gensyms)]
               (+
                [msym y]
                (bind.map.keys m msym)
                (bind rs '(c/dissoc ~msym . ~ks)))))}

          seq
          (fn [[v & args] y]
            (cs [op (bind.ops.get v)]
                (op args y)
                (bind.ops.default (cons v args) y)))

          ops
          {get
           (fn [x] (c/get @..table x))

           default
           (fn [[v s & args] y]
             (cs (sym? s)
                 (cs (key? v)
                     [s y (gensym "?typecheck") '(eq (type ~s) ~v)]
                     [s (lst* v y args)])
                 (error "guard binding takes a symbol as first argument"
                        (lst* v s args))))

           #_(!! (bindings.bind '(:vec a) 'x))

           table
           (atom
            {'&
             (fn [xs y]
               (let [ysym (gensym)]
                 (cat* [ysym y]
                       ($ xs #(bind % ysym)))))

             'ks
             (fn [xs y]
               (bind (zipmap ($ xs keyword) xs) y))

             'tup
             (fn [xs y]
               (let [xs (vec* xs)
                     [ysym checkline checkcount countable?] (gensyms)]
                 (+
                  [ysym y
                   checkline '(line? ~ysym)
                   countable? '(c/counted? ~ysym)
                   checkcount '(= ~(count xs) (count ~ysym))]
                  (bind.vec.body xs ysym))))

             '!
             (fn [[f & [p]] y]
               (bind (or p (gensym)) (lst f y)))})

           add
           (fn [s f]
             (swap! ..table assoc s f))}
          ]

         unified
         (fn [xs]
           (loop [ret [] seen #{}
                  [a b & nxt] (bindings xs)]
             (if a
               (if (seen a)
                 (recur (conj ret (gensym) '(eq ~a ~b)) seen nxt)
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
            (condp = (car (str s))
              \! :strict
              \? :short
              \_ :opt
              nil))

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
             (not p1) (cxp e expr)
             ;; if both e1 is syms we can just substitute
             ;; instead of adding a binding
             ;; we also check if p1 has not a different mode (have to think of this further)
             [? (sym? e1)
              ? (or (not (sym->mode p1))
                    (eq (sym->mode p1) (sym->mode e1)))
              e' (env.add-sub e [p1 (exp e e1)])]
             (form e' bs expr mode)
             ;; else we add a binding
             [step-mode (or (sym->mode p1) mode)
              [e' pat] (hygiene.shadow e p1)]
             (step-form
              pat (cxp e e1)
              (form e' bs expr mode)
              step-mode)))

          compile
          {:val
           (fn [e opts]
             (if (:name? opts)
               (.named e opts)
               (.anonymous e opts)))

           anonymous
           (fn [e opts]
             (bindings.let.form
              e (if (:unified opts)
                  (bindings.unified (cat* (:bs opts)))
                  (bindings (cat* (:bs opts))))
              (:expr opts)
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
                        (lst (if unified 'lut '?let)
                             h {::return e})
                        (lst `when h {::return e}))))

                 retform
                 '(get ~(exp e (lst* `or cs)) ::return)]
             (if strict
               #_'(assert ~retform "clet no match!")
               '(or ~retform ~(exp e '(error "clet no match!")))
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
                  [[:point x y] p] [x y])))

         :fx (clet.tests:do)]

        (import bindings.let.builtins
                [clet clut !clet !clut])
        )

    #_(error "after bindings")

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
            ({[nil nil nil] 'let
              [nil true nil] '?let
              [true nil nil] '!let
              [nil nil true] 'lut
              [true nil true] '!lut}
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

             (cxp e
                  (lst 'primitives.fn . (if name [name] [])
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
              (assert (apl eq ($ xs #(:min-arity %)))
                      (str* "variadic arities count mismatch\n"
                            (interleave ($ xs #(:pat %)) (repeat "\n")))))]

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
                (parse.check-variadic-sigs (:& arities)))

              {:name name
               :monadic monadic
               :variadic variadic
               :polyadic polyadic
               :arity-map arities
               :cases cases}))]

          compile
          {:val
           (fn [e {:as opts :keys [arity-map name]}]
             (exp e '(fn .~(when name [name])
                        .~($ (iter arity-map) (p .arity opts)))))
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
                  (cs (and unified strict) '!lut
                      unified 'lut
                      short '?let
                      strict '!let
                      'let)
                  (cs (and unified strict) '!clut
                      unified 'clut
                      strict '!clet
                      'clet)))

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
                    prefcnt (:min-arity (car cases))
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

         f_:mac   (f [e xs] (cxp e (lst* 'f1 '_ xs)))
         ?f_:mac  (f [e xs] (cxp e (lst* '?f1 '_ xs)))
         !f_:mac  (f [e xs] (cxp e (lst* '!f1 '_ xs)))
         !fu_:mac (f [e xs] (cxp e (lst* '!fu1 '_ xs)))
         fu_:mac  (f [e xs] (cxp e (lst* 'fu1 '_ xs)))

         cf:mac   (cased.compiler)
         !cf:mac  (cased.compiler :strict)
         ?cf:mac  (cased.compiler :short)
         cfu:mac  (cased.compiler :unified)
         !cfu:mac (cased.compiler :unified :strict)]}

        (import lambda.builtins :all)

        bindings.let.compile.named
        (fn [e opts]
          (lst* (lambda.compile
                 e (mrg opts
                        {:pat (mapv car (:bs opts))
                         :arity (count (:bs opts))
                         :body [(:expr opts)]
                         :name (:name opts)}))
                (cxp e ($ (:bs opts) second)))))

    #_( 1 2)
    #_(exp @E '(cf :io [a] 1 [a b] 2 [(:num a) b c . xs] :var1 [a b . d] :var2))
    #_(exp @E '(cf :io [a] 1 [(:num a) b] 2 [(:str a) b] 3))
    #_(exp @E '(primitives.let [s size ret [['a 'd]]]
                                (if (eq 1 s)
                                  (* + (cdr ret))
                                  (recur (dec s)
                                         (sip ret
                                              ($+ (last ret)
                                                  #($ ['a 'd] (p + %))))))))

    (do :guard-macro
        (E+ guard:mac
            (f [e (& form [argv . _])]
               (let [(& parsed (ks pat body)) (lambda.parse form)
                     body '(when ~body ~(car pat))]
                 (lambda.compile e (assoc parsed :body body))))
            guard:fn
            (f [g] (fn& [x] (when (g x ...) x))))

        #_(check ((guard:fn c/>) 2 1 0)
                 ((guard [x y] (c/> x y)) 3 2)))

    (E+ df
        {:mac
         (f [e [x]]
            (let [all (.walk x (f1 y (lst y (gensym))))
                  pat (.walk all second)
                  expr (.walk all (f1 (& x [v s]) (if (= '_ v) s x)))]
              (exp e '(f1 ~pat ~expr))))

         walk
         (f [x g]
            (cp x
                map? ($vals x #(walk % g))
                vec? ($ x #(walk % g))
                dot? x
                (g x)))})

    (do :§*$

        (E+

         callables
         {wrapper:mac
          (f [e [builder]]
             (let [[[a1 [e1]] . cs]
                   (fn&.cases '([x] ((~builder x) ...)))]
               (exp e '(fn (~a1 (p ~e1)) . ~cs))
               #_(exp e (lst 'fn '(~a1 (p ~e1)) . cs))))}

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

         > (f [x . fs]
              ((* + ($ fs §)) x))

         $
         (generic.reduced
          [x f]
          :map (c/into {} (c/map (fn [[k v]] [k (§ f v)]) x))
          :set (c/set (c/map (§ f) x))
          :vec (c/mapv (§ f) x)
          :seq (c/map (§ f) x)
          :any (error "not mappable" x))

         ;; $ indexed
         $i
         (generic.reduced
          [x f]
          :map (c/into {} (c/map (fn [[k v]] [k (§ f k v)]) x))
          :set (c/set (c/vals ($i (c/zipmap x x) f)))
          :vec (c/vec (c/map-indexed (§ f) x))
          :seq (c/map-indexed (§ f) x)
          :any (error "not mappable" x))

         walk
         (f [x in out]
            (if (coll? x)
              (out ($ x in))
              (out x)))

         dfwalk
         (f
          [x f]
          (walk x #(dfwalk % f) f))

         bfwalk
         (f
          [x f]
          (walk (f x) #(bfwalk % f) id))

         walk?
         (f
          [x ? f]
          (cs [nxt (? x)]
            ($ nxt #(walk? % ? f))
            (f x))) )

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
                    (c/merge-with * x y)))))

        (_ :tries

           (qbench (!! (§ {:a add :b sub} {:a 2 :b 1 :c 56} {:a 3 :b 5})))
           (qbench (!! (§ [add sub add] [1 2 3] [1 2 3] [1 2 3])))
           (qbench (!! (c/mapv (fn [f args] (apl f args))
                               [add sub add]
                               (mapv list [1 2 3] [1 2 3] [1 2 3]))))
           (qbench (!! (* [add sub add] (list [1 2 3] [1 2 3] [1 2 3]))))))

    (E+ subjectify
        {:val
         (f1 f
             (fn& [] (f1 s (f s ...))))
         :mac
         (f [e [g]]
            (exp e '(fn& [] (f1 s (~g s ...)))))
         definitions:upd
         (f [e xs]
            (zipmap
             ($ xs #(sym % '_))
             ($ xs (p lst 'subjectify))))}

        (subjectify.definitions
         take takend drop dropend section
         sip + * § $
         eq neq gt gte lt lte
         walk dfwalk bfwalk walk?))

    (do :$related

        (E+

         zip
         (f                            ;core/map(ish)
          [f . xs]
          (* c/map f ($ xs vals)))

         red
         (fn
                                        ;reduce with seed (init) as first argument
                                        ;and variadic seq(s) argument (like map does)
           ([x f xs]
            (c/reduce (§ f) x xs))
           ([x f y & ys]
            (red x (* f)
                 (* zip vec y ys))
            #_(if (c/every? cons? xs)
                (* red
                   (* f x ($ xs car))
                   f ($ xs cdr))
                x)))

         ;; $+ is to § what c/mapcat is to c/map
         $+
         (generic.reduced
          [x f]
          :any
          (* + #_(pure x) ($ x f)))

         zip+
         (f                            ;core/mapcat(ish)
          [f . xs]
          (cs [ret (c/seq (* zip f xs))]
            (* + ret) ()))

         scan
         (f                            ;similar to core/partition
          [x size step]
          (let [[pre post] (splat x size)]
            (if (cons? post)
              (cons pre (scan (drop x step) size step))
              (if (cons? pre)
                (sip (pure x) pre)
                (pure x)))))

         chunk
         (f [x size]
            (scan x size size))

         nths
         (f [x n]
            ($ (scan x n n) car))

         braid
         (p zip+ (p sip ()))

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

                                        ;scan
         (eq [[1 2] [3 4]]
             (scan [1 2 3 4] 2 2))
         (eq [[1 2] [2 3] [3 4]]
             (scan [1 2 3 4] 2 1))
         (eq '((0 1 2 3) (2 3 4))
             (scan (c/range 5) 4 2))

                                        ;chunk
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

    (do :compare

        (E+

         eq
         (generic

          ([x] #(eq % x))

          ([x y]
           :coll
           (when (and (c/= x y) (c/= (type x) (type y))) x)
           :nil (nil? y)
           :any (when (c/= x y) x))

          ([x y & xs]
           (* eq (eq x y) xs)))

         neq
         (guard [x y]
                (nil? (eq x y))))

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
                        '(+ . ~($ (> _ iter rev)
                                  (p c/get {\a 'car \d 'cdr})))})))}

            (linear-accesses.definitions))

        (check
         (eq :io
             (cadr [1 :io])
             (caddr [1 2 :io])
             (caadr [1 [:io 2] 3])
             (cadadr [1 [2 :io]]))))

    (do :dive

        (E+ dive
            (generic
             [x y]

             :vec
             (c/reduce #(dive %2 %1) y x)

             #{:key :sym}
             (c/when (hash? y)
               (c/get y x))

             :num
             (gat y x)

             :set
             (dive (+ {} ($ (iter x) #(vec % %))) y)

             :map
             ($ x #(dive % y))

             :fun (§ x y)

             :nil y
             )

            dive
            {:mac
             (f [e [x y]]
                (exp e
                     (lst* 'dive:val
                           (clet [[v . args] x
                                  impl (get .ops v)]
                                 [(impl args) y]
                                 [x y]))))
             ops:val
             {'ks
              (f [xs]
                 '(fn [y] (select-keys y ~(vec* xs))))}})

        #_(!! (dive (ks :a :b) {:a 1 :b 2 :c 2}))
        #_(!! (dive [:a :b :c -1] {:a {:b {:c [42 41 40]}}})))

    (do :flow

        (E+

         ;; guard based constructs
         ;; guard composition, filtering, control flow etc...

         $?
         (guard [x] (c/every? id (vals x)))

         ?$
         (+ $ $?)

         ;; ?zip is to zip what ?$ is to $
         ?zip (+ zip $?)

                                        ;:fx (pp 'iop)

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

         ;; is to $ what core/filter is to core/map
         filt
         (generic.reduced [x f] :any (shrink x f id))

         ;; is to $ what core/remove is to core/map
         rem
         (generic.reduced [x f] :any (shrink x f not))

         :fx
         (check
          (eq [1 2 3]  (filt [1 2 -1 -2 3] pos?))
          (eq [-1 -2] (rem [1 2 -1 -2 3] pos?)))

         ?$i (+ $i $?)

         :fx
         (check
          (eq [1 2 3] (filt [1 2 -1 -2 3] pos?))
          (eq [-1 -2] (rem [1 2 -1 -2 3] pos?))
          ;; success because no element is equal to its idx
          (?$i [1 2 3] neq)
          ;; fail because 2 element is equal to its idx
          (nil? (?$i [1 1 3] neq)))

         ?deep
         (guard
                                        ;"deeply check x for nils"
          [x]
          (if (coll? x)
            (?$ x ?deep)
            x))

         :fx
         (check
          (nil? (?deep {:a {:b 1 :c [1 2 nil]}}))
          (nil? (?deep {:a {:b 1 :c [1 2 3 {:d nil}]}}))
          ;; succeed
          (?deep {:a {:b 1 :c [1 2 3]}}))

         ?>
         (f
                                        ;"thread x thru guards
                                        ; shorting on first nil result"
          [x f . fs]
          (?let [x (c/and x (§ f x))]
                (if (cons? fs) (* ?> x fs) x)))

         ?<
         (f
                                        ;"thread x thru guards
                                        ; until first non nil result"
          [x f . fs]
          (c/or (§ f x)
                (c/when (cons? fs) (* ?< x fs))))

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
         (f                            ;"a scheme-cond(ish) function"
          [x . bs]
          (* ?< x ($ bs (* ?>_))))

         ?c
         (f                            ;"a clojure-cond(ish) function"
          [x . cs]
          (* ?c> x (chunk cs 2)))

         ?><
         (f                 ;"at least one guard of each branch have to succeed
                                        ; last branch's first success returned"
          [x . bs]
          (* ?> x ($ bs (* ?<_))))

         (subjectify.definitions
          ?c> ?c ?><)

         :fx
         (check

          (eq 2
              (?c 1
                  ;; like clojure cond
                  ;; works by couples
                  str? :pouet ;; if str? succeed :pouet is called
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

    (do :mlet

        (E+ mlet:mac
            (fn [e [ms & bod]]
              (let [local-path #(path (loc e) % :mac)
                    ;; this should be abstracted (local env extension)
                    e (red e (f [e [s x]] (env-add-member e (local-path s) (eval e x)))
                           (chunk ms 2))]
                (exp e (bindings.bodify bod)))))

        (_ :tries
           (!! (mlet [fi (fn [e [p f t]] (exp e (lst 'if p t f)))]
                     (fi (vec? []) :novec (?lut [a 1 a 2] :vec))))

           (exp @E '(mlet [fi (fn [e [p f t]] (exp e (lst 'if p t f)))]
                          (fi (vec? []) :novec (?lut [a 1 a 2] :vec)))))

        (E+ let-syms:mac
            (fn [e [xs & bod]]
              (exp e '(let [~xs (gensyms)] . ~bod))))

        (!! (let-syms [a b c] (list a b c)))

        (E+ mac:mac
            (f [e [pat . body]]
               (exp e '(f [e' ~pat] (exp e' ~(lst* 'do body))))))

        (_ :tries
           (exp @E '(mac [p f t] (lst 'if p t f)))
           (E+ fi:mac (mac [p f t] (lst 'if p t f)))
           (exp @E '(fi yop (lut [a 1 a 2] :pouet) (fn& [] (add ...)))))

        (E+ at:mac (mac [x y] (lst 'dive y x))
            at_:mac (mac [x] '(f_ (at _ ~x))))

        (_ :tries
           (!! (at {:a 1 :b 2 :c 2} (ks :a :b)))
           (!! ((at_ (ks :a :b)) {:a 1 :b 2 :c 2}))))

    #_(expand @E (qualify @E '(fn [verb symseed xs]
                       (if (even? (count xs))
                         (lst verb .(* + ($ (chunk xs 2) (f1 [p e] [[p symseed] e]))))
                         (rec verb symseed [.(butlast xs) (quot _) (last xs)])))))

    (E+ bindings.case
        [;:fx (pp "suspect unresolvable rec bindings case 3673")

         form
         (f [verb symseed xs]
            (if (even? (count xs))
              (lst verb .(* + ($ (chunk xs 2) (f1 [p e] [[p symseed] e]))))
              (rec verb symseed [.(butlast xs) (quot _) (last xs)])))

         ;:fx (pp "after suspect unresolvable rec bindings case 3673")

         verb
         (f1 (ks strict unified)
             (cs (and strict unified) '!clut
                 strict '!clet
                 unified 'clut
                 'clet))

         compile
         (f [e [seed . xs] opts]
            (let-syms [symseed]
              (exp e
                   '(c/let ~[symseed seed]
                      ~(form (verb opts) symseed xs)))))

         def:upd
         (f [_e [name . flags]]
            (let [mac-sym (sym name :mac)
                  smac-sym (sym name "_" :mac)]
              {mac-sym
               '(f [e bod]
                   (compile e bod ~(flagmap . flags)))
               smac-sym
               '(f [e xs]
                   (exp e (lst 'f_ (lst* '~name '_ xs))))
               }))

         builtins
         [(case.def case)
          (case.def casu :unified)
          (case.def !case :strict)
          (case.def !casu :unified :strict)]]

        (import bindings.case.builtins
                [case casu !case !casu
                 case_ casu_ !case_ !casu_])

        (assert

         [(let [t (case_
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
          ]))

    #_(E+ move-members
        {:doc
         "a couple of updates for moving (aliasing is more accurate) members from path to path
              It is mainly used in order to being able to define related functions in a module
              which is handy to put documentation tests exemples and helpers
              then make them available elsewhere (most commonly top level)"

         :usage
         '[(move-members target-path [mod1.foo mod2.bar])
           (move-members base-path target-path [m1 m2 m3])
           "if target-path is nil, root-path is used"]

         ;; helpers

         path-head
         (fn [p]
           (let [mk (.mkey p)
                 ps (path-segments (ppath p))]
             (path (last ps) mk)))

         switch-prefix
         (fn [pref p]
           (let [head (path-head p)]
             (or (path pref head) head)))

         build-upd
         (fn ([target-path xs]
             (let [xs ($ xs path)
                   ks ($ xs (p switch-prefix target-path))]
               (zipmap ($ ks path->sym)
                       ($ xs path->sym))))
           ([base-path target-path xs]
            (build-upd target-path
                       ($ xs (p path base-path)))))

         ;; update

         :upd
         (fn [_ xs]
           (apl build-upd xs))

         to-root:upd
         (fn [_ [a b]]
           (if-not b
             (build-upd nil a)
             (build-upd a nil b)))

         :tries
         '(do

            (E+ ffoo.bar {a 1 b 2}
                (move-members nil [ffoo.bar.a ffoo.bar.b]))

            (E+ ggoo.bar {a :ggoobara b :ggoobarb}
                (move-members ggoo.bar nil [a b]))

            (!! a))})

    (pp :will-declare-topforms)

    (init-top-forms
     let lut ?let !let !lut
     ?f !f f fu !fu
     f1 !f1 ?f1 !fu1 fu1
     f_ !f_ ?f_ !fu_ fu_
     cf ?cf !cf cfu !cfu 
     clet clut !clet !clut
     case casu !case !casu)
)

(defmacro qbench [& xs]
  `(do ~@(c/map (fn [x]
                  `(do (println) (println "benching:" '~x) (println)
                       (cr/quick-bench ~x)))
                xs)))

(_ :scratch

   (_ :vecbench
      (require '[modules.benchs :as bs])
      (let [r (vec (range 1000))]
        (bs/benchrace 10000
                      (doall (map inc r))
                      (mapv inc r)
                      (loop [v r cnt (count r) i 0]
                        (if (= i cnt) v
                            (recur (update v i inc) cnt (inc i)))))
        )

      (let [r (vec (range 1000))]

        #_(time (dotimes [_ 100000] (c/vec (drop 10 r))))
        #_(time (dotimes [_ 100000] (!! (drop r 10)))))
      )

   (_
    '(destr [a b . c])
    [a #(get % 0)
     b #(get % 1)
     c #(drop % 2)]

    '(destr [[a1 . as al] b . c])
    [[a1 . as al] #(get % 0)
     b #(get % 1)
     c #(drop % 2)]

    '[inc [dec pos?] . butlast zero?]
    '[[0 inc]
      [1 [0 dec] [1 pos?]]
      [(section 2 -1) sum]
      [-1 zero?]])

   (_
    #_(env-inspect 'bindings.let.compile)
    #_(!! (bindings '[[a [x y] . c] y]))

    (!! (let [a 1 b 2] (c/+ a b)))

    (!! (let [[a [b c . popo] . d e] [8 [9 10 11] 24 67 56 999]]
          #_[a b c d e]
          [(inc a) [(dec b) (pos? c) . (id popo)] . (butlast d) (zero? e)]))

    #_(pp @E))

   (_
    (exp @E '(?let [a 1] (c/+ a 1 2)))
    (!! (?let [(pos? x) 1 (gt y x) 2] (c/+ x y)))
    (!! ((?f [(pos? x) (gt y x)] (c/+ x y)) 1 2))

    (res @E '(f1 :rec a a)))

   (_
    (res @E '(df [inc [dec {:a pos? . _}] . _]))

    (!! ((df [inc [dec {:a pos? . _}] . _])
         [1 [0 {:a 2 :b 'io}] 'po :mo])))

   (_
    #_(!! ((f_ (c/+ _ _)) 1))

    #_(exp @E '(invocation.template invocation 5))

    #_(env-inspect '*')

    #_(!! (let [xs (range)] (take xs 3)))

    #_(!! (invocation c/+))

    (!! (§ c/+ 1 2 3))

    (require '[modules.benchs :as bs])

    (def add c/+)

    (bs/benchrace 1000000
                  (add 1 2 3)
                  (!! (§ add 1 2 3)))

    #_(!! (*' add))
    #_(!! (*' add [4 5 6]))

    (!! (application2.inspect))

    (!! (* c/+ 1 2 3 [4 5 6]))

    (bs/benchrace 1000000
                  (apl add 1 2 3 [4 5 6])
                  #_(!! (*' add 1 2 3 [4 5 6]))
                  (!! (* add 1 2 3 [4 5 6])))

    (qbench (c/vec (range 10))
            (!! (vec+ (range 10))))

    (let [f0 (!! (applied-lambda [a] (c/+ a)))
          f1 (fn [& xs] (apl c/+ xs))]
      (qbench (f0 1 2 3 4)
              (f1 1 2 3 4)))

    (!! (sip [] 1 2 3))

    (let [a [1 2 3] b [4 5 6]
          f (fn [x & xs] (reduce conj x xs))]
      (qbench
       (f a 1 2 3)
       (!! (sip a 1 2 3))))

    (let [a [1 2 3] b [4 5 6]
          f (fn [x & xs] (reduce conj x xs))]
      (qbench
       (vector 1 2 3)
       (!! (vec 1 2 3))))

    (qbench (c/vec (c/map inc (range 10)))
            #_(c/mapv inc (range 10))
            (!! (c/vec ($ (range 10) inc))))

    (cr/with-progress-reporting (cr/quick-bench (add 1 2 3) :verbose))
    (cr/with-progress-reporting (cr/quick-bench (!! (§ add 1 2 3)) :verbose))

    (cr/with-progress-reporting (cr/quick-bench (apl add 1 2 3 [4 5 6]) :verbose))
    (cr/with-progress-reporting (cr/quick-bench (!! (apl (invocation add) 1 2 3 [4 5 6])) :verbose))
    (cr/with-progress-reporting (cr/quick-bench (!! ((p apl (invocation add)) 1 2 3 [4 5 6])) :verbose))
    (cr/with-progress-reporting (cr/quick-bench (!! (* add 1 2 3 [4 5 6])) :verbose))

    )

   (_ :rt-mac-call
      (!! (let' [a 1] (c/+ a a)))
      (!! (let':mac @E '([a 1] (c/+ a a))))))

(_ :benchs

   (defmacro mX [x]
     (list 'quote (exp @E x)))

   (def add c/+)

   (mX (let [f0 (f [a b c] (add a b c))
                                        ;add c/+
             f1 (c/fn [a b c] (add a b c))]
         (qbench (f0 1 2 3)
                                        ;(§ f0 1 2 3)
                 (f1 1 2 3)
                 #_(§ f1 1 2 3))))

   (let [f0 (f [a b c] (add a b c))]
     (qbench (f0 1 2 3)
             (apl f0 [1 2 3])
             (* f0 [1 2 3])))

   (destructure '[[x & xs] y])
   (c/let [f0 (!! (f2 [x . xs] (* add x xs)))
           f1 (c/fn [x & xs] (apl add x xs))]
     (qbench (f0 1 2 3)
             (f1 1 2 3)))

   (qbench (drop 3 [1 2 3 4 5 6])
           (nthnext [1 2 3 4 5 6] 3))

   (clojure.walk/macroexpand-all '(c/fn [x & xs] (apl add x xs)))

   (clojure.walk/macroexpand-all '(c/fn [x [y & ys] & xs] (apl add x xs)))
   (clojure.walk/macroexpand-all '(c/fn [{:a a}] (apl add x xs)))

   (clojure.walk/macroexpand-all '(c/let [[x & xs] y] (apl add x xs)))

   (qbench (drop 3 (range 10))
           (nthnext (range 10) 3))

   (c/let [s (list 1 2 3 4 5 6 7 8)]
     (qbench (!! (take s 3))
             (take 3 s)))

   (qbench (!! (vec? [1 2 3]))
           (vec? [1 2 3]))

   (c/let [[x & xs] {:a 1 :b 2}] [x xs])
   (qbench (let [[x . xs] (c/vec (range 10))] [x xs])
           (c/let [[x & xs] (c/vec (range 10))] [x (c/vec xs)]))
   (qbench (let [[x . xs] (range 10)] [x xs])
           (c/let [[x & xs] (range 10)] [x xs]))

   (c/let [f0 (c/fn [& xs] (c/let [x (nth xs 0) xs (drop 1 xs)] (apl add x xs)))
           f1 (c/fn [x & xs] (apl add x xs))]
     (qbench (f0 1 2 3)
             (f1 1 2 3)))

   )

(_ :incub

    (do :partial-macro

        (E+ p':mac
            (f [e xs]
               (exp e '(fn& [] ~(sip xs '...)))))

        ;; we don't gain much
        #_(qbench (!! ((p' take (range 10)) 5))
                  (!! ((p take (range 10)) 5))))

    (defn attrseq [s]
      (let [pth (path s)]
        (-> (cons (ppath pth) (bubbling-paths pth))
            ($ (p env-get @E))
            ($ #(shrink+ % (fn [[k _]] (keyword? k)))))))

    (defn attr-pathmap
      ([x]
       (into {} (attr-pathmap x root-path)))
      ([x from]
       (if (and (map? x) (ppath? from))
         (cons [from (shrink- x (comp symbol? car))]
               (mapcat (fn [[k v]] (attr-pathmap v (path from k)))
                          (shrink+ x (comp symbol? car))))
         [[from x]])))

    (defn attr-shrink
      ([f] (attr-shrink @E f))
      ([e f]
       (reduce
        (fn [r [p _]]
          (let [ps (path-segments p)]
            (assoc-in r ps (env-get e p))))
        {} (shrink+ (attr-pathmap (:members e)) (fn [[_ v]] (f v))))))

    #_(attr-shrink #(contains? (:tags %) :wrappable))

    )

(pp :DONE)

(_ :scratch

   (let [f (fn [a b]
             (cond
               (and (c/number? a) (c/number? b)) (add a b)
               (and (c/number? a) (c/= [] b)) a
               (and (c/number? a) (c/vector? b)) (rec (add a (c/first b)) (c/vec (c/rest b)))))]
     (qbench (f 1 [1 2 3])))

   (exp @E '(!cf [(:num a) (:num b)] (add a b)
                 [(:num a) (pure? b)] a
                 [(:num a) [b1 . bs]] (rec (add a b1) bs)))

   (let [f (!cf [(:num a) (:num b)] (add a b)
                [(:num a) (pure? b)] a
                [(:num a) [b1 . bs]] (rec (rec a b1) bs))]
     (f 1 [1 2 3 :io]))

   (?let [[] [1]] :iop)

   (?let [[a . as] :iop] [a as])

   (_ :qual-with-ctx

      (defne qualify

        [e x]

        (c/let [e (update e :qual-ctx (fnil conj []) x)]

          #_(pp 'ctx= (:qual-ctx e))

          (cp x

              sym?
              (cs [qs (qualsym e x)] qs
                  (unqualifiable? x) x
                  (c/resolve x) (ns-resolve-sym x)
                  (error "not resolvable: " x))

              seq?
              (cs [p (-> x car path)
                   ? (or (ppath? p) (macpath? p))
                   p (qualsym e (path p :mac))]
                  (mark-exp (cons p (cdr x)))
                  ($ x (p qualify e)))

              holycoll?
              ($ x (p qualify e))

              x)))

      #_(c/let [r (doall (qualify @E '(add 1 2 (add 2 3))))]
          r)

      ))
