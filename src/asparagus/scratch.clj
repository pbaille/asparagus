(in-ns 'asparagus.core)

(_ :one

   (!! (env.exp @E '(let [x 1] (lst (add . (range 10)) (:yop x)))))
   (!! (env.exp @E
                '(lst (add . (range 10)) (:yop x . op ~(add 1 2)))
                {:composite false
                 ;;:method-calls false
                 ;;:strict false
                 }))

   (!! (env.expand.top-lvl-unquotes @E (qualify @E '(_.primitives.fn [_] nil))))

   (exp @E '(loop [[a b & xs] 42] :io))

   (exp @E '(let [a 1] a))

   (!! (and (walk? (c/get @E :members)
                   (f_ (clet [doc (at _ :doc)] (pp doc)) (map? _))
                   id)
            nil)))

(E+ (bind.op+ cons
              [[a b] y]
              (let [ysym (gensym)]
                (+ [ysym y]
                   (bind a (qq (car ~ysym)))
                   (bind b (qq (cdr ~ysym))))))

    (bind.op+ quote [[a] y]
              [(gensym "?!") (qq (eq ~y '~a))]))

(!! #_(env.members.tests:do)
    (eq (env.mk {'foo 42})
        (env.mk {'foo:val 42})
        (env.mk {'foo {:val 42}})))

(E+ env
    {
     mk
     (f_ (env.merge.members env0 _))

     members
     ["the env type has a :members field,
      containing a nested map :: {path members}|{keyword any}
      this module contains some utilities to transform such thing

      used as a function, given an env, returns its members"

      (f_ (c/get _ :members))

      leaf?
      (fn [x]
        (and (map? x)
             (?$ (idxs x) key?)))

      normalize
      [(cf
        [x]
        (red {}
             (f [a [k v]]
                (cp k
                    key? (sip a [k v])
                    sym? (rec a [(path k) v])
                    path?
                    (let [p (path k)]
                      (if (ppath? p)
                          ()))))
             x)
        [x at]
        ())

       entry
       (f1 [k v]
           (cp k
               key?
               sym?
               path?
               (case (path-segments k)
                 (tup x)
                 [[_ss . ls] ]
                 (tack (_ss) {} v))))]

      at
      (f [ms at]
         (get-in ms (path-segments at)))

      leaves
      [(cf
        ;; api
        [(:map m)] (rec m root-path [])
        [(:map m) (path at)] (rec (members.at m at) at [])

        ;; impl
        [(pure? ms) at ret] ret
        [(cons [k v] ms) at ret]
        (let []
          (rec ms at
               (cp k
                   key? (sip ret [(path at k) v])
                   sym? (+ ret (rec v (path at k) []))))))

       (tests
        (eq (env.leaves @E)
            (env.leaves @E root-path)
            (env.members.leaves (env.members @E))
            (env.members.leaves (env.members @E) root-path)))]

      leafpaths
      (+ leaves idxs)

      (tests
       :mk [(env.mk {})
            (eq (env.mk {'foo 42})
                (env.mk {'foo:val 42})
                (env.mk {'foo {:val 42}}))])

      ;:fx (members.tests:do)

      ]

     leaves
     (cf [(:env e)]
         (members.leaves (members e))
         [(:env e) (path at)]
         (members.leaves (members.at (members e) at) at))

     leafpaths
     (+ members members.leafpaths)

     subenv
     ["create a new environment from the value of 'e at 'path"
      (f [e path]
         (env.put env0 root-path
                  (env.get e path)))]

     merge
     {:val
      (f [e1 e2]
         (env.merge.members e1 (env.members e2)))

      at-current-location
      (f [e1 e2]
         (env.merge.members
          e1 (assoc-in {} (path-segments (loc e1)) e2)))

      members
      ["add members to an environment"
       (f [e ms]
          (red e
               (f [a [p v]] (env.put a p v))
               (env.members.leaves ms)))

       relatively
       (f [e x]
          (env.merge.members
           e (red {}
                  (f1 [e [k v]] (sip e [(path (loc e) k) v]))
                  (env.members.leaves x))))]}})

(E+ keep
    [
     (f [o . fs]
        (red o
             (f [o f]
                (> o ($_ f) (filt_ id)))
             fs))

     (tests

      :one
      (eq (keep [-1 2 3 -8] neg?)
          [-1 -8]))])

(E+ testing
    {
     all-tests
     [(f [e]
         (red {}
              (f [m [p v]]
                 (?let
                  [[. _ 'tests :do] (lst* (path-segments p))]
                  (sip m [p v])))
              (env.leaves e)))
      :demo
      (__ (!! (testing.all-tests @E)))]

     run-all:mac
     [(fn [e _]
        ($+ (iter (all-tests e))
            (f1 [k v] {(sq '~(path->sym k)) (v)})))
      :demo
      (__ (!! (testing.run-all)))]

     tests
     [:upd
      (fn [e xs]
        (let [xs (vec* (assertion.vec-split xs))]
          {'tests
           {:form (qq '~xs)
            :do
            (qq (fn []
                  .~($ xs
                       (fn [x]
                         (qq (assert ~x '~(path->sym (loc e))))))
                  ::ok))}}))
      :demo
      (__

       (E+ toto
           (tests

            :one
            (eq 43 (inc 42))

            :two
            "comment comment omment"
            (eq 3 (add 2 1))))

       (!! (toto.tests:do))
       (ppenv toto.tests))]})

(E+ local-env-expansion

    ["an experiment around macros that have a local environment
      the idea is to give some macros the opportunity to merge a local environment at current location before expanding"

     :upd
     (fn [e [local-env [esym args :as argv] expr]]
       (pp local-env)
       {'local-env local-env
        :mac (sq (f ~argv
                    (let [~esym
                          (env.merge.members.relatively
                           ~esym (env.get
                                  ~esym
                                  '~(path->sym (path (loc e) 'local-env))))]
                      ~expr)))})

     :demo
     (__

      ;; the update expansion
      (updxp
       (local-env-expansion
        {bar:mac (fn [e xs] (cons :bar (exp e (vec* xs))))
         nest {foo:mac (fn [e xs] (cons :foo (exp e (vec* xs))))}}
        [ee [x]] (exp ee x)))

      ;; an exemple
      (E+ foomic
          (local-env-expansion
           ;; the local env
           {bar:mac (fn [e xs] (cons :bar (exp e (vec* xs))))
            nest {foo:mac (fn [e xs] (cons :foo (exp e (vec* xs))))}}
           ;; the argv of the expansion function 
           [ee [x]]
           ;; the expansiion function body
           (exp ee x)))

      (ppenv foomic)

      (is (exp @E (foomic (bar 1 2 (bar 3) (nest.foo 2 3))))
          [:bar 1 2 [:bar 3] [:foo 2 3]])

      )])

(E+ map
    {addresses
     (fn [x]
       )})

(do :flath

            (declare flath_exp)

            (defn flath_exp-entry [[p v]]
              (cs [p (path p)]
                  (assoc-in {} (path-segments p) (flath_exp v))))

            (defn flath_exp [x]
              (if (map? x)
                ($ x flath_exp-entry)
                x))

            (defn flath_flatseq
              ([x] (flath_flatseq x []))
              ([x from]
               (if (map? x)
                 (mapcat (fn [[k v]] (flath_flatseq v (conj from k))) x)
                 [[from x]])))

            (defn flath_normalize [m]
              (->> m flath_exp flath_flatseq (into {})))

            (defn flath
              ([] {})
              ([& xs]
               (c/let [[x & [y & z :as xs]] xs]
                 (cp x
                     map? (mrg (flath_normalize x) (apl flath xs))
                     word? (apl flath {x y} z)
                     (throw (Exception. (str "what? " x)))))))

            (asserts

             (flath
              'a {'b {'c.d.e 1}})

             ($keys (flath '{c {a 1 a.b {m 1 p.o 5}}})
                    path*)

             

             (flath
              {:a 1
               'a.m.p {:b 2}
               'c {'d 3
                   :op {:iop 1}
                   'e {:p 2}}})))

(E+ pathmap
    
    ;; impl

    [normalize
     (cf
      [x] (+ {} (rec x []))
      [x from]
      (if (map? x)
          (red [] (f [a [k v]] (+ a (normalize v (sip from k)))) x)
          [[from x]]))

     preprocess
     (f1 [x . (& xs [y . z])]
         (cp x
             map? (merge (normalize x) (rec xs))
             word? (rec (cons {x y} z))
             nil? (when (cons? xs) (rec xs))
             (error  "map.addresses :: bad input:\n" x)))

     flat-addresses
     (f1 m
         (map . (zip (f1 [k v] [($+ k (+ path path-segments)) v]) m)))

     pathify-keys
     [mkey-split (p c/split-with sym?)

      (f1 m
          (red {}
               (f1 [k v]
                   (let [[syms kws] (mkey-split k)]
                     ))
               m))]

     ;; main

     (f xs
        (flat-addresses (preprocess xs)))

     #_(check
      (eq (map.addresses :a 1 'a.b 2)
          (map.addresses {:a 1 'a.b 2})
          (map.addresses {:a 1 'a {'b 2}})
          {[:a] 1 '[a b] 2})

      )]
    )

(do :misc
 (ppenv map)

 (!! (map [:a 1] [:b 2]))
 (!! )

 (!! (map . (zip (f1 [k v] [k v]) {:a 1 :b 2})))
 (!! ($i+ {:a 1 :b 2} (f [k v] [k v])))

 (let [(& xs [x . y]) ()] [xs x y])

 (!! (map.addresses :a 1 'a.b 2))

 (!! (map.addresses
      {:a 1
       'a.m.p {:b 2}
       'c {'d 3
           :op {:iop 1}
           'e {:p 2}}}
                                        ;[]
      ))

 (E- foo)
 (E+ foo (f [] :ho!)
     )

 (ppenv foo)
 (!! (foo))
 (!! (foo.bob))
 (E+ foo [bob [iop 1 (f [] :foobob)]])

 (!! (bindings.bind 'x 'y))
 (!! (bindings.bind '[a b c . d e] 'y))
 (!! (bindings.bind '(& m {:a a}) 'y))

 (!! (eq nil nil nil))

 (exp @E '(let
              [(& mymap (ks a b)) {:a 1, :b 2, :c 3}]
              [mymap a b]))
 )

(do :bindings
    (E+ bindings.let
        {
         remove-mode-prefix
         (fn [s]
           (->> (str s)
                (c/drop-while #{\? \!})
                (apl str)
                symbol))

         mode
         {
          from-sym
          ["s: symbol
        turn a prefixed into a modesym or return nil for non-prefixed syms"
           (fn [s]
             (or (maybe-strict-sym? s)
                 (condp = (car (str s))
                   \! :strict
                   \? :short
                   \_ :opt
                   nil)))]

          maybe-strict?
          ["when a symbol starts by ?! it indicates that the resulting binding mode
        cannot be :opt, if current mode is strict it will be strict else it will be :short"
           (fn [s]
             (and (= '(\? \!) (c/take 2 (str s)))
                  :maybe-strict))]

          next
          ["m1: modesym s: symbol
        return the mode to use for binding s"
           (fn [m1 s]
             (let [m2 (from-sym s)]
               (cs (= :maybe-strict m2)
                   (cs (= :opt m1) :short m1)
                   m2 m2
                   m1)))]

          to-num:val {:opt 0 :short 1 :strict 2}
          compare (fn [m1 m2] (c/compare (to-num m1) (to-num m2)))
          gte (fn [m1 m2] (#{0 1} (compare m1 m2)))
          lte (fn [m1 m2] (#{0 -1} (compare m1 m2)))
          eq (fn [m1 m2] (zero? (compare m1 m2)))}

         form
         (fn
           ([e bs expr mode]
            (rec e bs expr mode {}))

           ([e [p1 e1 & bs] expr mode sym->mode]
            ;; the sym->mode map is holding previoously bound syms with their corresponding mode
            (cs
             ;; no more bindings we just expand the body expression
             (not p1) (exp e expr)
             ;; if both e1 is syms we can just substitute
             ;; instead of adding a binding
             ;; we also check if p1 has not a different mode (have to think of this further)
             [? (sym? e1)
              m1 (mode.next mode p1)
              ? (mode.lte m1 (sym->mode e1))
              p1' (remove-mode-prefix p1)
              e' (env.add-sub e [p1' (exp e e1)])]
             (form e' bs expr mode
                   (assoc sym->mode p1' m1))
             ;; else we add a binding
             [p1' (remove-mode-prefix p1)
              [e' pat] (hygiene.shadow e p1')
              m1' (mode.next mode p1)]
             (step-form
              pat (exp e e1)
              (form e' bs expr mode
                    (assoc sym->mode p1' m1'))
              m1'
              ))))})

    (!! (bindings.let.form @E '[a 1 b a] '(+ a b) :strict))
    (!! (bindings.let.form @E '[a 1 ?b a] '(+ a b) :opt))
    (!! (bindings.let.form @E '[a 1 ?b a] '(+ a b) :strict))
    (!! (bindings.let.form @E '[!a 1 ?b a] '(+ a b) :opt))
    (!! (bindings.let.form @E '[!a 1 !b a] '(+ a b) :opt))
    (!! (bindings.let.form @E '[?a 1 b a] '(+ a b) :strict))

    (updxp (type+ :pierre [a b]))

    (E+ (type+ :pierre [a b]))

    (let [(pierre [x . xs] y) (pierre (range 10) 42)]
      [x xs y])

    (bind (list '& 'a 'b) 'x)
    )

(do :org-script

    (import '(java.io BufferedReader StringReader))

    (spit "src/asparagus/tutorial-without-comment.clj"
          (apply str
                 (interpose "\n"
                            (mapv (fn [line]
                                    (if (re-matches #"^ *;;.*" line)
                                      (-> line
                                          (str/replace #"\"" "\\\"")
                                          (str/replace #";; *" "\"")
                                          (str "\""))
                                      line))
                                  (line-seq (BufferedReader. (StringReader. (slurp "src/asparagus/tutorial.clj"))))))))
    )

(do :instaparse

    (require '[instaparse.core :as insta])

    (def parser
      (insta/parser
       "expr = number | vector | operation
    operation = operator space+ vector
    operator = '+' | '-' | '*' | '/'
    vector = snumber+ number
    <snumber> = (number space)*
    <space> = <#'[ ]+'>
    number = #'[0-9]+'"))

    (parser "+ 1 2 3 4")

    (defn choose-operator [op]
      (case op
        "+" +
        "-" -
        "*" *
        "/" /))

    (def transform-options
      {:number read-string
       :vector (comp vec list)
       :operator choose-operator
       :operation apply
       :expr identity})

    (defn parse [input]
      (->> (parser input) (insta/transform transform-options)))

    (parse "+ 1 2 3 4")                 ;=> 10

    )
