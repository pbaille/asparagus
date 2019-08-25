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

(E+ env
    {
     members
     {:val (f_ (c/get _ :members))

      pathmap
      (cf [e] ($+ (leaves e) (f_ {_ (env.get e _)}))
          [e at]
          (> (leaves e)
             (filt_ (p parent-path? at))
             ($+_ (f_ {_ (env.get e _)}))))}

     leaves
     (cf [e] (rec (members e) root-path [])
         [(pure? ms) at ret] ret
         [(cons [k v] ms) at ret]
         (rec ms at
              (cp k
                  key? (sip ret (path at k))
                  sym? (+ ret (rec v (path at k) [])))))

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
       (f [e1 e2]
         (red e1
              (f [a [p v]] (env.put a (path p) v))
              e2))

       relatively
       (f [e x]
          (env.merge.members
           e ($+ (iter (env.members.pathmap (env.mk x)))
                 (f1 [k v] {(path (loc e) k) v}))))]}

     mk
     (f_ (env.merge.members env0 _))})

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
         ($+ (env.leaves e)
             (f1 p
                 (clet
                  [[. _ 'tests :do] (lst* (path-segments p))]
                  {p (env.get e p)}
                  {}))))
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


