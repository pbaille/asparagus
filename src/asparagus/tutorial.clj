(do "Introduction"

  ; Asparagus is my latest experience in language design.
  ; After several attempt, i've compiled some of the ideas I like, found in other languages/books/papers.

  ; It is embedded in clojure, but at the same time is quite far from it.
  ; It has its own environment/namespaces mechanism, and another kind of macro system.
  ; It is far from being production ready, but I hope that it can be interesting as an experiment for some of you.

  ; I'm an autodidact with no formal computer science degree, my approach to programming is quite empirical.
  ; First of all I'm seeking for feedback, advices, discussions and people to work with.

  ; For now the main goals are: 

  ; - flexibility
  ; - extensibility
  ; - expressivity
  ; - performance (close to clojure)

  ; The main ways/devices to achieve them:

  ; - (environment/expansion)-passing-style macros (a more general/powerful macro system)
  ; - pattern matching (built in to lambdas and binding-forms, extensible binding semantics)
  ; - functional control flow (heavy use of function composition and guards, nil-based short-cutting constructs)
  ; - generic functions at heart (all core operations of the language will be implementable by user types and implemented meaningfully by core types)
  ; - holy data structure literals (maps, vectors, lists and sets) at the heart (giving them more usages and extra syntax sugar)

  ; This tutorial assumes familiarity with clojure, or at least another Lisp.

  ; In order to be able to evaluate the forms contained in this file,
  ; You should start a REPL and load =asparagus.core=.

  (load-file "src/asparagus/core.clj")
  (in-ns 'asparagus.core)

)

(do "Environment"

  ; The asparagus environment is held by the =asparagus.core/E= atom

  @E

  (do "simple definitions"

    ; You can define a variable like this (=E+= stands for extend-environment).
    ; Its like a really (really!) fancy =def=.

    (E+ foo 1)

    ; Or several at once.

    (E+ bar \a
        baz 42)

    ; For now we will use =!!= macro to evaluate forms (it is kind of ugly...), but later it will no longer be needed.

    (!! foo) ;; return the value under foo e.g 1

    ; The =is= macro just assert equality of its arguments.

    (is 1 foo)
    (is 42 baz)
    (is \a bar)

    ; You can modularize definitions (one of the motivational point of all this).
    ; Here we use a hashmap literal to define several variables in 'mymodule and =mymodule.c=.

    (E+ mymodule
        {a 1
         b "hey"
         c {d 42
            e :pouet}})

    ; We can achieve the same with vector litteral syntax.
    ; The semantic difference between hash and vec literals will be explained later.

    (E+ mymodule
        [a 1
         b "hey"
         c [d 42
            e :pouet]])

    ; We use dot notation to access nested environment members

    (is 43
        (add mymodule.a mymodule.c.d)) ;; will add 1 and 42

    ; Dot notation can be used in definitions too.

    (E+ mymodule.c.f 2
        foo.bar 'foob)

    ; This definition does not override our previous ones, =foo= is still defined.

    (is 1 foo)
    (is 'foob foo.bar)

    ; One handy usage of this behavior is scoped helpers definition.

    (E+

     ;; our intent is to implement a stat function that takes any number of numeric arguments and return a map holding some statistics

     ;; first we are defining some helpers, that will be scoped under the stats identifier
     stats.sum
     (fn [xs] (apl add xs))
     stats.mean
     (fn [xs] (div (stats.sum xs) (count xs)))

     ;; then we are defining the main implementation with the help of the above definitions
     stats
     (fn [& xs]
       {:xs xs
        :sum (stats.sum xs)
        :mean (stats.mean xs)}))

    ; In my clojure practice I was often annoyed to put stats.sum and stats.mean at the same level than stats.
    ; Certainly I can create a stats namespace holding those helpers, but... it seems heavy for such a common/natural thing...

    (is (stats 1 2 3 4)
        {:xs '(1 2 3 4), :sum 10, :mean 5/2})

    ; It could be defined with a map literal too.

    (E+ stats
        {;; for now i've hidden an important detail,
         ;; each identifier can have any number of what we will call attributes (or meta-keys, not really sure about the naming yet...)
         ;; attributes are stored and accessible using clojure keywords
         ;; for instance an identifier 'foo can have an attribute :size
         ;; it would be defined like this (E+ foo:size 3) and accessed like this 'foo:size, simple enough...

         ;; one of those attributes, that is systematically used under the hood is the :val attribute
         ;; :val hold the main value of the current identifier (here 'stats)
         ;; if the identifier 'stats' appears as is in the code this is the value we are refering to

         ;; note that the sum and mean helpers function (defined after) are available
         ;; when using map literal for definition, all members are available to each others
         :val
         (fn [& xs]
           {:xs xs
            :sum (.sum xs) ;; relative access, more on this later...
            :mean (.mean xs)
            })

         ;; helper submodules
         sum
         (fn [xs] (apl add xs))
         mean
         (fn [xs] (div (..sum xs) (count xs)))})

    ; The =:val= thing is implicit in most cases.
    ; Those three forms are equivalent:

    (E+ myval {:val 1})
    (E+ myval:val 1)
    (E+ myval 1)

    ; Any environment variable can have any number of those attributes.

    (E+ stats
        {:doc "a functions that takes some numbers and do some statistics on it"
         :version 0.1
         :tags #{:math}
         :foo :bar})

    ; They can be referred in code with colon notation.

    (is stats:doc
        "a functions that takes some numbers and do some statistics on it")

    (is stats stats:val)

    ; We also could have used vector syntax to define stats.

    (E+ stats
        [;; in vector literal definitions occurs sequentially
         ;; so we have to define helpers before 
         sum
         (fn [xs] (apl add xs))
         mean
         (fn [xs] (div (..sum xs) (count xs))) ;; once again ..sum is relative environment access, more later

         ;; here the :val of stats (the :val keyword can be omitted)
         (fn [& xs]
           {:xs xs
            :sum (.sum xs)
            :mean (.mean xs)})])

    ; In =E+=, top level's strings literals represent documentation (a bold choice maybe...).
    ; But I've said to myself, maybe hard-coded string in code are not so common? (at least at the top level) far less than keywords for instance.

    (E+ myvar
        ["myvar doc" 42])

    ;;  is equivalent to
    (E+ myvar {:val 42 :doc "myvar doc"})

    (is  "myvar doc"
         myvar:doc)

    ; Finally we can redefine stats with doc litterals.

    (E+ stats
        [sum
         (fn [xs] (apl add xs))

         mean
         ["given a seq of numbers, return the mean of it"
          (fn [xs] (div (..sum xs) (count xs)))]

         "returns a map of statistics concerning given numbers"
         (fn [& xs]
           {:xs xs
            :sum (.sum xs)
            :mean (.mean xs)})])

    ; So you may have a question now :)
    ; If hashmaps, vectors and strings have special semantics in =E+=,
    ; How can I use them as normal values for my variables?!

    ; The answer is the =:val= field.

    (E+ rawvals
        [h:val {:a 1 :b 2}
         v:val [1 2 3]
         s:val "iop"])

    (is {:a 1 :b 2} rawvals.h)
    (is "iop" rawvals.s)

    ; One thing that may have intrigued you is relative environment member accesses. 
    ; e.g =.sum=, =.mean= and =..sum= (in the stats previous definition)

    (E+ relative-access
        {demo1
         {a
          (fn []
            ;; we are resolving b and c in the parent module
            (add ..b ..c))
          b 1
          c 2}

         demo2
         {:val
          (fn [x]
            ;; the :val field is at the current module level
            ;; so we only need one dot here (meaning, 'in the current module')
            (add .b .c x))
          b 3
          c 4}

         demo3
         (fn [x]
           (add (..demo2 x)
                ;; relative dotted
                ..demo1.c))})

    (is (relative-access.demo1.a) 3)
    (is (relative-access.demo2 5) 12)
    (is (relative-access.demo3 9) 18)

    ; You may wonder about interop... it is not supported for now, more thinking is required on that matter.
    ; At those early stages I thought that the core design is the main focus,
    ; Asparagus is not at the get-the-things-done stage for now ;)

)

  (do "Bubbling resolution"

    ; Using absolute and relative paths for all our vars is kind of painfull and ugly.
    ; Sometimes it is needed to disambiguate but certainly not all the time.
    ; When a symbol cannot be resolved at the current level, it will be searched bubbling up the environment.


    (E+ bubbling.demo
        {a 1
         b.c
         (fn []
           ;; here 'a will be resolved bubbling up the environment
           ;; in this case it will be resolved to bubbling.demo.a
           a)
         c
         {a 2
          b
          (fn []
            ;; here it will be resolved to bubbling.demo.c.a
            a)}}
        )

    (is 1 (bubbling.demo.b.c))
    (is 2 (bubbling.demo.c.b))


)

  (do "Links"

    ; The =:links= attribute let you define shorter accesses to other modules or members.
    ; When a non relative symbol cannot be resolved at the current location,
    ; its first segment will be searched in the current module links.
    ; If there is an existent link it will be substituted by it.
    ; If there is no link at the current level, we go up (bubbling) and loop, until root.

    ; #+begin_src  clojure

    ; (E+ links.demo
    ;     {mod1 {a 1 b 2 c {d 3 e 4}} ;; a bunch of things that we will link to

    ;      mod2
    ;      {:links {m1 links.demo.mod1
    ;               m1c links.demo.mod1.c
    ;               bub bubbling.demo} ;; <- defined in previous section
    ;       f
    ;       (fn []
    ;         ;; here m1.a will be substituted by links.demo.mod1.a
    ;         ;; and m1c.d by links.demo.mod1.c.d
    ;         (add m1.a m1c.d bub.a))}})

    ; (is (links.demo.mod2.f) 5)

    ; ;; with this we can achieve some of the things we do with :require and :use in clojure ns's form
    ; ;; it will not be oftenly used directly, but will be used under the hood by higher level macros...


)

  (do "=E-="

    ; You can remove global environment's members with =E-=

    ; #+begin_src  clojure 

    ; (E-
    ;  foo bar baz my.module
    ;  stats myval myvar rawvals relative-access bubbling.demo links.demo)

    ; ;; it no longer exists
    ; (isnt (env.get @E 'relative-access))


))

(do "Data Primitives"

  (do "Literals"

    ; literals works the same way as clojure ones (except for some extensions that will be explained later)

    {:a 1}
    [1 2 3]
    '(1 2 3)
    #{1 2}
    "hello"
    :iop
    'mysym
    \A
    42
    1.8
    1e-7

)

  (do "Collections"

    (do "Constructor functions"

      ; Compared to clojure, the API have been uniformized

      (is (vec 1 2 3) [1 2 3])
      (is (lst 1 2 3) '(1 2 3))
      (is (set 1 2 3) #{1 2 3})
      (is (map [:a 1] [:b 2]) {:a 1 :b 2})

      ; With sequential last argument (like =core/list*=).

      (is (vec* (lst 1 2 3 4)) ;; with one argument it behaves like core.vec
          (vec* 1 2 [3 4])
          [1 2 3 4]) 

      (is (lst* [1 2 3 4])
          (lst* 1 2 [3 4])
          (lst* 1 2 3 4 [])
          '(1 2 3 4))

      (is #{1 2 3 4}
          (set* 1 2 [3 4]))

      (is (map* [:a 1] [:b 2] {:c 3 :d 4})
          (map* [:a 1] [[:b 2] [:c 3] [:d 4]])
          {:a 1 :b 2 :c 3 :d 4})

)

    (do "Guards"

      ; Each collection have its guard, that returns the given collection on success or nil otherwise.

      (is (vec? [1 2 3]) [1 2 3])
      (is (lst? (lst 1 2 3)) (lst 1 2 3))
      (is (set? #{1 2 3}) #{1 2 3})
      (is (map? {:a 1}) {:a 1})

      ; We will see that in asparagus we avoid predicates (functions that returns booleans)
      ; in favor of guards (functions that can return nil indicating failure, or data).
      ; For instance (pos? 1) may be more useful if it returns 1 in case of success and nil otherwise.
      ; This way it can be composed more easily I think.
      ; More on control flow, short-circuiting and stuff later...

))

  (do "Words"

    (do "Constructors "

      ; Symbols and keywords have their core/str(ish) constructors

      (is (sym "foo") 'foo)
      (is (key "foo") :foo)

      (is (sym :foo "bar") 'foobar)
      (is (key "foo" :bar "baz") :foobarbaz)

      (do "Star variants"

        (is (sym* "ab" (lst "cd" "ef" "gh"))
            'abcdefgh)
        (is (key* "my" :keyword "_" [:foo :bar "baz"])
            :mykeyword_foobarbaz)
        (is (str* "mystr_" ["a" "b"])
            "mystr_ab")

))

    (do "Guards"

      ; As for collections, we use guards instead of preds

      (is (key? :iop) :iop)
      (is (sym? 'bob) 'bob)
      (is (str? "hi") "hi")

)))

(do "Joining"

  (do "=+= (concat(ish)) "

    ; =+= is joining things together.

    ; As I mentioned in the rational, core operations are generic functions that can be extended. 
    ; =+= is one of them

    (do "Collections"

      (is (+ [1 2] '(3 4))
          [1 2 3 4])

      (is (+ (lst 1 2) [3 4])
          '(1 2 3 4))

      (is (+ {:a 1 :b 0} {:b 2})
          {:a 1 :b 2})

      ;; + is variadic
      (is (+ #{} (lst 1 2) [3 4] #{3 5})
          #{1 2 3 4 5})

      ; As you have seen, the return type is determined by the first argument.

)

    (do "Strings, symbols and keywords"

      (is (+ 'foo "bar") 'foobar)
      (is (+ :foo 'bar) :foobar)
      (is (+ "foo" 'bar :baz) "foobar:baz")

)

    (do "Functions "

      ; On functions it do composition (left to right, not like core.comp do).

      (is ((+ inc inc (p mul 2)) 0)
          4)

))

  (do "=sip= "

    ; =sip= adds one or several element into something

    (is (sip [] 1 2)
        [1 2])

    ; For lists it adds at the end (not like =core/conj= do)
    ; It is a choice that can be discussed, in my own practice i'm not relying often on way that clojure lists implements conj
    ; =sip= being a generic operation (extendable by user types) we could add a datatype that conj elements at its head like clojure lists...

    (is (sip (lst 1 2) 3)
        '(1 2 3))

    (is (sip #{3 4} 1 2)
        #{1 2 3 4})

    ; For maps it works on entries.

    (is (sip {:a 1} [:b 2] [:c 3])
        {:a 1 :b 2 :c 3})

    ; For function it partially apply given args.
    ; (i'm not sure it should behave that way, it's more like an experimental fantasy that is not used in core code)

    (is ((sip add 1 2) 3)
        6)


)

  (do "=pure= "

    ; Returns the empty version of the given argument.


    (is (pure "foobar") "")

    (is (pure {:a 1 :b 2}) {})

    (is (pure inc) id)

    ; Like =sip= and =+=, =pure= is a generic function.

)

  (do "=pure?= "

    ; Tests for purity.

    (is {} (pure? {}))

    (isnt (pure? {:a 1}))

))

(do "Composing"

  ; I've always wondered why splicing was available only at certain places, it should be so handy to be able to use it anywhere...

  (do "Vectors "

    (let [a 1
          b 2
          c [3 4]
          d [5 6]]

      ;; with a dot you can do splicing
      (is [a b . c] [1 2 3 4])
      ;; the spliced part can be anywhere
      (is [a b . c b a] [1 2 3 4 2 1])
      ;; several spliced parts
      (is [a b . c . d] [1 2 3 4 5 6])
      ;; shortcut (everything after the double dot is spliced)
      (is [a b .. c d] [1 2 3 4 5 6])
      ;; nested
      (is [a b [42 . d] . c]
          [1 2 [42 5 6] 3 4]))

)

  (do "Maps"

    (let [a {:a 1}
          b {:b 2}
          c [1 2 3]]

      (is {:a 1
           :c 3
           . b} ;; we are merging b into the host map

          ;; if you want to splice several map into your literal use .. []
          {:c 3
           .. [a b]}

          {:a 1 :b 2 :c 3})

      ;; it can be nested

      (is
       {:foo [0 . c 4] ;; a composite vector
        :bar {:baz 1 . b}
        . a}

       {:foo [0 1 2 3 4]
        :bar {:baz 1 :b 2}
        :a 1})
      )

)

  (do "Lists "

    (let [nums [2 3 4]]

      ;; in conjunction with 'lst you can do the same things that we have shown with vectors
      (is (lst 1 . nums)
          (lst 1 2 3 4))

      ;; but more interesting is this
      ;; you can achieve apply semantics with dot notation
      (is (add 1 . nums)
          (c/apply add 1 nums)
          10)

      ;; but unlike with apply it does not have to be the last argument that is a collection
      (is (add 1 . nums 5) 15)

      ;; we have doubledot also
      (is (add .. nums nums nums)
          (add . nums . nums . nums)
          27)
      )

))

(do "Binding"

  ; Asparagus has a whole family of let like binding forms.
  ; But unlike clojure's one, the binding behavior can be extended by the user in several ways.

  (do "Let"

    ; Basic usage (nothing new)

    (is (let [a 1] a)
        1)

    (is (let [a 1 b 2] (add a b))
        3)

    ;; refer earlier binding
    (is (let [a 1 b a] (add a b))
        2)

    ; Binding symbols can be prepended by special character to indicate special behavior

    (do "Short-circuiting bindings"

      ; If a binding symbol is prefixed by =?=,
      ; It has to bind to a non =nil= value else the whole =let= form is short-circuited and return =nil=

      (isnt (let [?a nil ;; this binding fail, therefore the next line will never be evaluated
                  b (error "never evaluated")] 42))

)

    (do "Strict bindings"

      ; Binding symbol's prepended by =!= must bind to non =nil= value, else an error is thrown.

      (is :catched
           (try (let [!a (pos? -1)] :never)
                (catch Exception _ :catched)))

      ; Those three modes of binding (regular (non prefixed symbols), short-circuited, strict) can be combined inside =let= forms.
      ; Resulting, I think, in much expressivity.

))

  (do "Let variants"

    (do "=?let= (short-circuiting let)"

      ; Is behaving like =let=, but the =?= prefix is implicit to all binding symbols.

      (?let [a 1 b 2] (add a b))

      ; Is equivalent to

      (let [?a 1 ?b 2] (add a b))

      ; We can use strict bindings in a =?let= form, it will behave as in =let=.

      (is :catched
          (try (?let [a 1
                      !b (pos? -1)] (add a b))
               (catch Exception _ :catched)))

      ; If we want to allow regular bindings (as normal symbols in a classic =let=)
      ; We use the =_= prefix:

      (is (?let [a 1
                 _b nil] ;; _b is bound to nil but this does not shorts
                a)
          1)

)

    (do "=!let= (strict let)"

      ; Is like =?let= but with implicit prefix =!=, it support =?= and =_= prefixes

      (is :catched
            (try (!let [a nil] :never)
                 (catch Exception _ :catched)))

)

    (do "=lut= (unified let)"

      ; In a =lut=, all symbols that appears several times have to bind to the same value (equal values), otherwise it will short-circuit.

      (is (lut [a 1 a (dec 2)] :success)
          :success)

      (isnt
       (lut [a 1
             a 2] ;; this will shorts because a is already bound to 1
            (error "never thrown")))

)

    (do "=!lut= (unified strict let)"

      (is :catched
            (try (!lut [a 1
                        a 2] ;; this will throw because a is already bound to 1
                       :never)
                 (catch Exception _ :catched)))

))

  (do "Destructuration "

    (do "Literals"


      ; Like =core/let= we support destructuration. 
      ; But unlike clojure, destructuration is an extensible mecanism. 
      ; The user can define its own destructuration special forms.

      (do "Sequential patterns"

        ; Using a vector in pattern position do the same as clojure (at first glance).

        (is (let [[a b] [1 2]] {:a a :b b})
            {:a 1 :b 2})

        ; But it is more strict.

        ; This does not pass because the seed and the pattern have different length.

        (isnt (let [[a b c] [1 2]] :ok)
              (let [[a b] [1 2 3]] :ok))

        ; Rest pattern:

        (is (let [[x . xs] (range 5)] [x xs])
            [0 (range 1 5)])

        ; In clojure the following is valid.

        (clojure.core/let [[a b] [1 2 3]] {:a a :b b}) ;; {:a 1 :b 2}

        ; The equivalent in asparagus should be written like this:

        (is (let [[a b . _] (range 10)] {:a a :b b}) ;; with the . _ we allow extra elements
            {:a 0 :b 1})
        ;; This way lambda argument patterns and let patterns behaves the same, which seems like a good thing

        ; Preserves collection type.

        (is (let [[x . xs] (vec 1 2 3)] [x xs])
            [1 [2 3]]) ;; in clojure [2 3] would be a seq

        ; Post rest pattern.

        ; In clojure the rest pattern has to be the last binding, here we can bind the last element easily.

        (is (let [[x . xs lastx] (range 6)] [x xs lastx])
            [0 (range 1 5) 5])

        ;; (we could also have bound several things after the rest pattern)
        (is (let [[x . xs y1 y2 y3] (range 6)] [x xs y1 y2 y3])
            [0 (lst 1 2) 3 4 5])

)

      (do "Map patterns"

        (is (let [{:a aval :b bval} {:a 1 :b 2 :c 3}] [aval bval])
            [1 2])

        ; In clojure the same is achieved like this (I don't really understand why).

        (c/let [{aval :a bval :b} {:a 1 :b 2 :c 3}] [aval bval])

        ; Maps have rest patterns to:

        (is (let [{:a aval . xs} {:a 1 :b 2 :c 3}] [aval xs])
            [1 {:b 2 :c 3}])

        ; As you may think, all binding modes are supported in destructuration bindings forms.

))

    (do "Operators "

      ; =ks= is a builtin binding operator.
      ; It behaves like clojure's =:keys=.

      (is (let [(ks a b) {:a 1 :b 2 :c 3}] (add a b))
          3)

      ; In a =?let= form it shorts on =nil= keys.

      (isnt (?let [(ks a) {}] (error "never"))) 

      ; =opt-ks= for keys that may not be here.

      (is "foo"
          (?let [(ks-opt foo) {:foo "foo"}] foo))

      (exp @E '(let [{:foo _foo} {}] (or foo "foo")))

      ; =ks-or= let you define default values for missing keys.

      (is "default"
          (?let [(ks-or foo "default") {}] foo))
      ;; you can use previous binding in further expressions
      (is "Bob Doe"
          (?let [(ks-or firstname "John"
                        lastname "Doe"
                        fullname (+ firstname " " lastname)) ;; <- here
                 {:firstname "Bob"}]
                fullname))

      ; =&= (parallel bindings)

      ; Several patterns can be bound to the same seed. 
      ; Something that i've sometimes missed in clojure (lightly).

      (is (?let [(& mymap
                    (ks a b)
                    (ks-opt c)
                    (ks-or d 42))
                 {:a 1 :b 2 :c 3}]
                [mymap a b c d])
          [{:a 1 :b 2 :c 3} 1 2 3 42])

      ; =cons=

      (is (let [(cons a b) [1 2 3]] [a b])
          [1 [2 3]])

      ; =quote=

      (is (let ['iop 'iop] :ok)
          :ok)
      (is (let [['foo :bar . xs] '[foo :bar 1 2 3]] xs)
          [1 2 3])
      (is :ok
          (let ['(add 1 2) (lst 'add 1 2)] :ok))

      ; Some others builtin bindings exists, see source.

      (do "Defining new bindings operators"

        ;; we can extend binding ops like this

        ;; as an example we are redefining the '& operation
        (E+ (bind.op+ ks [xs seed] ;; xs are the arguments passed to the operation, seed is the expr we are binding
                      (bind (zipmap ($ xs keyword) xs) seed)))

        ;; when this operation is used
        '(let [(ks a b) x] ...)

        ;; at compile time the implementation is called with args: '(a b) and seed: 'x
        ;; =>
        '(bind {:a 'a :b 'b} 'x) ;; we are using the map impl of bind
        ;; =>
        '[G__244129 x
          G__244128 (guards.builtins.map? G__244129)
          a (clojure.core/get G__244129 :a)
          b (clojure.core/get G__244129 :b)]

        ;; finally it is substituted in the original form
        '(let [G__244129 x
               G__244128 (guards.builtins.map? G__244129)
               a (clojure.core/get G__244129 :a)
               b (clojure.core/get G__244129 :b)]
           ...)

))

    (do "Special Bindings"

      ; When a sexpr in found in binding position (left side of let bindings), if it is not a binding operator call (like we've just seen =ks= and =&= for instance), it can be what we call a 'guard pattern'.

      (do "Guard pattern"

        ; a 'guard pattern' is an expression with a binding symbol as first argument

        (is 1

            (?let [(pos? a) 1] ;; if 1 is pos then the return value of (pos? 1) which is 1 is bound to the symbol a
                  a) ;;=> 1

            ;; we could have bound the input of the guard directly to a,
            ;; but binding the return value of the guard is letting you use guards as coercing functions, which seems nice

            ;; is equivalent to
            (?let [a 1
                   a (pos? a)]
                  a))

        ; This can be a bit confusing I guess, but wait a minute. 
        ; This syntax is firstly making sense with guards that returns their first argument unchanged in case of success.

        ; In asparagus there is a semantic convention that first argument to any function is "the thing the function is working on".

        ; In OO terms the first argument is the object ('this' or 'self'). Other arguments are just parametrizing the operation.

        ; I think that observing this convention is payful because it ease function composition.

        ; As a counterexample in Clojure we often have mix -> and ->> because some functions are "working on" their first argument (as in asparagus)  
        ; and others (map,filter etc..) on the last, it result in less clear code i think.

        ; With this in mind, the fact that we bound the return value of the guard to the symbol that is in first argument position ('object position' we could say) makes a little more sense I guess.
        ; And last but not least, by behaving this way, guard patterns can serve as a way to coerce input data (seed).

        ; Disclaimer: someone that I trust has said to me that in the "data world" the convention is that the flowing data is the last argument, so... :)  

        (is 4
            (?let [(gt a 3) 4] ;; guards can have more than one arg
                  a))

        (isnt
         (?let [(gt a 3) 2]              ;; shorts
               (error "never touched")))

)

      (do "Type guards"

        ; A sexpr starting with a type keyword (see =asparagus.boot.types=) indicates a type guard pattern

        (is [1 2 3]
            (?let [(:vec v) [1 2 3]]
                  v))

        (isnt
         (?let [(:seq v) [1 2 3]]
               (error "never"))) ;;=> nil

))

    (do "Value Patterns"

      ; Any value can be used in pattern position,

      (is :ok (let [a (inc 2)
                    3 a] ;; 3 is in binding position, therefore the seed (a) is tested for equality against it, and it shorts if it fails
                :ok))

      (isnt
       (let [a (inc 2)
             4 a]
         (error "never")))

      ;; some tests

      (is :ok
          (let [42 42] :ok)
          (?let [42 42] :ok)
          (!let [42 42] :ok))

      (isnt 
       (let [42 43] :ok)
       (?let [42 43] :ok))

      (!! (throws (!let [42 43] :ok)))

))

  (do "=clet= (Cased Let) "

    ; =clet= is like a cascade of =?let= (short-circuiting let) forms. 
    ; It can be be compared to =cond-let= but is more powerful.

    (is (clet [x (pos? -1)] {:pos x}      ;first case
              [x (neg? -1)] {:neg x}      ;second case
              )
        {:neg -1})

    ; Each binding block can have several bindings.

    (let [f (fn [seed]
              (clet [x (num? seed) x++ (inc x)] x++
                    [x (str? seed) xbang (+ x "!")] xbang))]
      (is 2 (f 1))
      (is "yo!" (f "yo"))
      (isnt (f :pop)))

    ; Default case:

    (is (clet [x (pos? 0) n (error "never touched")] :pos
              [x (neg? 0) n (error "never touched")] :neg
              :nomatch)
        :nomatch)

    (do "=!clet= (strist version) "

      (throws
       (!clet [x (pos? 0)] :pos
              [x (neg? 0)] :neg))

)

    (do "=clut= (unified version) "
      (let [f (fn [seed]
                (clut [[a a] seed] :eq
                      [[a b] seed] :neq))]
        (is :eq (f [1 1]))
        (is :neq (f [1 2])))

)

    (do "=!clut= (unified strict version) "

      (let [x [:tup [1 2]]]
        (throws
         (!clut [[:wat a] x] :nop
                [(:vec vx) x [:tup [a a]] vx] :yep)))

      (let [p [:point 0 2]]
        (clet [[:point x 0] p] :y0
              [[:point 0 y] p] :x0
              [[:point x y] p] [x y]))

))

  (do "=let :rec= (loop)"

    ; let can be given a name (here :rec) in order to loop

    (is (let :rec [ret 0 [x . xs] (range 10)]
                (if (pure? xs) ret
                    (rec (add ret x) xs)))
           36)

)

  (do "=case= "


    (let [x (range 12)]
      ;; try those values:  42 "iop" :pouet
      (case x
        (num? x) {:num x}         ;; first clause, x is a number
        (str? x) {:str x}         ;; second clause, x is a string
        [x . xs] {:car x :cdr xs} ;; third clause, x is sequential
        :nomatch))

    (do "=casu= (unified version)"

      (let [t (f [x]
                 (casu x
                       [:point x 0] :y0
                       [:point 0 y] :x0
                       [:point (:num x) (:num x)] :twin
                       [:point (:num x) (:num y)] [x y]
                       :pouet))]
        (is :y0 (t [:point 1 0]))
        (is :x0 (t [:point 0 1]))
        (is :twin (t [:point 1 1]))
        (is [1 2] (t [:point 1 2]))
        (is :pouet (t [:point 1 "io"])))

)

    (do "=!case= and =!casu= (strict versions)"

      ; Throws if nothing match the input.

      (let [x 1]
        (throws
         (!case x
                (str? x) :str
                (vec? x) :vec)))

)

    (do "Misc"

      ; With the help of =case_= we can rewrite the first exemple more concisely.

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

      ; We can put guard symbols in pattern position.

      (case :zer ;42 ;'zer ;"iop"
        num? :num ;; is equivalent to: (num? x) :num
        str? :hey
        (:sym x) x
        :nope)

      (let [t (case_
               num? _
               str? _
               :pouet)]
        [(t 1)
         (t "iop")
         (t :iop)])

)))

(do "Lambdas "

  (do "=f= (lambda)"

    ; All the binding forms that we have seen so far have their lambda equivalent.

    ; Regular monoarity lambda:

    (let [fun (f [a b] (add a b))]
      (is 3 (fun 1 2)))

    ; Variadic syntax:

    (let [fun (f [x . xs] (add x . xs))]
      (is 10 (fun 1 2 3 4)))

    ; All binding patterns are available:

    (let [fun (f [x (ks a b)]
                 (+ x {:a a :b b}))]
      (is (fun {:foo 1 :bar 2}
               {:a 1 :b 2 :c 3})
          {:foo 1, :bar 2, :a 1, :b 2}))

    (let [fun (f [(& x [x1 . xs])
                  (& y [y1 . ys])]
                 {:x x :y y :cars [x1 y1] :cdrs [xs ys]})]
      (is
       (fun [1 2 3 4] [7 8 9])
       {:x [1 2 3 4],
        :y [7 8 9],
        :cars [1 7],
        :cdrs [[2 3 4] [8 9]]}))

    ; Like =let=, different binding modes are available via prefix syntax.

    (let [fun (f [!a ?b] (lst a b))] ;; a is mandatory, and b can short the execution
      (is (fun 1 2) (lst 1 2))
      (isnt (fun 1 nil))
      (throws (fun nil 2)))

    ; For recursion, like =clojure/fn= we can give a name to a lambda (we use keyword litteral to indicate a name)

    (let [g (f :mylambda [x . xs]
               (if-not (c/seq xs) x
                       (add x (mylambda . xs))))]
      (is (g 1 2 3 4) 10))

    ; The same can be acheive with =rec=

    (let [g (f [x . xs]
               (if-not (c/seq xs) x
                       (add x (rec . xs))))]
      (is (g 1 2 3 4) 10))

    ; Like in scheme, binding pattern can be a simple symbol, 
    ; this is the reason why we need keyword litteral to name lambdas (to disambiguate).

    (let [g (f xs (add . xs))]
      (is (g  1 2 3 4) 10))

)

  (do "Variants "

    ; Like let, =f= has its binding mode variants, =?f=, =!f=

    (let [fun (?f [(vec? a) (num? b)] ;; this is guard patterns (see previous section)
                  (sip a b))]
      ;; the binding succeed
      (is (fun [1 2 3] 4) [1 2 3 4])
      ;; first arg is not a vector so it shorts
      (isnt (fun 1 2)))

    ; And also unified variants: =fu= and =!fu=

    (let [fun (fu [a b a] :ok)]
      (is (fun 1 0 1) :ok)
      (isnt (fun 1 2 3)))

    (let [fun (!fu [a a] :ok)]
      (is (fun 1 1) :ok)
      (throws (fun 1 2)))

)

  (do "Syntactic sugar"

    (do "=f1= (arity 1 syntax)"

      ; Functions that takes one argument are so common that it deserves, i think, some syntactic sugar.

      (let [double (f1 a (add a a))]
        (is (double 2) 4))

      ; You can use any binding pattern:

      (let [fun (f1 (:vec a) (+ a a))] ;; we use a type guard (check if the given arg is a vector)
        (is (fun [1 2 3]) [1 2 3 1 2 3])
        (isnt (fun 42)))

      ; It has all the common variations: =!f1=, =?f1=, =!fu1= and =fu1= that do what you should expect (if you have not skip previous parts of this file)

      ; We also have =f_= that is a bit more concise than =f1=, if you don't need destructuring.

      (let [double (f_ (add _ _))]
        (is (double 2) 4))

      ; It also have common variations, =f_=, =?f_= and =!f_= (unification variants are useless here)

))

  (do "=cf= (case lambda)"

    ; The =cf= macro is a bit like clojure's =fn=, it let's you define polyarity functions, but it benefits from all asparagus binding capabilities.

    (let [fun (cf [a] 1
                  [a b] 2
                  [(:num a) b c . xs] :var1
                  [a b c . d] :var2)]
      (is (fun "iop") 1)
      (is (fun 1 2) 2)
      (is (fun 1 2 3 4 5) :var1)
      (is (fun "iop" 1 2 3) :var2))

    ; It can have several implementaion with the same arity.

    (let [fun (cf [(num? a)] {:num a}
                  [(str? a)] {:str a})]
      (is (fun 1) {:num 1})
      (is (fun "aze") {:str "aze"}))

    ; Note that variadic cases must have the same length.

    '(cf [x . xs] :one
         [x y . zs] :two) ;;compile time error

    (cf [(:vec x) . xs] :one
        [(:num x) . xs] :two) ;; is ok

    ; all previous variations are implemented: =!cf=, =?cf=, =cfu=, =!cfu=. maybe I should have considered =cf1=...

    ; You may ask yourself what is the price for this expressivity. I've worked hard on compiling those forms into performant code. 
    ; There is certainly a price for the shortcircuit, strict and unified binding modes, but probably not as high as you may expect.
    ; Sometimes it is close to bare clojure's perfs.

))

(do "Iterables"
  (do "Basic"

    ; =car= (is like Lisp's car or clojure.core/first)
    (is 1 (car (lst 1 2)))
    (is 1 (car [1 2]))
    (is [:a 1] (car {:a 1 :b 2}))

    ; =cdr= (is like clojure.core/rest but preserve collection type)
    (is (cdr [1 2 3]) [2 3])
    (is (cdr (lst 1 2 3)) (lst 2 3))
    (is (cdr {:a 1 :b 2 :c 3}) {:b 2 :c 3}) ;; on map it does not make much sense but...

    ; =last=
    (is 2 (last (lst 1 2)))
    (is 2 (last [1 2]))
    (is [:b 2] (last {:a 1 :b 2})) ;; same here...

    ; =butlast= (is like clojure.core/butlast but preserve collection type)
    (is (cdr [1 2 3]) [2 3])
    (is (cdr (lst 1 2 3)) (lst 2 3))
    (is (cdr {:a 1 :b 2 :c 3}) {:b 2 :c 3})

    ; =take= (like clojure.core/take with arguments reversed and preserving collection type)
    (is (take (lst 1 2 3) 2) (lst 1 2))
    (is (take [1 2 3] 2) [1 2])
    (is (take {:a 1 :b 2 :c 3} 2) {:a 1 :b 2})

    ; =drop=
    (is (drop (lst 1 2 3) 2) (lst 3))
    (is (drop [1 2 3] 2) [3])
    (is (drop {:a 1 :b 2 :c 3} 2) {:c 3})

    ; =takend=
    (is (takend (lst 1 2 3) 2) (lst 2 3))
    (is (takend [1 2 3] 2) [2 3])
    (is (takend {:a 1 :b 2 :c 3} 2) {:b 2 :c 3})

    ; =dropend=
    (is (dropend (lst 1 2 3) 2) (lst 1))
    (is (dropend [1 2 3] 2) [1])
    (is (dropend {:a 1 :b 2 :c 3} 2) {:a 1})

    ; =rev=
    (is (rev [1 2 3]) [3 2 1])
    (is (rev (lst 1 2 3)) (lst 3 2 1))

    ; =section= (select a subsection of a sequantial data structure)
    (is (section [1 2 3 4 5 6] 2 5) [3 4 5])
    (is (section (lst 1 2 3 4 5 6) 1 5) (lst 2 3 4 5))

    ; =splat= (split a sequential datastructure at the given index)
    (is (splat [1 2 3 4] 2) [[1 2] [3 4]])
    (is (splat (lst 1 2 3 4) 2) [(lst 1 2) (lst 3 4)])

    ; =uncs= (uncons)
    (is (uncs [1 2 3]) [1 [2 3]])
    (is (uncs (lst 1 2 3)) [1 (lst 2 3)])

    ; =runcs=
    (is (runcs [1 2 3]) [[1 2] 3])
    (is (runcs (lst 1 2 3)) [(lst 1 2) 3])

    ; =cons=
    (is (cons 1 [2 3]) [1 2 3])
    (is (cons 1 (lst 2 3)) (lst 1 2 3))
    ;; it can take more arguments
    (is (cons 0 1 [2 3]) [0 1 2 3])
    (is (cons 1 2 3 (lst)) (lst 1 2 3))

)

  (do "=map=, =reduce= and friends"

    (do "=$= (map)"

      ; Following the first argument convention we mentioned earlier,  =map= is taking the object as first argument.

      (is ($ [0 1 2] inc)
          [1 2 3])

      ; It preserves collection type.

      (is ($ #{1 2 3} inc)
          #{2 3 4})

      ; On maps it behaves differently from =clojure.core/map=, given functions are receiving only the values.

      (is ($ {:a 1 :b 2} inc)
          {:a 2 :b 3})

)

    (do "=$i= (map-indexed)"

      (is ($i [:a :b :c] (f [idx val] {:idx idx :val val}))
          [{:idx 0, :val :a}
           {:idx 1, :val :b}
           {:idx 2, :val :c}])

      ; On maps it receives key-value pairs, given functions has to return only the value.

      (is ($i {:a 1 :b 2}
              (f [idx val]
                 ;; we return the key-value pair as is
                 [idx val]))
          ;; the key-value pair has been put in value position
          ;; the keys cannot be altered with $i,
          ;; if you think about it $i on a vector or sequence cannot alter indexes,
          ;; map keys are like unordered indexes somehow, so it seems to be the correct behavior
          {:a [:a 1], :b [:b 2]})

      ; With sets, given functions receives a twin pair, which seems logical as sets can be viewed as maps with twin entries.
      ; It is pointless to use $i explicetly on a set, but in a ploymorphic context, sets have to have a meaningful implementation.

      (is ($i #{:a :b :c}
              ;; the same function we use above in the map exemple
              (f [idx val] [idx val]))
          #{[:a :a] [:b :b] [:c :c]})

      ; So now you may wonder about what we leave behing from the =clojure.core/map= behavior, in particular, =core/map= can takes several sequences.

)

    (do "=zip="

      (c/map + (range 10) (range 10)) ;;=> (0 2 4 6 8 10 12 14 16 18)

      ; In asparagus there is another function for that called =zip=. 
      ; Zipping several iterables together using the given function.

      (is (zip add (range 10) (range 10))
          (lst 0 2 4 6 8 10 12 14 16 18))

      ; Like =core.map= it is variadic.
      (is (zip add (range 10) (range 10) (range 10) (range 10))
          (lst 0 4 8 12 16 20 24 28 32 36))

)

    (do "=$+="

      ; =$+= is to =$= what =mapcat= is to =map=.

      (is ($+ (range 6) (f_ (c/repeat _ _)))
          (lst 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5))

      (is ($+ [[3 2 1 0] [6 5 4] [9 8 7]] rev)
          [0 1 2 3 4 5 6 7 8 9])

)

    (do "=$i+= "

      ; Indexed version of =$+=.

      (is ($i+ [[3 2 1 0] [6 5 4] [9 8 7]]
               (f [i v] (cons [:idx i] (rev v))))
          [[:idx 0] 0 1 2 3 [:idx 1] 4 5 6 [:idx 2] 7 8 9])

)

    (do "=zip+="

      (is (zip+ (f [a b]
                   (c/sort
                    ;; set+ makes a set from several collections
                    (set+ a b)))
                [[3 1 0] [6 5] [9 8 7]]
                [[3 2 0] [5 4] [9 7]])
          (lst 0 1 2 3 4 5 6 7 8 9))

      ; While writing this i'm considering =zipi= and =zipi+=...

)

    (do "=red="

      ; =red= is like =core/reduce= but with different argument order and variadic arity.
      ; =red= takes the 'seed as first argument (because it is the data we are working on, we are following the convention), a reducing function as second argument and (unlike clojure.core/reduce) as many iterables as you like (here one).

      (is (red #{} sip [1 2 3 3 4 2 1 5 6 4 5]) ;; 'sip is asparagus conj(ish) function
          #{1 4 6 3 2 5})

      ; With several iterables.

      (is (red []
               (f [ret a b] ;; note that the reducing function arity is dependant on the number of given iterables (here two)
                  (sip ret (add a b)))
               [1 2 3 4]
               [2 3 4 5])
          [3 5 7 9])

)

    (do "=filt= and =rem="

      (is [1 2 3]  (filt [1 2 -1 -2 3] num? pos?))
      (is [-1 -2] (rem [1 2 -1 -2 3] pos?))

))

  (do "=iter=, =idxs= and =vals="

    ; Under the hood many of the functions described in the previous section rely on those three basics operations.

    (do "=iter= "

      ; Is like =core/seq= (but do not returns nil on empty things).

      (is (iter {:a 1 :b 2})
          (lst [:a 1] [:b 2]))
      (is (iter [1 2 3])
          (lst 1 2 3))
      (is (iter (lst 1 2 3))
          (lst 1 2 3))

)

    (do "=vals= "

      ; Returns a seq of values in the given argument.

      (is (vals {:a 1 :b 2})
          (lst 1 2))
      (is (vals [1 2 3])
          (lst 1 2 3))
      (is (vals (lst 1 2 3))
          (lst 1 2 3))

)

    (do "=idxs="

      ; Returns a seq of keys for maps, or a seq of idexes for sequentials.

      (is (idxs {:a 1 :b 2})
          (lst :a :b))
      (is (idxs [1 2 3])
          (lst 0 1 2))
      (is (idxs (lst 1 2 3))
          (lst 0 1 2))

      ; Those three functions are generic and can be implemented for your types.

))

  (do "Extra operations "

    ; =scan= (like =core/partition=)
    (is [[1 2] [3 4]]
        (scan [1 2 3 4] 2 2))
    (is [[1 2] [2 3] [3 4]]
        (scan [1 2 3 4] 2 1))
    (is '((0 1 2 3) (2 3 4))
        (scan (c/range 5) 4 2))

    ; =chunk=
    (is [[1 2] [3]]
        (chunk [1 2 3] 2))
    (is []
        (chunk [] 2))

    ; =braid= (like =core/interleave=)
    (is '(1 4 2 5 3 6)
        (braid [1 2 3] [4 5 6]))
    (is '(1 4 2 5)
        (braid [1 2 3] [4 5]))

    ; =nths=
    (is (nths (range 10) 3)
        (lst 0 3 6 9))

    ; =car= and =cdr= compositions, like in scheme we have those little facilities, this is the main reason I chose =car= =cdr= over =first= =rest=.
    (is :io
        (cadr [1 :io])
        (caddr [1 2 :io])
        (caadr [1 [:io 2] 3])
        (cadadr [1 [2 :io]]))

)

  (do "Walking"

    ; =depth first=
    (!! (dfwalk [1 2 {:a 1 :b [1 2 3]}] p/prob))

    ; =breadth first=
    (!! (bfwalk [1 2 {:a 1 :b [1 2 3]}] p/prob))

    ; =walk?=
    (!! (walk? [1 2 {:a 1 :b [1 2 3]}]
               coll? ;; this is call on each node, in order to decide to walk deeper or not
               p/prob ;; when the above fails on a node, this one is called on it
               ))

))

(do "Functional Programing"

  ; One thing we all love in functional programming is the ability to compose functions together. 
  ; Manipulating them easily, passing them to other functions, partially apply them etc... 
  ; In asparagus I've tried to push all those things further than clojure.

  (do "Application, Invocation"

    ; Application and invocation are generic function that can be implemented for any type.
    ; Those operations are so central in functional programming that i've decided to give them really short symbols.

    ; - =*= for application
    ; - =§= for invocation

    (do "=§= (invocation)"

      ; for function it is trivial.

      (is (§ add 1 2)
          3)

      ; For constants it returns itself.

      (is (§ 42 "iop") 42)
      (is (§ "pouet" 1 2 3) "pouet")

      ; Datastructures have their invocation implementation, that differs from clojure, it does not perform a get.

      ; Some exemples should speak by themselves:

      (do "Vectors"

        (is (§ [inc dec] [0 0])
            [1 -1])

        ; You can nest invocables several level deep, it will do what you expect

        (is (§ [inc dec [inc dec :foo]] [0 0 [0 0 0]])
            [1 -1 [1 -1 :foo]])

        ; But wait you can feed several arguments too!

        (is (§ [add sub add] [1 2 3] [1 2 3] [1 2 3])
            [3 -2 9])

        ; It leaves extra indexes as is.

        (is (§ [inc dec] [0 1 2 3])
            [1 0 2 3])

)

      (do "Maps"

        (is (§ {:a inc :b dec :c [inc dec]}
             {:a 0 :b 0 :c [0 0]})
            {:a 1 :b -1 :c [1 -1]})

        ; several args:

        (is (§ {:a add :b sub}
             {:a 1 :b 2}
             {:a 1 :b 2})
            {:a 2 :b 0})

        ; Extra keys are left as is:

        (is (§ {:a inc}
             {:a 0 :b 0})
            {:a 1 :b 0})

        ; If extra keys are present in several args the last is kept.
        (is (§ {:a add} {:a 1 :b 2} {:a 1 :b 7})
            {:a 2 :b 7})


))

    (do "=*= (apply)"

      (is (* add 40 [1 1]) 42)

      ; TODO

))

  (do "The "object" convention"

    ; In asparagus, many functions takes what we can call the object as first argument.

    ; I mean, the thing we are working on, for instance, in the expression =(assoc mymap :a 1 :b 2)=, =mymap= is what we call the object. 

    ; All functions that can be viewed this way, will always take the 'object' as first argument.

    ; With this simple convention we can achieve a regularity that yield to easier function composition.

    ; The argumentation function will help to turn this kind of function into a one that takes only the arguments (in the previous exemple: :a 1 :b 2) and return a function that takes only the target object, and return the result.

    (let [assoc_ (argumentation:val assoc)
          assoc-a-and-b (assoc_ :a 1 :b 2)]
      (assoc-a-and-b {}))

    ; You can also pass arguments immediatly.
    (let [f (argumentation assoc :a 1)]
      (f {}))

    ; Many of the asparagus functions that follow this convention, have their argumentation version with the same name suffixed with =_=.

    ; This is handy, for instance, to create chains of 1 argument functions.

    (is (> (range 10) (drop_ 3) (dropend_ 2)) ;; will thread '(range 10) thru 2 functions, the semantics is analog to core/-> but it is a function
        (range 3 8))

    ; the =>= function is defined in the =:invocation-application-mapping= section of =asparagus.core=

    ; It will return a function that wait for its first argument ('myseq in the previous example)

    (!! (>_ (take_ 3) (dropend_ 2)))

)

  (do "Guards"

    ; One other thing that ease function composition is what I call guards (for lack of better name). 
    ; Guards differs from predicate by the fact that they can either return =nil= or something (in most case the given 'object' unchanged) so they can be used like predicates, but do not stop the flowing data, therefore they can be chained via function composition.

    ; Some examples of guards:
    (is (vec? [1 2]) [1 2])
    (isnt (vec? (lst 1 2)))
    (is (pos? 1) 1)
    (isnt (pos? -1))

    ; As we've seen we can chain them like this.
    (let [g (>_ num? pos? (gt_ 2))] ;; gt is greater-than
      (is 3 (g 3)))
    ;; but + does the same
    (let [g (+ num? pos? (gt_ 2))]
      (is 3 (g 3)))

    (do "Collection guards"

      (do "=$?="

        ; check if all values of a datastructure are not =nil= (see =iterables= section)

        (is ($? [1 2 3])
            [1 2 3])

        (isnt ($? [1 nil 2 3]))

        (is ($? {:a 1 :b 2})
            {:a 1 :b 2})

        (isnt ($? {:a 1 :b nil}))

)

      (do "=?$="

        ; =?$= is a composition of =$= and =$?=.

        ; It can be viewed as a =map= operation that succed if all values of the resulting collection are non =nil=

        (is (?$ [2 3 4 5] num? inc (gt_ 2))
            [3 4 5 6])

        (isnt (?$ [3 4 1 5] num? inc (gt_ 2)))

)

      (do "=?zip="

        ; the =zip= variant

        (is (?zip #(pos? (add %1 %2)) [1 2 3] [1 2 3])
            (lst 2 4 6))

        (isnt (?zip #(pos? (add %1 %2)) [1 2 3] [1 2 -3]))

)

      (do "=?deep="

        ; a deep variant of =?$=, checks if all nested values are non =nil=

        (check
         (nil? (?deep {:a {:b 1 :c [1 2 nil]}}))
         (nil? (?deep {:a {:b 1 :c [1 2 3 {:d nil}]}}))
         ;; succeed
         (?deep {:a {:b 1 :c [1 2 3]}}))

))

    (do "Creating Guards"

      (let [g (guard.unary c/odd?)]
        (is 1 (g 1)))

      (let [g (guard.binary c/>=)]
        (is 2 (g 2 1)))

      (let [g (guard.variadic c/>=)]
        (is 8 (g 8 8 7 6 5 2)))

      ;; or simply
      (let [g (guard:fn c/>=)]
        (is 8 (g 8 8 7 6 5 2)))

      (do "the =guard= macro"

        ; It has the same syntax than the =f= macro but the resulting function will return the first argument unchanged if its body succeeds, otherwise =nil=.

        (let [g (guard [x] (odd? (count x)))]
          (is (g [1 2 3]) [1 2 3])
          (isnt (g [1 2 3 4])))

)

      (do "wrapping and importing predicates "

        (E+ (guards.import [odd? 1] [even? 1]))

        (is 1 (odd? 1))
        (isnt (even? 1))

)))

  (do "Control Flow"

    (do "=?>="

      ; Thread the object thru guards shorting on first =nil= result.

      (is 1 (?> 1 num? pos?))
      (isnt (?> 1 num? neg?))

      ; Shorts after =str?= (else it would be an error).

      (isnt (?> 1 str? (+_ "aze")))

      ; More exemples:

      (is 3 (?> [1 2 3] (guard:fn (+ c/count c/odd?)) last))
      (isnt (?> [1 2] (guard [x] ((+ c/count c/odd?) x)) last))


      ; More composed exemple:

      ; =?>= use =§= under the hood, so anything that implement invocation is allowed.

      (is (?> -1
              num? ;;=> -1
              (c/juxt (add_ -2) (add_ 2)) ;;=> [-3 1]
              [neg? (?>_ num? pos?)] ;; using _ version
              )
          [-3 1])

)

    (do "=?<="

      ; Trying all given guards against its first argument until first non =nil= result.

      (is 1 (?< 1 coll? num?))
      (isnt (?< 1 str? coll? sym?))

      ; Build a guard that succeed for numbers or strings.

      (let [f (?<_ num? str?)]
        (is [1 "a" nil]
            [(f 1) (f "a") (f :a)]))

      ; Basic composition with =?<= and =?>_=:
      (is 42
          (?< 44
              str?
              (?>_ num? (gt_ 10) dec dec)))

)

    (do "=?c="

      ; A clojure-cond(ish) function.

      (is 2
          (?c 1
              ;; like clojure cond
              ;; works by couples
              str? :pouet ;; if str? succeed :pouet is called
              pos? inc
              neg? dec))

      (is 10
          (?c 10
              num? (lt_ 3) ;; if the second pred fail, we go to next couple
              num? (gt_ 7) ;; this line succeed
              ))

      ; (non function values act as constant functions).

      (is :pouet
          (?c "a"
              str? :pouet
              pos? inc
              neg? dec))

      ; Same with =?c_=

      (is -2
          (let [f (?c_
                   str? :pouet
                   pos? inc
                   neg? dec)]
            (f -1)))

)

    (do "=?c>="

      ; A scheme-cond(ish) function.

      (is -8
          (?c> -2
               ;; like scheme cond
               ;; several vecs of guards
               [str? :pouet]
               [pos? inc inc inc]
               [neg? dec dec (p mul 2)]))

      (is :1
          (?c> 1
               ;; here too, if the line does not succeed entirely,
               ;; skip to the next line
               [pos? dec pos? :gt1]
               [pos? :1]))

      (is 5
          (let [f (?c>_
                   [str? :pouet]
                   [pos? inc inc inc]
                   [neg? dec dec (p mul 2)])]
            (f 2)))

)

    (do "=df= (data function) "

      ; create a function from a data structure that apply all functions contained in it (deeply) to further args while preserving its original structure.

      ; you can use vectors and maps to compose the resulting function

      (!! (df [inc
               dec
               {:doubled (f_ (mul 2 _))
                :halfed (f_ (div _ 2))}])) ;;=> <fn>

      ; Invoking it:
      (let [f (df [inc dec
                   {:doubled (f_ (mul 2 _))
                    :halfed (f_ (div _ 2))}])]
        (is (f 1)
            [2 0 {:doubled 2 :halfed 1/2}]))

      ;; is equivalent to write
      ((f1 a [(inc a) (dec a)
              {:doubled (mul 2 a)
               :halfed (div a 2)}])
       1)

      ; Any invocable can serve as a leaf. Don't know if you remember, but in asparagus almost everything is invocable.
      ; In particular constant values like =42= or =:foo= return themselves. To demonstrate that =df= can handle any invocable, we will use some of those.

      (let [f (df [inc dec :foo 42])]
        (is (f 1)
            [2 0 :foo 42]))

      ; Can take several arguments.
      (let [f (df [add sub])]
        (is (f 1 2 3)
            [6 -4]))

      ; You can deeply mix maps and vecs to compose your function.
      (let [f (df {:addsub [add sub]
                   :average (f xs (div (* add xs) (count xs)))})]
        (is (f 1 2 3)
            {:addsub [6 -4], :average 2}))

      ; Maybe you are wondering about our vec and map invocation behavior. This is prevented here because vecs and maps mean something else in this context.
      ; But you can use the =§= function to state that a leaf that is a map or a vec has to be treated as an invocable.

      (let [f (df [concat
                   (§ [add sub mul]) ;; here
                   ])]
        (is (f [1 2 3] [4 5 6])
            ['(1 2 3 4 5 6) [5 -3 18]]))

)

    (do "Composing data flow "

      ; with guards, short-circuiting binding/lambda forms (?let, clet, cf, ?f...) , invocable datastructures, data functions, conditional functions (?c and ?c>), guard connectors (?< and ?>)

      ; This example is really dummy but tries to show how those things can be used together...

      (is

       (?> ["foo" 0]
           ;; with invocable data we can go inside the flowing data
           [
            ;; we check if the first idx is a :word (str, sym or keyword),
            ;; if yes cast it to keyword
            (?>_ word? key)

            ;; with data functions we can do sort of the opposite (wrapping instead of going inside)
            ;; (here we receiving 0 and returning {:val 0, :inc 1, :dec -1}
            (df {:val id :++ inc :-- dec})]

                                              ;p/prob

           (case_ [:bar x] {:bar x}
                  [:foo (& x (ks val))] ;; we check that data idx 0 is :foo, and that the idx 1 has a :val key
                  (case val
                    pos? {:positive-foo x}
                    neg? {:negative-foo x}
                    {:zero-foo x})
                  (id x) {:fail x})

                                              ;p/prob

           (?c_ (?f1 {:fail x}) (f_ (pp "fail: " _) _) ;; short-circuiting lambdas can be useful in those contexts
                (?f1 {:zero-foo x}) (f_ (pp "zero-foo " _) _)
                (f_ (pp "num-foo " _) _))

           )

       {:zero-foo {:val 0, :++ 1, :-- -1}})

)))

(do "Templating, Quoting"

  ; Asparagus does not have the same quasiquote semantics and syntax than clojure (in clojure, the ` character).
  ; Inspired by brandon bloom's backtic library, I tried to separate symbol qualification from templating.
  ; There is 3 tastes of quasiquotes in asparagus (in addition to the normal quote)

  (do "=sq= (syntax-quote or quasiquote)"

    ; quasiquote expressions are useful for constructing datastructures when most but not all of the desired structure is known in advance.
    ; If no ~ (unquote) appear within the template (the first and only argument of the quasiquote form), the result of evaluating (sq template) is equivalent to the result of evaluating 'template.

    (is (sq (a b c))
        '(a b c))

    ; If a ~ appears within the template, however, the expression following the ~ is evaluated ("unquoted") and its result is inserted into the structure replacing the ~ and the expression.

    (is (sq (+ 1 ~(+ 2 3)))
        '(+ 1 5))

    (is (sq (list ~(+ 1 2) 4))
        '(list 3 4))

    (let [name 'a]
      (is (sq (list ~name '~name))
          '(list a (quote a))))

    ; If a tilde (=~=) appears preceded immediately by a dot, then the following expression must evaluate to an iterable structure. 
    ; The evaluated iterable structure will be merged into its host structure, replacing the dot, the unquote and the expression.
    ; Therefore a 'dot unquote expr' (=.~expr=) structure has to appears only in an iterable structure (in order to be able to be merged into it).


    (is (sq (0 .~($ [0 1 2] inc) 4))
        '(0 1 2 3 4))

    (let [amap {:b 2 :c 3}]
      (is (sq {:a 1 .~amap})
          {:a 1, :b 2, :c 3}))

    ; Quasiquote forms may be nested. 
    ; Substitutions are made only for unquoted components appearing at the same nesting level as the outermost backquote.
    ; The nesting level increases by one inside each successive quasiquotation, and decreases by one inside each unquotation.


    (is (sq (a (sq (b ~(+ 1 2) ~(foo ~(+ 1 3) d) e)) f))
        '(a (sq (b ~(+ 1 2) ~(foo 4 d) e)) f))

    (let [name1 'x
          name2 'y]
      (is (sq (a (sq (b ~~name1 ~'~name2 d)) e))
          '(a (sq (b ~x y d)) e)))

)

  (do "=qq= (qualified quasiquote)"

    ; Is somehow similar to clojure =quasiquote=, in the sense that it let you template a structure like =sq= do, but also qualifies symbols.

    (is (qq (+ 1 ~(+ 2 3)))
        '(_.joining.+ 1 5))

    ; A word on qualified symbols: 

    ; When qualifying =+= we resolve it to =joining.+= (indicating that the =+= function lives in the =joining= module).
    ; The underscore prefix simply make explicit that it is an absolute path (preventing any relative or bubbling resolution that could occur at a later stage).

    ; If a symbol is not resolvable it is left as is.
    (is (qq (+ a b c))
        '(_.joining.+ a b c))

    ; All the things that we've seen with =sq= are possible with =qq=.

)

  (do "=qq!= (qualified strict quasiquote)"

    ; The behavior is the same as =qq= but it throws when encountering an unqualifiable symbol
    (is (qq! (+ 1 ~(+ 2 3)))
          '(_.joining.+ 1 5))

    ; the following will throw, indicating: "unqualifiable symbol: a"
    '(!! (qq! (+ a b c)))

))

(do "Environment again "

  ; It's time to go deep into the environment and the =E+= macro

  ; let's talk about metaprograming first.
  ; As any Lisp, asparagus has metaprograming facilities (e.g macros but not only).
  ; Asparagus macro system is a bit different than regulars Lisps.

  ; It use a technique introduced by Daniel Friedman called "expansion passing style".
  ; In regular Lisps, macro calls are recursively expanded until no more macro calls appears in the resulting expression.
  ; It results in more concise macro definition, but is less powerfull and prevents you to do more advanced things.

  ; A macros in asparagus is a function that takes 2 arguments:
  ; - the current environment (in this, it can reminds you Fexprs, more precisely 'compile time fexprs')
  ; - the list of operands of the macro call

  ; Since macros have access to the environment, and are responsable to thread expansion further.
  ; They can do all sort of transformations/updates on the environment before passing it to further expansions.
  ; Therefore a macro has full control over its operands, which seems legitimate to me.

  ; Another interesting thing about asparagus macros, occuring from the fact that there is no automatic recursive expansions.
  ; Is that functions and macros can share the same name. you may ask yourself what is the point :).
  ; In fact in clojurescript, this kind of technique is used for compile time optimizations for exemple.

  (do "Macros"

    ; A simple macro definition.

    (E+ postfix:mac ;; note the :mac sufix
        (fn [e args]
          (reverse args)))

    ; Let's expand this form in the current global environment using =exp=.
    (exp @E '(postfix 3 2 1 add))
    ;;=> (add 1 2 3)

    ; In trivial cases like this one it works but if the operands contains some macro calls, they will not be expanded.
    (exp @E '(postfix (postfix 5 4 add) 3 2 1 add))
    ;;=> (add 1 2 3 (postfix 5 4 add))

    ; As we've said, we have to thread the expansion further. 
    ; We've just seen the =exp= function that perform expansion given an environment and an expression.
    (E+ postfix:mac
        (fn [e args]
          (reverse
           ;; we are mapping the expansion over the arguments, with the same environment e
           ($ args (p exp e)) ;; equivalent to (clojure.core/map (partial exp e) args)
           )))

    ;; therefore
    (exp @E '(postfix (postfix 5 4 add) 3 2 1 add))
    ;;=> (add:val 1 2 3 (add:val 4 5))

    (is (postfix (postfix 5 4 add) 3 2 1 add)
        15)

    ; It is not a high price to pay I think, given the flexibility and power it can provide.

    ; You can do many crazy things with this behavior, but you don't have to.
    ; Those days I personaly tends to prefer technologies that let the power to the user, even if I agree that strong opinions and good practices enforcement can yield to a powerfull language and strong community (like clojure)
    ; Its a matter of taste and needs after all (at a point in time).

    ; All the binding forms and lambda macros are defined this way, so maybe its time to look at the =asparagus.core= namespace

)

  (do "=mac="

    ; And as you may have guessed, we can implement regular Lisp behavior in terms of those semantics, using the builtin =mac= macro, we will define a dummy 'fi macro (same as clojure's if-not)

     (E+ fi:mac
          ;; the 'mac macro let you define a macro in a standard way
          (mac [p f t] ;; we do not receive the environment
               (lst 'if p t f) ;; we don't thread the expansion, it will be automatically done
               ))

      (exp @E '(fi (pos? 1)
                   (fi (neg? 1) :zero :neg)
                   :pos))

    ; And operands are expanded as they would be with regular Lisp macro.

    '(if (guards.builtins.pos?:val 1)
         :pos
         (if (guards.builtins.neg?:val 1)
           :neg
           :zero))

)

  (do "Dual Stage"

    ; Our =fancy-add= function, will check the presence of litteral numbers in its operands and preprocess them at compile time, living others operands as is for runtime.

    (E+ fancy-add
        { ;; compile time behavior
         :mac
         (f [e xs]
            (let [lit-nums (shrink+ xs num?) ;; we grab all litteral numbers
                  others (shrink- xs num?)]  ;; and we keep others operands
              ;; we return the preprocessed form (expanded with e)
              (exp e
                   (qq (fancy-add:val ;; we have to explicitly write the :val suffix in this case
                        ;; we peform the compile time work
                        ~(* add lit-nums)
                        ;; we thread the expansion mapping exp on others operands and splice the result
                        .~($ others (p exp e)))))))

         ;; runtime behavior, a normal addition
         ;; using the scheme's variadic args syntax and the composite syntax (the dot)
         :val
         (f xs (add . xs)) ;; equivalent to (fn [& xs] (apply add xs))
         }) 

    (exp @E '(let [a 1]
               (fancy-add 1 a 2)))
    (is 4
        (let [a 1]
          (fancy-add 1 a 2)))

)

  (do "Substitutions"

    ; 'substitutions' are another metaprograming device that deserve attention I think.
    ; It gives the user a way to replace a simple symbol with an arbitrary expression.

    ; Trivial substitution
    (E+ macro-exemple.foo:sub ;; :sub attribute denotes a substitution function 
        (fn [e]
          ;; we return a simple keyword, but this could have been any expression
          ;; (built or not with the help of the environment. bound to 'e here)
          :foofoofoo)) 

    (exp @E 'macro-exemple.foo)

    ; Substitutions are performed at expansion time, like macros.

    ; Another simple substitution:
    ; A really dummy exemple just to demonstrate that you have access to the environment.

    (E+ substitution-exemple.bar:sub
        (f [e]
           (or
            ;; the bubget function try to resolve a symbol in the given env
            (bubget e 'substitution-exemple.bar:val)
            ;; if not found, it will be substitute by this expression
            '(some expression) 
            )))

    (exp @E 'substitution-exemple.bar)    ;=> (some expression)

    (E+ substitution-exemple.bar 42) ;; now we define a :val

    ; So now it is substituted by what's in substitution-exemple.bar:val.
    (exp @E 'substitution-exemple.bar)    ;=> 42

)

  (do "Updates "

    ; Extending =E+= behavior with the :upd attribute.

    ; It can hold a function that creates an update datastructure (like the one =E+= takes), hold on, see the exemple:

    ; We declare an update called =tagged=.
    (E+ tagged:upd ;; note the :upd suffix
          (f [e [tag data]]
             ;; an update function has to return any datastructure that is understood by E+
             ;; like expansions and substitutions it has access to the environment (not used in this case)
             [:tag tag :val data]))

    ; we can use it in an =E+= form like this
    (E+ upd-function-demo.foo (tagged :my-tag 42))

    ; when =E+= see an =:upd= call it will execute it and substitute it with its return value. 
    ; In this case it will be equivalent to this:

    (E+ upd-function-demo.foo [:tag :my-tag :val 42])

    (env-inspect 'upd-function-demo.foo)

    ; This technique is used at several place in asparagus source (feel free to look at it)
    ; Contrary to macros and substitutions, updates are recursivelly executed, so an update functions can return an expression which is another update call, which will be further processed.

    ; All clojure's macros that emits def forms falls into this category

)

  (do "Side effects"

    ; Sometimes you need to do dirty/real things,
    ; =:fx= let you write an expression that will be executed at environment extension time, the expression being previously compiled with the current environment.

    (E+ fx-demo.foo [:fx (println "defining fx-demo.foo") :val 42])

    ; In practice it is used for things like, extending a protocol or running tests for exemple.
    ; See the =generic= section (which wrap clojure's protocols)

    ; All those special attributes may appears abstract at this time, but looking at asparagus source (that is highly documented) will show them in practice, and it will become more clear I hope.

))

(do "Generics"

  ; Generic functions are at heart of asparagus, every core operations is defined this way.

    (do "mono arity"

      (E+ my-generic
          (generic
           [a]                         ;; the argument vector
           :vec (str "vector: " a)     ;; the vector implementation
           :num (str "number: " a)     ;; the number implementation
           :coll (str "collection: " a) ;; collection impl
           (str "something else: " a)   ;; default case
           ))

      (check
       (eq (my-generic [1 2]) "vector: [1 2]")
       (eq (my-generic 1) "number: 1")
       (eq (my-generic (lst 1 2)) "collection: (1 2)")
       (eq (my-generic :iop) "something else: :iop"))

)

    (do "poly arity"

      ; As clojure.core/fn generics a multi arity syntax.

      (E+ my-generic2
          (generic
           ([a]
            ;; coll impl
            :coll {:coll a}
            ;; default case
            {:something a})
           ([a b]
            ;; line impl
            :line {:line a :extra-arg b}
            ;; default case
            {:my-generic2-arity2-default-case [a b]})
           ;; unlike clojure protocols, asparagus genric functions can have a variadic arity
           ([a b . (& c [c1 . cs])] ;; you can put any asparagus binding pattern in arguments
            ;; default case
            {:my-generic2-variadic-arity
             {:a a :b b :c c :c1 c1 :cs cs}})
           ))

      (!! (my-generic2.inspect))

      (check
       (eq (my-generic2 [1 2 3]) {:coll [1 2 3]})
       (eq (my-generic2 "iop") {:something "iop"})
       (eq (my-generic2 (lst 1 2 3) 42) {:line (lst 1 2 3) :extra-arg 42})
       (eq (my-generic2 :iop 42) {:my-generic2-arity2-default-case [:iop 42]})
       (eq (my-generic2 [1 2 3] 1 :iop {})
           {:my-generic2-variadic-arity
            {:a [1 2 3] :b 1
             :c (lst :iop {})
             :c1 :iop
             :cs (lst {})}})
       )

)

    (do "inspection, extension"

      ; We can inspect your generic like this.

      (!! (my-generic2.inspect))

      ; Here we will define an arity2 implementation for vectors.
      ; With clojure protocols, if we extend a protocol to our type, we have to implement all arities.
      ; In asparagus this is not mandatory.
      (E+ (my-generic2.extend
           [a b]
           :vec {:vec a :extra-arg b}))

      (is (my-generic2 [1 2 3] "iop")
          {:vec [1 2 3], :extra-arg "iop"})

      ; We still benefits from the others arity implementations.
      (is (my-generic2 [1 2 3])
          {:coll [1 2 3]})
      (is (my-generic2 [1 2 3] 1 2 3)
          {:my-generic2-variadic-arity
           {:a [1 2 3], :b 1, :c '(2 3), :c1 2, :cs '(3)}})

      ; The extend form is letting you implement several things at once.
      ; It has the same syntax as the initial definition.
      (E+ (my-generic2.extend
           ([a]
            :num {:num a}
            :str {:str a}
            :lst {:seq a})
           ([a b . c]
            :vec {:variadic-arity-vec-extension [a b c]})))

      (check
       (eq (my-generic2 1) {:num 1})
       (eq (my-generic2 "yo") {:str "yo"})
       (eq (my-generic2 (lst 1 2)) {:seq (lst 1 2)})
       (eq (my-generic2 {:a 1}) {:coll {:a 1}})
       (eq (my-generic2 [1 2] 1 2 3)
           {:variadic-arity-vec-extension [[1 2] 1 (lst 2 3)]})
       (eq (my-generic2 "hey" 1 2 3)
           {:my-generic2-variadic-arity
            {:a "hey", :b 1, :c (lst 2 3), :c1 2, :cs (lst 3)}}))

)

    (do "'à la carte' polymorphism"

      ; One consideration that came to my mind and that is experimented at the end of =asparagus.boot.generics= 
      ; is that each implementation should be callable directly when there is no need for polymorphism.

      ; Candidate syntaxes would be:
        (my-generic2_vec my-vec arg1 arg2)
        (my-generic2.case :vec my-vec arg1 arg2)

      ; It seems reasonable to be able to disambiguate when we can do so.
      ; And for critical code it can speed things a bit...
      ; It looks like best of both world to me

))

(do "Types"

  ; Declaring a simple new type.
  (E+ (type+
       :split ;; type tag
       [left right] ;; fields
       ))

  ; Definition with generic implementations. 
  (E+
   (type+ :mytyp ;; type tag

          [bar baz] ;; fields

          ;; generic implementations ...
          ;; only one here but there can be several of them
          (+ [a b]
             (!let [(:mytyp b) b]
                   (mytyp (+ (:bar a) (:bar b))
                          (+ (:baz a) (:baz b)))))))

  ;; instantiation
  (is (mytyp 1 2)
      (map->mytyp {:bar 1 :baz 2}))

  ;; typecheck
  (is (mytyp? (mytyp 1 2))
      (mytyp 1 2))

  ;; type
  (is (type (mytyp 1 2))
      :mytyp) ;;=> :mytyp

  ;; using generic implmentations
  (is (+ (mytyp 1 2) (mytyp 1 2))
      (mytyp 2 4))

  ; Also have pattern matching for free.
  (let [p1 (mytyp "Bob" "Wallace")
        (mytyp x y) p1]
    (is [x y]
        ["Bob" "Wallace"]))

  ; Types can be sort-of namespaced. 
  ; the folling declare a foo.kons type:
  (E+ foo ;; we are in the foo module
      (type+ :kons [kar kdr]) ;; we declare a type
      )

  (let [p1 (foo.kons 12 34)
        (foo.kons x y) p1]
    (is [12 34] [x y]))

)

(do "Object Orientation"

  ; A light way to mimic object oriented programming in clojure is to put methods inside a map.

  (let [obj
        {;; the greet method, taking the object has first argument and a name
         ;; returning a greet string
         :greet
         (fn [o name]
           (str "Hello " name " my name is "
                (c/get o :name))) ;; we use the object to retreive its name
         ;; a name attribute
         :name "Bob"}]

    ;; in asparagus when the verb of the expression is a literal keyword, it denotes a method call
    ;; this syntax comes from the Janet language (which you should check if you haven't already)
    (is (:greet obj "Joe")
        "Hello Joe my name is Bob")

    ;; (:greet obj "Joe") it is compiled roughly to
    ;; notice that the method receives the object as first argument
    (§ (c/get obj :greet) obj "Joe") ;; where § is invocation

    ;; since it compile to an explicit invocation
    ;; anything with an § impl can be fetch with this syntax

    (is (:name obj) "Bob")

    ;; (:name obj) will be compiled to
    (§ (c/get obj :name) obj)

    ;; it can seems problematic at first, but strings are constant and constants returns themselves when invoked
    ;; so it returns "Bob" as we want

    ;; on missing method it throw an informative error
    (throws (:bark obj))

    ::ok

    ;; this makes me think that we would like to be able to use this semantics in patterns maybe ?...
    ;; the pattern (:quak x) match on anything that :quak
    ;; the keyword syntax in pattern is already taken by typetags... but we will think about it
    ;; maybe we should use exclusively type-guards instead e.g (mytype? x) ......
  )

  ; Here's an experimental way to create objects that shares prototypes.

  (E+ (obj+ :named
            [name] ;; fields
            ;; proto
            {:greet
             (fn [o name]
               (str "Hello " name " my name is "
                    (c/get o :name)))}))


  (!! (named "Bob"))

  (E+ (obj+ :person
            [firstname name] ;; fields
            [:named] ;; ancestors (we will inherit from their prototypes)
            {:walk (f1 o "i'm walking")} ;; proto
            [] ;; a vector of generic implementations
            ))

  #_(env-inspect 'named)

  (let [o (named "Bob")]
    (is (:greet o "Joe")
        "Hello Joe my name is Bob"))

  (let [o (person "Bob" "Wallace")]
    (is (:greet o "Joe")
        "Hello Joe my name is Wallace" )
    (is (:walk o)
        "i'm walking")
    person.proto)

)

(do "Dive and Tack"

  (do "=dive="

    ; The =dive= generic function, let you get something inside something else.
    ; Its first argument represent the address of what you want to get.
    ; The second is the thing in which you want to find it.

    ; It is like =core/get= but with arguments reversed, and being a generic function, it can be extended.

    ; For key and syms it does what =core/get= had done.
    (is 1 (dive :a {:a 1}))
    (is 1 (dive 'a {'a 1}))

    ; For nums it search an idx.
    (is :io (dive 2 [0 0 :io 0]))
    ;; negative idxs supported
    (is :io (dive -1 [0 0 :io]))

    ; For vector it goes deep.
    (is :io (dive [:a :b] {:a {:b :io}}))
    ;; but any valid diving-address can be used
    (is :io (dive [:a -1] {:a [0 0 0 :io]}))

    ; It also accpets raw functions.
    (is 1 (dive inc 0))
    ;; which does not seems to really make sense at first but it can be handy in more complex dives
    (is 1
        (dive [:a num? inc]
              {:a 0}))


    ; Also, we can mention that it is a concrete exemple of something that is a function and a macro at the same time. 
    ; Here we use a technique that is analog to the one we used in =bind= (binding operators). 
    ; The =dive= module holds a map of operations implementations in =dive.ops=. 
    ; At expansion time, if the first argument to =dive= is an sexpr, the verb will be searched in =dive.ops=. 
    ; If an implementation is found, it will be executed (at expansion time) and the return value will take the place of the original expression.

    ; As an exemple, we use the 'ks operation.
    (!! (dive (ks a b) {:a 1 :b 2 :c 2}))

    ; =ks= is resolved in =dive.ops= and applied to the given args (here :a and :b), producing this form:
    '(dive (fn [y] (select-keys y [:a :b]))
           {:a 1 :b 2 :c 2})

    ; Functions implements =dive= so the expansion time work is done, the form will now ready for runtime.

    ; As you may have deduced by yourself, =dive.ops= can be extended with new operations. 
    ; Keep in mind that it will not alter all previous call to =dive=, which are already compiled. (this is a good thing :)).

    (do "Extension"

      ; Adding a =wtf= op that do something that does not make sense. 

      (E+ (dive.op+ wtf [x]
                    (qq (f_ [:wtf ~x _]))))

      ; Using it.
      (is (dive (wtf 42) {:a 1 :b 2})
          [:wtf 42 {:a 1 :b 2}])

      ; Inspecting the ops table.
      (ppenv dive.ops)

))

  (do "=tack="

    ; =tack= is not really intended to be used directly.

    ; In most cases we will use =put= and =upd= (that are defined in terms of it, and follow the 'object convention')

    ; It is semantically similar to =core/assoc= with different arg order.

    ; Like in =dive= the first argument is the address (and is used to dispatch). 
    ; The second argument is the object we work on (the tacking-target :)). 
    ; The third is the thing we want to put at this location. 

    (is  (tack 1      ;; the address
                 [1 2 3] ;; the object (target)
                 :io     ;; what we put
                 )
           [1 :io 3])

    ; It supports neg idexes like dive does. 
    (is (tack -1 [1 2 3] :foo))
    (is (tack -1 '(1 2 3) :foo))

    ; Vectors denotes nesting. 
    (is (tack [:a :b] {} 42)
        {:a {:b 42}})

    (is (tack [:a :b] nil 42)
        {:a {:b 42}})

    (is (tack [:a 2] nil 42)
        {:a [nil nil 42]})

    (is [[[nil nil 42]]]
        (tack [0 0 2] [] 42))

    (do "=put="

      ; =put= is the same as tack but takes the target as first argument, more similar to assoc.
      (is {:a 1}
          (put nil :a 1))

      (is {:a {:b 1}}
          (put {} [:a :b] 1))

      (is [[[nil nil 42]]]
          (put nil [0 0 2] 42))

      (is [nil [nil nil [1]] nil 89]
          (put [] [1 2 0] 1 3 89))

      (is {:a {:b 1, :p {:l [0 1 2]}}}
          (put {} [:a :b] 1 [:a :p :l] [0 1 2]))

)

    (do "=upd="

      ; Like clojure update but using tack under the hood.

      (is [0 [1 1]]
          (upd [0 [0 1]] [1 0] inc))

      ; It can take several updates at once.

      (is {:a {:b [0 2 2], :c {:d 42}}}
          (upd {:a {:b [0 1 2]}}
               [:a :b 1] inc
               [:a :c :d] (k 42)))

)

    (do "Extension "

      ; TODO

)))

(do "Dev utilities"

  ; Inspecting the environment. 

  ;; function
  (env-inspect 'take)
  ;; macro
  (ppenv take)

  ; Printing documentation.
  (ppdoc bind.ops)
  (ppdoc dive)

  ; Expanding an update call.
  (updxp (generic.reduced [a b] :num (add a b)))

  (updxp (bindings.bind.op+ pouet [[n] x]
                            [(gensym) (qq (c/repeat ~n ~x))]))

  ; TODO

)

(do "At last..."

  ; First of all, If you made it until here, thank you very much :)

  ; There is much things that are still to be done in order to make asparagus a usable language.

  ; - tooling is lacking
  ; - error messages are not always so helpful
  ; - we need a proper inspector for the environment (wich will certainly be fun to implement when asparagus will be ported to clojurescript)
  ; - the port to clojurescript will not be trivial, because asparagus rely on eval (but bootstrapped clojurescript is here, so it should be possible)

  ; TODO


