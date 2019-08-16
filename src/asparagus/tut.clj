(in-ns 'asparagus.core)

(init-top-forms
     let lut ?let !let !lut
     ?f !f f fu !fu
     f1 !f1 ?f1 !fu1 fu1
     f_ !f_ ?f_ !fu_ fu_
     cf ?cf !cf cfu !cfu 
     clet clut !clet !clut
     case casu !case !casu)

(defmacro is [& xs]
  `(!! (check (~'eq ~@xs))))

(defmacro isnt [& xs]
  `(!! (check ~@($ xs (f_ (lst `nil? _))))))

;; ------------------------------------------------------------------------
;;                               Rational
;; ------------------------------------------------------------------------

(_
 ;; asparagus is my last experience in language design
 ;; after several attempt, i've compiled some of the ideas I like, found in other languages/books/papers

 ;; it is embedded in clojure, but at the same time is quite far from it.
 ;; it has its own environment/namespaces mecanism, and another kind of macro system.
 ;; it is far from being production ready, but I hope that it can be interesting as an experiment for some of you

 ;; I'm an autodidact with no proper computer science degree, my approch to programming is quite empirical.
 ;; first of all I'm seeking for feedback, advices, discussions and even people to work with (and I'm also searching for a real job)

 ;; for now the main goals are:

 ;; flexibility
 ;; extensibility
 ;; expressivity
 ;; performance (close to clojure)

 ;; the main ways/devices to acheive them:

 ;; (environment/expansion)-passing-style macros (a more general/powerful macro system)
 ;; pattern matching (built in to lambdas and binding-forms, extensible binding semantics)
 ;; functional control flow (heavy use of function composition and guards, nil based shortcuiting constructs)
 ;; generic functions at heart (all core operations of the language will be implementable by user types and implemented meaningfully by core types)
 ;; holy datastructures's litterals (maps, vecs, lists and sets) at the heart (giving them more usages and extra syntax sugar)
 )

;; ------------------------------------------------------------------------
;;                            the environment
;; ------------------------------------------------------------------------

(_

 (_
  ;; you can define a variable like this (E+ stands for extend-environment)
  ;; its like a really (really!) fancy 'def

  (E+ foo 1)

  ;; or several at once

  (E+ bar \a
      baz 42)

  ;; for now we will use !! macro to evaluate forms (it is kind of ugly...), but later it will no longer be needed

  (!! foo) ;; return the value under foo e.g 1
  (!! baz) ;; 42
  (!! bar) ;; \a

  ;; you can modularize definitions (one of the motivational point of all this)
  ;; here we use a hashmap literal to define several variables in 'mymodule and 'mymodule.c

  (E+ mymodule
      {a 1
       b "hey"
       c {d 42
          e :pouet}})

  ;; we can achieve the same with vector litteral syntax
  ;; the semantic difference between hash and vec literals will be explained later

  (E+ mymodule
      [a 1
       b "hey"
       c [d 42
          e :pouet]])

  ;; we use dot notation to access nested environment members

  (!! (add mymodule.a mymodule.c.d)) ;; will add 1 and 42

  ;; dot notation can be used in definitions too

  (E+ mymodule.c.f 2
      foo.bar 'foob)

  ;; this definition does not overide our previous ones

  ;; foo is still defined

  (!! foo)                              ;=> 1
  (!! foo.bar)                          ;=> 'foob

  ;; one handy usage of this behavior is scoped helpers definition

  (E+

   ;; our intent is to implement a stat function that takes any number of mumeric arguments and return a map holding some statistics
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

  ;; in my clojure practice I was often annoyed to put stats.sum and stats.mean at the same level than stats
  ;; Certainly I can create a stats namespace holding those helpers, but... it seems heavy for sush a common/natural thing...

  (!! (stats 1 2 3 4))                  ;=> {:xs (1 2 3 4), :sum 10, :mean 5/2}

  ;; it could be defined with a map literal to

  (E+ stats
      { ;; for now i've hidden an important detail,
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

  ;; the :val thing is implicit in most cases
  ;; those three forms are equivalent
  (E+ myval {:val 1})
  (E+ myval:val 1)
  (E+ myval 1)

  ;; any environment variable can have any number of those attributes

  (E+ stats
      {:doc "a functions that takes some numbers and do some statistics on it"
       :version 0.1
       :tags #{:math}
       :foo :bar})

  ;; they can be refered in code with colon notation

  (!! stats:doc)

  (!! (eq stats stats:val))

  ;; we also could have used vector syntax to define stats

  (E+ stats
      [ ;; in vector literal definitions occurs sequentially
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

  ;; in E+, top level's strings literals represent documentation
  ;; (a bold choice maybe... but maybe not so much if we really want to make documentation a first class citizen)
  ;; and I've said to myself, maybe hardcoded string in code are not so common? far less than keywords for instance

  (E+ myvar
      ["myvar doc" 42])

  ;;  is equivalent to

  (E+ myvar {:val 42 :doc "myvar doc"})

  (!! (eq "myvar doc"
          myvar:doc))

  ;; finally we can redefine stats with doc litterals

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

  ;; so you may have a question now :)
  ;; If hashmaps, vectors and strings have special semantics in E+,
  ;; how can I use them as normal values for my variables?!
  ;; the answer is the :val field

  (E+ rawvals
      [h:val {:a 1 :b 2}
       v:val [1 2 3]
       s:val "iop"])

  (!! (and (eq {:a 1 :b 2} rawvals.h)
           (eq "iop" rawvals.s)))

  ;; one thing that may have intrigued you is relative environment member accesses
  ;; e.g .sum, .mean and ..sum (in the stats previous definition)

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

  (!! [(relative-access.demo1.a)
       (relative-access.demo2 5)
       (relative-access.demo3 9)])

  ;; you may wonder about interop... we do not support it for now, we have to think more carrefully about it
  ;; at those early stages I tought that the core design is the main focus,
  ;; We are not at the get-the-things-done stage for now ;)

  )

 ;; bubbling resolution -----

 (_

  ;; using absolute and relative paths for all our vars is kind of painfull and ugly
  ;; sometimes it is needed to desambiguate but certainely not all the time
  ;; when a symbol cannot be resolved at the current level, it will be searched bubling up the environment

  (E+ bubling.demo
      {a 1
       b.c
       (fn []
         ;; here 'a will be resolved bubling up the environment
         ;; in this case it will be resolved to bubling.demo.a
         a)
       c
       {a 2
        b
        (fn []
          ;; here it will be resolved to bubling.demo.c.a
          a)}}
      )

  (!! (eq 1 (bubling.demo.b.c)))
  (!! (eq 2 (bubling.demo.c.b)))
  )

 ;; links --------------------

 (_

  ;; the :links attribute let you define shorter accesses to other modules or members
  ;; when a non relative symbol cannot be resolved at the current location
  ;; its first segment will be searched in the current module links
  ;; if there is an existant link it will be substituted by it
  ;; if there is no link at the current level, we go up (bubling) and loop, until root

  (E+ links.demo
      {mod1 {a 1 b 2 c {d 3 e 4}} ;; a bunch of things that we will link to

       mod2
       {:links {m1 links.demo.mod1
                m1c links.demo.mod1.c
                bub bubling.demo}
        f
        (fn []
          ;; here m1.a will be substituted by links.demo.mod1.a
          ;; and m1c.d by links.demo.mod1.c.d
          (add m1.a m1c.d bub.a))}})

  (!! (links.demo.mod2.f))

  ;; with this we can acheive some of the things we do with :require and :use in clojure ns's form
  ;; it will not oftenly used directly, but will be used under the hood by higher level macros...


  ;; the asparagus environment is holded by the asparagus.core/E atom

  (env-inspect root-path)
  ))

;; ------------------------------------------------------------------------
;;                           compilation steps
;; ------------------------------------------------------------------------

(_
 ;; there is three fundamentals compilation steps in asparagus

 ;; qualify ;; will try to turn all symbols of the given expression into path objects
 ;; expand ;; macro and substitutions will be executed here (see environment(continued) section)
 ;; resolve ;; paths are turned to their corresponding var syms
 )

;; ------------------------------------------------------------------------
;;                            data primitives
;; ------------------------------------------------------------------------

(_

 (_
  ;; literals works the same way as clojure ones (except for some extensions that will be explained later)

  {:a 1}
  [1 2 3]
  #{1 2}
  "hello"
  :iop

  ;; the quote is different from clojure's one, it is more like quasiquote, it qualifies its operand
  ;; so symbols and list litterals will not be written as in clojure

  ;; a quoted sym
  (quot mysym)                          ;=> 'mysym
  (sym "mysym")                         ;=> 'mysym

  ;; it can seem like an heavy syntax for literal symbols, but I think that keywords can replace them in much use cases.
  ;; literal symbols are used mainly for meta programming. Introducing an unqualified symbol is not a trivial thing, so it deserve a bit of syntax
  ;; at the opposite, having to write qualified symbol is much more common/trivial and deverses better the quote reader syntax
  ;; clojure's quasiquote have special treatment that prevents me to overide it to my purposes, this is why asparagus has to make this choice.

  ;; a list
  (lst 1 2 3)
  ;;or if we want full quote
  (quot (a b c)) ;; which is not pretty I agree
  )

 ;; collections -------------------------------------

 (_

  ;; constructor functions
  ;; compared to clojure, the API have been uniformized

  (vec 1 2 3)                           ;=> [1 2 3]
  (lst 1 2 3)                           ;=> '(1 2 3)
  (set 1 2 3)                           ;=> #{1 2 3}
  (map [:a 1] [:b 2])                   ;=> {:a 1 :b 2}

  ;; with sequential last argument (like core/list*)

  (vec* (lst 1 2 3 4)) ;; with one argument it behaves like core.vec
  (vec* 1 2 [3 4])

  (lst* [1 2 3 4])
  (lst* 1 2 [3 4])

  (set* 1 2 [3 4])

  (map* [:a 1] [:b 2] {:c 2 :d 5}) ;; same as (map* [:a 1] [:b 2] [[:c 2] [:d 5]])

  ;; preds
  ;; each collection have its pred, that returns the given collection on success or nil otherwise

  (vec? [1 2 3])                        ;=> [1 2 3]
  (lst? (lst 1 2 3))                    ;=> '(1 2 3)
  (set? #{1 2 3})                       ;=> #{1 2 3}
  (map? {:a 1})                         ;=> {:a 1}

  ;; we will see that in asparagus we avoid predicates (functions that returns booleans)
  ;; in favor of guards (functions that can return nil indicating failure, or data)
  ;; for instance (pos? 1) will be, i think, more useful if it returns 1 in case of success and nil otherwise
  ;; this way it can be composed more easily i think.
  ;; more on control flow, shortcircuiting and stuff later...
  )

 ;; words -------------------------------------------

 (_

  ;; constructors
  ;; symbols and keywords have their core/str(ish) construtors

  (sym "foo")                           ;=> 'foo
  (key "foo")                           ;=> :foo

  (sym :foo "bar")                      ;=> 'foobar
  (key "foo" :bar "baz")                ;=> :foobarbaz

  ;; star variants

  (sym* "ab" (lst "cd" "ef" "gh"))          ;=> 'abcdefgh
  (key* "my" :keyword "_" [:foo :bar "baz"]) ;=> :mykeyword_foobarbaz
  (str* "mystr_" ["a" "b"])                  ;=> "mystr_ab"

  ;; guards
  ;; as for collections, we use guards instead of preds

  (key? :iop)                           ;=> :iop
  (sym? (quot bob))                     ;=> 'bob
  (str? "hi")                           ;=> "hi"
  )
 )

;; ------------------------------------------------------------------------
;;                            joining things
;; ------------------------------------------------------------------------

(_

 ;; joining things together with +
 ;; ------------------------------

 (_

  (is (+ [1 2] '(3 4))
      [1 2 3 4])

  (is (+ (lst 1 2) [3 4])
      '(1 2 3 4))

  (is (+ {:a 1 :b 0} {:b 2})
      {:a 1 :b 2})

  ;; + is variadic
  (is (+ #{} (lst 1 2) [3 4] #{3 5})
      #{1 2 3 4 5})

  ;; as you have seen, the return type is determined by the first argument

  ;; strs syms and keywords

  (is (+ 'foo "bar") 'foobar)
  (is (+ :foo 'bar) :foobar)
  (is (+ "foo" 'bar :baz) "foobar:baz")

  ;; on function it do composition
  ;; (left to right, not like core.comp do)
  (is ((+ inc inc (p mul 2)) 0)
      4)

  ;; unlike clojure.core.concat, + is an extensible operation that can be implementated by user types

  )

 ;; sip add one or several element into something
 ;; ---------------------------------------------

 (_

  (is (sip [] 1 2)
      [1 2])

  ;; for lists it adds at the end (not like conj do)
  ;; it is a choice that can be disctable, in my own pratice i'm not realying often on way that clojure lists implements conj
  ;; sip being a generic operation (extendable by user types) we could add a datatype that conj elements at its head like clojure lists...
  (is (sip (lst 1 2) 3)
      '(1 2 3))

  (is (sip #{3 4} 1 2)
      #{1 2 3 4})

  ;; for maps it works on entries
  (is (sip {:a 1} [:b 2] [:c 3])
      {:a 1 :b 2 :c 3})

  ;; for function it partially apply given args
  ;; (i'm not sure it should behave that way, it's more like an experimental fantasy that is not used in core code)
  (is ((sip add 1 2) 3)
      6)
  )

 ;; 'pure returns the empty version of the given argument
 ;; ---------------------------------------------------

 (_

  (is (pure "foobar") "")

  (is (pure {:a 1 :b 2}) {})

  (is (pure inc) id)

  ;; like sip and +, pure is a generic operation that can be implemented by user types

  )

 ;; pure? test for purity
 ;; ---------------------

 (_

  (is {} (pure? {}))

  (nil? (!! (pure? {:a 1}))))
 )

;; ------------------------------------------------------------------------
;;                           composing  things
;; ------------------------------------------------------------------------

(_

 ;; vectors ----

 (_

  (let [a 1
        b 2
        c [3 4]
        d [5 6]]

    ;; with a dot you can do splicing
    (eq [a b . c] [1 2 3 4])
    ;; the spliced part can be anywhere
    (eq [a b . c b a] [1 2 3 4 2 1])
    ;; several spliced parts
    (eq [a b . c . d] [1 2 3 4 5 6])
    ;; shortcut (everything after the double dot is spliced)
    (eq [a b .. c d] [1 2 3 4 5 6])
    ;; nested
    (eq [a b [42 . d] . c]
        [1 2 [42 5 6] 3 4])
    ))

 ;; maps ---------

 (_

  (let [a {:a 1}
        b {:b 2}
        c [1 2 3]]

    (eq {:a 1
         :c 3
         . b}

        ;; if you want to splice several map into your literal use .. []
        {:c 3
         .. [a b]}

        {:a 1 :b 2 :c 3})

    ;; it can be nested

    (eq
     {:foo [0 . c 4] ;; a composite vector
      :bar {:baz 1 . b}
      . a}

     {:foo [0 1 2 3 4]
      :bar {:baz 1 :b 2}
      :a 1})
    ))

 ;; lists ----------

 (_

  (let [nums [2 3 4]]

    ;; in conjunction with 'lst you can do the same things that we have shown with vectors
    (eq (lst 1 . nums)
        (lst 1 2 3 4))

    ;; but more interesting is this
    ;; you can achieve apply semantics with dot notation
    (eq (add 1 . nums)
        (c/apply add 1 nums)
        10)

    ;; but unlike with apply it does not have to be the last argument that is a collection
    (eq (add 1 . nums 5) 15)

    ;; we have doubledot also
    (eq (add .. nums nums nums)
        (add . nums . nums . nums)
        27)
    ))
 )

;; ------------------------------------------------------------------------
;;                            binding things
;; ------------------------------------------------------------------------

(_

 ;; let --------------------------------------------------------------------

 (_
  ;; asparagus has a whole family of let like binding forms
  ;; but unlike clojure's one, the binding behavior can be extended by the user in several ways

  ;; let form is similar to clojure's one but with extra capabilities

  (eq (let [a 1] a)
      1)

  (eq (let [a 1 b 2] (add a b))
      3)

  ;; refer earlier binding
  (eq (let [a 1 b a] (add a b))
      2)

  ;; binding symbols can be prepended by special character to indicate special behavior

  ;; shortcircuiting bindings
  ;; if a binding symbol is prefixed by ?,
  ;; it has to bind to a not nil value else the whole let form is shortcircuited and return nil

  (nil? (let [?a nil ;; this binding fail, therefore the next line will never be evaluated
              b (error "never evaluated")] 42))

  ;; strict bindings
  ;; binding symbol's prepended by ! must bind to non nil value, else an error is thrown

  (eq :catched
      (try (let [!a (pos? -1)] :never)
           (catch Exception _ :catched)))

  ;; those three modes of binding (regular (non prefixed symbols), shortcircuited, strict) can be combined inside let forms
  ;; resulting, i think, in much expressivity
  )

 ;; let variants -----------------------------------------------------------

 (_

  (_
   ;; ?let (shortcircuiting let) ------

   ;; is behaving like let, but the ? prefix is implicit to all binding symbols
   (?let [a 1 b 2] (add a b))
   ;; is equivalent to
   (let [?a 1 ?b 2] (add ?a ?b))
   ;; we can use strict bindings in a ?let form, it will behave as in let
   (eq :catched
       (try (?let [a 1
                   !b (pos? -1)] (add a !b))
            (catch Exception _ :catched)))
   ;; if we want to allow regular bindings (as normal symbols in a classic let)
   ;; we use the _ prefix
   (eq (?let [a 1
              _b nil] ;; _b is bound to nil but this does not shorts
             a)
       1)
   )

  ;; !let (strict let) ------------------

  (_
   ;; is like ?let but with implicit prefix !, it support ? and _ prefixes
   (eq :catched
       (try (!let [a nil] :never)
            (catch Exception _ :catched)))
   )

  ;; lut (unified let) ------------------

  (_
   ;; in a unified let, all symbols that appears several times have to bind to the same value (equal values)

   (lut [a 1 a (dec 2)] :success)

   (lut [a 1
         a 2] ;; this will shorts because a is already bound to 1
        (error "never thrown"))
   )

  ;; !lut (unified strict let) ----------

  (_
   (eq :catched
       (try (!lut [a 1
                   a 2] ;; this will throw because a is already bound to 1
                  :never)
            (catch Exception _ :catched)))
   ))

 ;; destructuration --------------------------------------------------------

 (_

  ;; literals ---------------------------

  (_
   ;; like clojure's let we support destructuration
   ;; but unlike clojure, destructuration is an extensible mecanism
   ;; the user can define its own destructuration special forms

   ;; sequential bindings
   (eq (let [[x . xs] (range 5)] [x xs])
       [0 (range 1 5)])

   ;; preserve collection type
   (eq (let [[x . xs] (vec 1 2 3)] [x xs])
       [1 [2 3]]) ;; in clojure [2 3] would be a seq

   ;; post rest pattern
   ;; in clojure the rest pattern has to be the last binding, here we can bind the last element easily
   (eq (let [[x . xs lastx] (range 6)] [x xs lastx])
       [0 (range 1 5) 5])

   ;; (we could also have bound several things after the rest pattern)
   (eq (let [[x . xs y1 y2 y3] (range 6)] [x xs y1 y2 y3])
       [0 (lst 1 2) 3 4 5])

   ;; maps
   (eq (let [{:a aval :b bval} {:a 1 :b 2 :c 3}] [aval bval])
       [1 2])

   ;; in clojure the same is acheived like this (I don't really understand why)
   (c/let [{aval :a bval :b} {:a 1 :b 2 :c 3}] [aval bval])

   ;; maps have rest patterns to
   (eq (let [{:a aval . xs} {:a 1 :b 2 :c 3}] [aval xs])
       [1 {:b 2 :c 3}])

   ;; as you may think, all binding modes are supported in destructuration bindings forms
   )

  ;; operators ---------------------------

  (_
   ;; ks is a builtin binding operator
   ;; it behaves like clojure's :keys
   (eq (let [(ks a b) {:a 1 :b 2 :c 3}] (add a b))
       3)


   ;; & (parrallel bindings)
   ;; several patterns can be bound to the same seed
   ;; something that i've sometimes missed in clojure (lightly)
   (eq (let [(& mymap (ks a b)) {:a 1 :b 2 :c 3}] [mymap a b])
       [{:a 1 :b 2 :c 3} 1 2])

   ;; some others builtin bindings exists, see source

   ;; new binding operators can be defined by the user
   ;; TODO
   )

  ;; special bindings --------------------

  (_
   ;; when an sexpr in found in binding position (left side of let bindings)
   ;; if it is not a binding operator call (like we've just seen 'ks and '& for instance)
   ;; it can be what we call a guard pattern
   ;; a guard pattern is an expression with a binding symbol as first argument

   (?let [(pos? a) 1] ;; if 1 is pos then the return value of (pos? 1) which is 1 is bound to the symbol a
         a)

   ;; is equivalent to
   (?let [a 1
          a (pos? a)]
         a)

   ;; this syntax is really making sense whith guards that returns their first argument unchanged in case of success

   (?let [(gt a 3) 4] ;; guards can have more than one arg
         a)

   (?let [(gt a 3) 2] ;; shorts
         (error "never touched"))

   ;; type guards
   ;; an sexpr starting with a type keyword (see asparagus.boot.types) indicates a type guard pattern
   (?let [(:vec v) [1 2 3]]
         v)

   (?let [(:seq v) [1 2 3]]
         (error "never"))

   ;; guard syntax
   (eq (?let [(pos? a) 1
              (neg? b) -1] (add a b))
       0)

   (?let [(pos? a) 1
          (neg? b) 1]
         (error "never thrown"))
   )

  ;; value patterns ----------------------

  (_
   ;; any value can be used in pattern position,

   (eq :ok (let [a (inc 2)
                 3 a] ;; 3 is in binding position, therefore the seed (a) is tested for equality against it, and it shorts if it fails
             :ok))

   (let [a (inc 2)
         4 a]
     (error "never"))
   )

  ;; user type patterns ------------------

  (_
   ;; any type can implement the 'bind operation and join the party
   ;; TODO
   )
  )

 ;; cased let --------------------------------------------------------------

 (_

  ;; cased let is like a cascade of shortcircuiting let forms
  ;; it can be be compared to cond-let but is more powerful

  ;;simple
  (eq (clet [x (pos? -1)] {:pos x}      ;first case
            [x (neg? -1)] {:neg x}      ;second case
            )
      {:neg -1})

  ;; each binding block can have several bindings
  (let [f (fn [seed]
            (clet [x (num? seed) x++ (inc x)] x++
                  [x (str? seed) xbang (+ x "!")] xbang))]
    (and (eq 2 (f 1))
         (eq "yo!" (f "yo"))
         (nil? (f :pop))))

  ;; default-case
  (eq (clet [x (pos? 0) n (error "never touched")] :pos
            [x (neg? 0) n (error "never touched")] :neg
            :nomatch)
      :nomatch)

  ;; strict version
  (throws
   (!clet [x (pos? 0)] :pos
          [x (neg? 0)] :neg))


  ;; unfied version of clet
  (let [f (fn [seed]
            (clut [[a a] seed] :eq
                  [[a b] seed] :neq))]
    (and (eq :eq (f [1 1]))
         (eq :neq (f [1 2]))))

  ;;unified strict version 
  (let [x [:tup [1 2]]]
    (throws
     (!clut [[:wat a] x] :nop
            [(:vec vx) x [:tup [a a]] vx] :yep)))

  (let [p [:point 0 2]]
    (clet [[:point x 0] p] :y0
          [[:point 0 y] p] :x0
          [[:point x y] p] [x y]))
  )
 )

;; ------------------------------------------------------------------------
;;                                lambdas
;; ------------------------------------------------------------------------

(_

 ;; the f macro --------------------------------

 (_
  ;; all the binding forms that we have seen so far have their lambda equivalent

  ;; regular monoarity lambda

  (let [fun (f [a b] (add a b))]
    (eq 3 (fun 1 2)))

  ;; variadic syntax
  (let [fun (f [x . xs] (add x . xs))]
    (fun 1 2 3 4))

  ;; all binding patterns are available
  (let [fun (f [x (ks a b)]
               (+ x {:a a :b b}))]
    (fun {:foo 1 :bar 2}
         {:a 1 :b 2 :c 3}))
  ;;=> {:foo 1, :bar 2, :a 1, :b 2}


  (let [fun (f [(& x [x1 . xs])
                (& y [y1 . ys])]
               {:x x :y y :cars [x1 y1] :cdrs [xs ys]})]
    (fun [1 2 3 4] [7 8 9]))
  ;;=> {:x [1 2 3 4], :y [7 8 9], :cars [1 7], :cdrs [[2 3 4] [8 9]]}

  ;; like let, different binding modes are available via prefix syntax

  (let [fun (f [!a ?b] (lst !a ?b))] ;; a is mandatory, and b can short the execution
    (and
     (eq (fun 1 2) (lst 1 2))
     (nil? (fun 1 nil))
     (throws (fun nil 2))
     :ok))

  ;; for recursion, like clojure/fn we can give a name to a lambda
  ;; we use keyword litteral to indicate a name
  ((f :mylambda [x . xs]
      (if-not (c/seq xs) x
              (add x (mylambda . xs))))
   1 2 3 4)                             ;=> 10

  ;; the same can be acheive with 'rec
  ((f [x . xs]
      (if-not (c/seq xs) x
              (add x (rec . xs))))
   1 2 3 4)                             ;=> 10

  ;; like in scheme, binding pattern can be a simple symbol
  ;; this is the reason why we need keyword litteral to name lambdas (to disambiguate)
  ((f xs (add . xs))
   1 2 3 4)                             ;=> 10
  )

 ;; variants -----------------------------------

 (_

  ;; like let, f has its binding mode variants, ?f, !f

  (let [fun (?f [(vec? a) (num? b)] ;; this is guard patterns (see previous section)
                (sip a b))]
    (and
     ;; the binding succeed
     (eq (fun [1 2 3] 4) [1 2 3 4])
     ;; first arg is not a vector so it shorts
     (nil? (fun 1 2))))

  ;; and also unified variants: fu and !fu

  (let [fun (fu [a b a] :ok)]
    (and (eq (fun 1 0 1) :ok)
         (nil? (fun 1 2 3))))

  (let [fun (!fu [a a] :ok)]
    (and (eq (fun 1 1) :ok)
         (throws (fun 1 2))
         :yeah)))

 ;; syntactic sugar ----------------------------

 (_

  ;; arity 1 syntax (f1)
  ;; function that takes one argument are so common that it deserves, i think, some syntactic sugar

  (let [double (f1 a (add a a))]
    (eq (double 2) 4))

  ;; you can use any binding pattern
  (let [fun (f1 (:vec a) (+ a a))] ;; we use a type guard (check if the given arg is a vector)
    (and
     (eq (fun [1 2 3]) [1 2 3 1 2 3])
     (nil? (fun 42))))

  ;; it has all the common variations: !f1 ?f1 !fu1 fu1
  ;; that do what you should expect (if you have not skip previous parts of this file)

  ;; we also have f_ that is a bit more concise than f1, if you don't need destructuring

  (let [double (f_ (add _ _))]
    (eq (double 2) 4))

  ;; it also have common variations, f_, ?f_ , !f_ (unification variants are useless here)
  )

 ;; case lambda --------------------------------

 (_
  ;; the cf macro is a bit like clojure's fn, it let's you define polyarity functions, but it benefits from all asparagus binding capabilities

  (let [fun (cf [a] 1
                [a b] 2
                [(:num a) b c . xs] :var1
                [a b c . d] :var2)]
    (and (eq (fun "iop") 1)
         (eq (fun 1 2) 2)
         (eq (fun 1 2 3 4 5) :var1)
         (eq (fun "iop" 1 2 3) :var2)))

  ;; it can have several implementaion with the same arity

  (let [fun (cf [(num? a)] {:num a}
                [(str? a)] {:str a})]

    (and (eq (fun 1) {:num 1})
         (eq (fun "aze") {:str "aze"})))

  ;; note that variadic cases must have the same length

  (cf [x . xs] :one
      [x y . zs] :two) ;;compile time error

  (cf [(:vec x) . xs] :one
      [(:num x) . xs] :two) ;; is ok

  ;; all previous variations are implemented: !cf, ?cf, cfu, !cfu
  ;; maybe we should have considered cf1...

  ;; You may ask yourself what is the price for this expressivity
  ;; i've worked hard on compiling those forms into performant code,
  ;; there is certainly a price for the shortcircuit, strict and unified binding modes, but certainly not as high as you may expect
  ;; sometimes it is close to bare clojure's perfs
  )

 ;; case ---------------------------------------

 (_

  (let [x (range 12)]
    ;; try those values too:  42 "iop" :pouet
    (case x
      (num? x) {:num x}         ;; first clause, x is a number
      (str? x) {:str x}         ;; second clause, x is a string
      [x . xs] {:car x :cdr xs} ;; third clause, x is sequential
      :nomatch))                ;; the default value

  ;; case has its unified variant 'casu

  (let [t (f [x]
             (casu x
                   [:point x 0] :y0
                   [:point 0 y] :x0
                   [:point (:num x) (:num x)] :twin
                   [:point (:num x) (:num y)] [x y]
                   :pouet))]
    (and
     (eq :y0 (t [:point 1 0]))
     (eq :x0 (t [:point 0 1]))
     (eq :twin (t [:point 1 1]))
     (eq [1 2] (t [:point 1 2]))
     (eq :pouet (t [:point 1 "io"]))))

  ;; there is also !case and !casu that throws if nothing match the input

  (let [x 1]
    (throws
     (!case x
            (str? x) :str
            (vec? x) :vec)))))

;; ------------------------------------------------------------------------
;;                              iterables
;; ------------------------------------------------------------------------

(_

 ;; basic operations

 (_

  ;; car (is like Lisp's car or clojure.core/first)
  (!! (eq 1 (car (lst 1 2))))
  (!! (eq 1 (car [1 2])))
  (!! (eq [:a 1] (car {:a 1 :b 2})))

  ;; cdr (is like clojure.core/rest but preserve collection type)
  (!! (eq (cdr [1 2 3]) [2 3]))
  (!! (eq (cdr (lst 1 2 3)) (lst 2 3)))
  (!! (eq (cdr {:a 1 :b 2 :c 3}) {:b 2 :c 3})) ;; on map it does not make much sense but...

  ;; last
  (!! (eq 2 (last (lst 1 2))))
  (!! (eq 2 (last [1 2])))
  (!! (eq [:b 2] (last {:a 1 :b 2}))) ;; same here...

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

 ;; map reduce and friends

 (_

  ;; map ($)

  ;; like many asparagus functions, map is taking the object as first argument
  (is ($ [0 1 2] inc)
      [1 2 3])

  ;; it preserves collection type
  (is ($ #{1 2 3} inc)
      #{2 3 4})

  ;; it can take several functions
  (is ($ (lst 0 1 2) inc inc)
      (lst 2 3 4))

  ;; on maps it behaves differently from clojure.core/map
  ;; given functions are receiving only the values
  (is ($ {:a 1 :b 2} inc)
      {:a 2 :b 3})

  ;; map indexed ($i)
  (is ($i [:a :b :c] (f [idx val] {:idx idx :val val}))
      [{:idx 0, :val :a}
       {:idx 1, :val :b}
       {:idx 2, :val :c}])

  ;; on maps it receives key-value pairs
  ;; given functions has to return only the value
  (is (!!($i {:a 1 :b 2}
             (f [idx val]
                ;; we return the key-value pair as is
                [idx val])))
      ;; the key-value pair has been put in value position
      ;; the keys cannot be altered with $i,
      ;; if you think about it $i on a vector or sequence cannot alter indexes,
      ;; map keys are like unordered indexes somehow, so it seems to be the correct behavior
      {:a [:a 1], :b [:b 2]}
      )

  ;; with sets, given functions receives a twin pair,
  ;; which seems logical as sets can be viewed as maps with twin entries
  ;; it is pointless to use $i explicetly on a set, but in a ploymorphic context, sets have to have a meaningful implementation
  (eq ($i #{:a :b :c}
          ;; the same function we use above in the map exemple
          (f [idx val] [idx val]))
      #{[:a :a] [:b :b] [:c :c]})

  ;; so now you may wonder about what we leave behing from the clojure.core/map behavior
  ;; in particular, core/map can takes several sequences
  (c/map + (range 10) (range 10)) ;;=> (0 2 4 6 8 10 12 14 16 18)

  ;; in asparagus there is another function for that called 'zip
  ;; zipping several iterables together using the given function
  (is (zip add (range 10) (range 10))
      (lst 0 2 4 6 8 10 12 14 16 18))

  ;; like core.map it is variadic
  (is (zip add (range 10) (range 10) (range 10) (range 10))
      (lst 0 4 8 12 16 20 24 28 32 36))

  ;; $+
  (is ($+ (range 6) (f_ (c/repeat _ _)))
      (lst 1 2 2 3 3 3 4 4 4 4 5 5 5 5 5))

  (is ($+ [[3 2 1 0] [6 5 4] [9 8 7]] rev)
      [0 1 2 3 4 5 6 7 8 9])

  ;; $i+ (indexed version of $+)
  (is ($i+ [[3 2 1 0] [6 5 4] [9 8 7]]
           (f [i v] (cons [:idx i] (rev v))))
      [[:idx 0] 0 1 2 3 [:idx 1] 4 5 6 [:idx 2] 7 8 9])

  ;; zip+
  (is (zip+ (f [a b]
               (c/sort
                ;; set+ makes a set from several collections
                (set+ a b)))
            [[3 1 0] [6 5] [9 8 7]]
            [[3 2 0] [5 4] [9 7]])
      (lst 0 1 2 3 4 5 6 7 8 9))

  ;; while writing this i'm considering zipi and zipi+...

  ;; red
  ;; like core/reduce but with different argument order and variadic arity
  ;; red takes the 'seed as first argument
  ;; a reducing function and second argument
  ;; and as many iterables as you like (here one)
  (is (red #{} sip [1 2 3 3 4 2 1 5 6 4 5]) ;; 'sip is asparagus conj(ish) function
      #{1 4 6 3 2 5})

  (is (red []
           (f [ret a b] ;; note that the reducing function arity is dependant on the number of given iterables (here two)
              (sip ret (add a b)))
           [1 2 3 4]
           [2 3 4 5])
      [3 5 7 9])

  ;; filt and rem
  (check
   (eq [1 2 3]  (filt [1 2 -1 -2 3] num? pos?))
   (eq [-1 -2] (rem [1 2 -1 -2 3] pos?)))

  )

 ;; iter, idxs and vals

 (_

  ;; under the hood many of the functions described in the previous section rely on those three basics operations

  ;; iter is like core/seq (but do not returns nil on empty things)
  (is (iter {:a 1 :b 2})
      (lst [:a 1] [:b 2]))
  (is (iter [1 2 3])
      (lst 1 2 3))
  (is (iter (lst 1 2 3))
      (lst 1 2 3))

  ;; vals return a seq of values in the given argument
  (is (vals {:a 1 :b 2})
      (lst 1 2))
  (is (vals [1 2 3])
      (lst 1 2 3))
  (is (vals (lst 1 2 3))
      (lst 1 2 3))

  ;; idxs return a seq of keys for maps, or a seq of idexes for sequentials
  (is (idxs {:a 1 :b 2})
      (lst :a :b))
  (is (idxs [1 2 3])
      (lst 0 1 2))
  (is (idxs (lst 1 2 3))
      (lst 0 1 2))

  ;; those three functions are generic and can be implemented for your types

  )

 ;; extra operations

 (_

  ;; scan (like core/partition)
  (is [[1 2] [3 4]]
      (scan [1 2 3 4] 2 2))
  (is [[1 2] [2 3] [3 4]]
      (scan [1 2 3 4] 2 1))
  (is '((0 1 2 3) (2 3 4))
      (scan (c/range 5) 4 2))

  ;; chunk
  (is [[1 2] [3]]
      (chunk [1 2 3] 2))
  (is []
      (chunk [] 2))

  ;; braid (like core/interleave)
  (is '(1 4 2 5 3 6)
      (braid [1 2 3] [4 5 6]))
  (is '(1 4 2 5)
      (braid [1 2 3] [4 5]))

  ;; nths
  (is (nths (range 10) 3)
      (lst 0 3 6 9))

  ;; car and cdr compositions
  ;; like in scheme we have those little facilities
  ;; this is the main reason I chose car/cdr over first/rest
  (is :io
      (cadr [1 :io])
      (caddr [1 2 :io])
      (caadr [1 [:io 2] 3])
      (cadadr [1 [2 :io]]))

  )

 ;; walk
 (_

  ;; depth first
  (!! (dfwalk [1 2 {:a 1 :b [1 2 3]}] p/prob))

  ;; breadth first
  (!! (bfwalk [1 2 {:a 1 :b [1 2 3]}] p/prob))

  ;; walk?
  (!! (walk? [1 2 {:a 1 :b [1 2 3]}]
             coll? ;; this is call on each node, in order to decide to walk deeper or not
             p/prob ;; when the above fails on a node, this one is called on it
             ))
  )

 )

;; ------------------------------------------------------------------------
;;                         functional programing
;; ------------------------------------------------------------------------

(_
  
 ;; one thing we all love in functional programming is the ability to compose functions together
 ;; manipulate them easily, passing them to other functions, partially apply them etc...
 ;; in asparagus I've tried to push all this stuff further than clojure

 ;; application and invocation

 (_

  ;; application and invocation are generic function that can be implemented for any type
  ;; those operations are so central in functional programming that i've decided to give them really short symbols
  ;; * for application 
  ;; § for invocation

  ;; invocation

  ;; for function it is trivial
  (is (§ add 1 2)
      3)

  ;; for constants it returns itself
  (is (§ 42 "iop") 42)
  (is (§ "pouet" 1 2 3) "pouet")

  ;; datastructures have their invocation implementation
  ;; that differs from clojure, it does not perform a get
  ;; some exemples should speak by themselves

  ;; vectors
  (is (§ [inc dec] [0 0])
      [1 -1])

  ;; you can nest invocables several level deep, it will do what you expect
  (is (§ [inc dec [inc dec :foo]] [0 0 [0 0 0]])
      [1 -1 [1 -1 :foo]]) ;; don't forget that :foo is a :constant and return itself when invoked

  ;; but wait you can feed several arguments too!
  (is (§ [add sub add] [1 2 3] [1 2 3] [1 2 3])
      [3 -2 9])

  ;; it leaves extra indexes as is
  (is (§ [inc dec] [0 1 2 3])
      [1 0 2 3])

  ;; maps
  (is (§ {:a inc :b dec :c [inc dec]}
       {:a 0 :b 0 :c [0 0]})
      {:a 1 :b -1 :c [1 -1]})

  ;; several args
  (is (§ {:a add :b sub}
       {:a 1 :b 2}
       {:a 1 :b 2})
      {:a 2 :b 0})

  ;; extra keys are left as is
  (is (§ {:a inc}
       {:a 0 :b 0})
      {:a 1 :b 0})

  ;; if extra keys are present in several args the last is kept
  (is (§ {:a add} {:a 1 :b 2} {:a 1 :b 7})
      {:a 2 :b 7})

  )

 ;; the object convention

 (_

  ;; in asparagus, many functions takes what we can call the object as first argument
  ;; I mean, the thing we are working on, for instance, in the expression (assoc mymap :a 1 :b 2), mymap is what we call the object
  ;; all functions that can be viewed this way, will always take the 'object' as first argument
  ;; with this simple convention we can achieve a regularity that yield to easier function composition

  ;; the subjectify function will help to turn this kind of function into a one that takes only the arguments (in the previous exemple: :a 1 :b 2)
  ;; and return a function that takes only the target object, and return the result.
  (let [assoc_ (subjectify assoc)
        assoc-a-and-b (assoc_ :a 1 :b 2)]
    (assoc-a-and-b {})) ;;=> {:a 1 :b 2}

  ;; many of the asparagus functions that follow this convention, have their subjectified version with the same name suffixed with _
  ;; this is handy, for instance, to create chains of 1 argument functions
  (> myseq (take_ 3) (dropend_ 2)) ;; will thread 'myseq thru 2 functions, the semantics is analog to core/-> but it is a function
  ;; the '> function is defined in the :invocation-application-mapping section of asparagus.core
  (>_ (take_ 3) (dropend_ 2)) ;; will return a function that wait for its first argument ('myseq in the previous example)
  )

 ;; guards

 (_

  ;; one other thing that ease function composition is what I call guards (for lack of better name)
  ;; guards differs from predicate by the fact that they can either return nil or something (in most case the given 'object' unchanged)
  ;; so they can be used like predicates, but do not stop the flowing data
  ;; therefore they can be chained via function composition

  ;; some examples of guards
  (vec? [1 2])     ;;=> [1 2]
  (vec? (lst 1 2)) ;;=> nil
  (pos? 1)         ;;=> 1
  (pos? -1)        ;;=> nil

  ;; as we've seen we can chain them like this
  (let [g (>_ num? pos? (gt_ 2))] ;; gt is greater-than
    (g 3))                        ;;=> 3

  ;; but + does the same
  (let [g (+ num? pos? (gt_ 2))]
    (g 3))

  ;; collection guards ----------------------

  ;; $?
  ;; check if all values of a datastructure are not nil (see 'iterables section)
  (is ($? [1 2 3])
      [1 2 3])

  (!! (nil? ($? [1 nil 2 3])))

  (is ($? {:a 1 :b 2})
      {:a 1 :b 2})

  (nil? ($? {:a 1 :b nil}))

  ;; ?$ is a composition of $ and ?$
  ;; it can be viewed as a map operation that succed if all values of the resulting collection are non nil

  (is (?$ [2 3 4 5] num? inc (gt_ 2))
      [3 4 5 6])

  (!! (nil? (?$ [3 4 1 5] num? inc (gt_ 2))))

  ;; ?zip
  ;; the zip variant

  (is (?zip #(pos? (add %1 %2)) [1 2 3] [1 2 3])
      (lst 2 4 6))

  (!! (nil? (?zip #(pos? (add %1 %2)) [1 2 3] [1 2 -3])))

  ;; ?deep
  ;; a deep variant of $?
  ;; check if all nested values are non nil
  (check
   (nil? (?deep {:a {:b 1 :c [1 2 nil]}}))
   (nil? (?deep {:a {:b 1 :c [1 2 3 {:d nil}]}}))
   ;; succeed
   (?deep {:a {:b 1 :c [1 2 3]}}))

  ;; creating guards --------------------------

  (let [g (guard.unary c/odd?)]
    (g 1)) ;;=> 1

  (let [g (guard.binary c/>=)]
    (g 2 1)) ;;=> 2

  (let [g (guard.variadic c/>=)]
    (g 8 8 7 6 5 2)) ;;=> 8

  ;; or simply
  (let [g (guard:fn c/>=)]
    (g 8 8 7 6 5 2))

  ;; the guard macro
  ;; it has the same syntax than the f macro
  ;; but the resulting function will return the first argument unchanged if its body succeeds, otherwise nil
  (is ::ok
      (let [g (guard [x] (odd? (count x)))]
        (and
         (eq (g [1 2 3]) [1 2 3])
         (nil? (g [1 2 3 4]))
         ::ok)))

  ;; wrapping and importing predicates 

  (E+ (guards.import [odd? 1] [even? 1]))

  (is 1 (odd? 1))
  (isnt (even? 1))

  )

 ;; flow

 (_

  ;; ?> --------------------------------------------------------------
  ;; thread the object thru guards shorting on first nil result

  (is 1 (?> 1 num? pos?))
  (isnt (?> 1 num? neg?))

  ;; we can modify the seed too
  (eq 2 (?> 1 num? pos? inc))

  ;; shorts after str? (else it would be an error)
  (isnt (?> 1 str? (+_ "aze")))

  ;; more exemples
  (eq 3 (?> [1 2 3] (guard:fn (+ c/count c/odd?)) last))
  (nil? (?> [1 2] (guard [x] ((+ c/count c/odd?) x)) last))

  ;; more composed exemple
  ;; ?> use § under the hood,
  ;; so anything that implement invocation is allowed
  (!! (?> -1
          num? ;;=> -1
          (c/juxt (add_ -2) (add_ 2)) ;;=> [-3 1]
          [neg? (?>_ num? pos?)] ;; using _ version
          ))


  ;; ?< --------------------------------------------------------------
  ;; trying all given guards against x until first non nil result

  (is 1 (?< 1 coll? num?))
  (isnt (?< 1 str? coll? sym?))

  ;; build a guard
  ;; that succeed for numbers or strings
  (let [f (?<_ num? str?)]
    (eq [1 "a" nil]
        [(f 1) (f "a") (f :a)]))

  ;; basic composition with ?< and ?>_
  (is 42
      (?< 44
          str?
          (?>_ num? (gt_ 10) dec dec)))

  ;; ?c --------------------------------------------------------------
  ;; a clojure-cond(ish) function

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

  ;; (non function values act as constant functions)
  (is :pouet
      (?c "a"
          str? :pouet
          pos? inc
          neg? dec))

  ;; same with ?c_
  (is -2
      (let [f (?c_
               str? :pouet
               pos? inc
               neg? dec)]
       (f -1)))

  ;; ?c> --------------------------------------------------------------
  ;; a scheme-cond(ish) function

  (is -8
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

  (is 5
      (let [f (?c>_
               [str? :pouet]
               [pos? inc inc inc]
               [neg? dec dec (p mul 2)])]
        (f 2)))

  )

 ;; 'df (data function)

 (_

  ;; df: data function,
  ;; create a function from a data structure that
  ;; apply all functions contained in it (deeply) to further args.
  ;; preserve original structure

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

  )

 ;; composing data flow

 ;; with guards, shortcircuiting binding/lambda forms (?let, clet, cf, ?f...) ,
 ;; invocable datastructures, data functions, conditional functions (?c and ?c>),
 ;; guard connectors (?< and ?>)

 (!!
  (?> ["foo" 0]
      ;; with invocable data we can go inside the flowing data
      [
       ;; we check if the first idx is a :word (str, sym or keyword),
       ;; if yes cast it to keyword
       (?>_ word? key)

       ;; with data functions we can do sort of the opposite (wrapping instead of going inside)
       ;; (here we receiving 0 and returning {:val 0, :inc 1, :dec -1}
       (df {:val id :++ inc :-- dec})]

      (case_ [:bar x] {:bar x}
             [:foo (& x (ks val))] ;; we check that data idx 0 is :foo, and that the idx 1 has a :val key
             (case x
               pos? {:positive-foo x}
               neg? {:negative-foo x}
               {:zero-foo x})
             x {:fail x})

      (?c_ (?f1 {:fail x}) (f_ (pp "fail: " _) _) ;; shortcircuiting lambdas can be useful in those contexts
           (?f1 {:zero-foo x}) (f_ (pp "zero-foo " _) _)
           (f_ (pp "num-foo " _) _))

      ))
 )

;; ------------------------------------------------------------------------
;;                         environment (continued)
;; ------------------------------------------------------------------------

(_

 ;; it's time to go deep into the environment and the E+
 ;; let's talk about metaprograming first
 ;; as any Lisp, asparagus has metaprograming facilities (e.g macros but not only)
 ;; asparagus macro system is a bit different than regulars Lisps

 ;; it use a technique introduced by Daniel Friedman called "expansion passing style"
 ;; in regular Lisps, macro calls are recursively expanded until no more macro calls appears in the resulting expression
 ;; it results in more concise macro definition, but is less powerfull and prevents you to do more advanced things

 ;; a macros in asparagus is a function that takes 2 arguments:
 ;; the current environment (in this, it can reminds you Fexprs, more precisely 'compile time fexprs')
 ;; the list of operands of the macro call

 ;; since macros have access to the environment, and are responsable to thread expansion further
 ;; they can do all sort of transformations/updates on the environment before passing it to further expansions
 ;; therefore a macro has full control over its operands, which seems legitimate to me.

 ;; another interesting thing about asparagus macros, occuring from the fact that there is no automatic recursive expansions
 ;; is that functions and macros can share the same name. you may ask yourself what is the point :)
 ;; in fact in clojurescript, this kind of technique is used for compile time optimizations for exemple.

 ;; macros ------------------

 (_
  ;; a simple macro definition

  (E+ postfix:mac ;; note the :mac prefix
      (fn [e args]
        (reverse args)))

  ;; let's expand this form in the current global environment using 'exp
  (exp @E '(postfix 3 2 1 add))
  ;;=> (add 1 2 3)

  ;; in trivial cases like this one it works but if the operands contains some macro calls, they will not be expanded
  (exp @E '(postfix (postfix 5 4 add) 3 2 1 add))
  ;;=> (add 1 2 3 (postfix 5 4 add))

  ;; as we've said, we have to thread the expansion further
  ;; we've just seen the 'exp function that perform expansion given an environment and an expression
  (E+ postfix:mac
      (fn [e args]
        (reverse
         ;; we are mapping the expansion over the arguments, with the same environment e
         ($ args (p exp e)) ;; equivalent to (clojure.core/map (partial exp e) args)
         )))

  ;; therefore
  (exp @E '(postfix (postfix 5 4 add) 3 2 1 add))
  ;;=> (add:val 1 2 3 (add:val 4 5))

  (!! (postfix (postfix 5 4 add) 3 2 1 add))
  ;;=> 15

  ;; it is not a high price to pay I think, given the flexibility and power it can provide

  ;; You can do many crazy things with this behavior, but you don't have to.
  ;; I personaly tends to prefer technologies that let the power to the user
  ;; even if I agree that strong opinions and good practices enforcement can yield to a powerfull language and strong community (like clojure)
  ;; Its a matter of taste and needs after all (at a point in time)

  ;; all the binding forms and lambda macros are defined this way, so maybe its time to look at the asparagus.core namespace
  )

 ;; regular macros with the 'mac macro -------

 (_
  ;; and as you may have guessed, we can implement regular Lisp behavior in terms of those semantics
  ;; using the builtin 'mac macro, we will define a dummy 'fi macro (same as clojure's if-not)

  (E+ fi:mac
      ;; the 'mac macro let you define a macro in a standard way
      (mac [p f t] ;; we do not receive the environment
           '(if ~p ~t ~f) ;; we don't thread the expansion, it will be automatically done
           ))

  (exp @E '(fi (pos? 1)
               (fi (neg? 1) :zero :neg)
               :pos))

  ;; and operands are expanded as they would be with regular Lisp macro
  '(if (guards.builtins.pos?:val 1)
     :pos
     (if (guards.builtins.neg?:val 1)
       :neg
       :zero)))

 ;; dual stage ------

 (_
  ;; our fancy-add function, will check the presence of litteral numbers in its operands and preprocess them at compile time
  ;; living others operands as is for runtime
  (E+ fancy-add
      { ;; compile time behavior
       :mac
       (f [e xs]
          (let [lit-nums (shrink+ xs num?) ;; we grab all litteral numbers
                others (shrink- xs num?)]  ;; and we keep others operands
            ;; we return the preprocessed form
            '(fancy-add:val ;; we have to explicitly write the :val suffix in this case
              ;; we peform the compile time work
              ~(* add lit-nums)
              ;; we thread the expansion mapping cxp (composite expand) on others operands and splice the result
              .~($ others (p cxp e)))))

       ;; runtime behavior, a normal addition
       ;; using the scheme's variadic args syntax and the composite syntax (the dot)
       :val
       (f xs (add . xs)) ;; equivalent to (fn [& xs] (apply add xs))
       })

  (exp @E '(let [a 1]
             (fancy-add 1 a 2)))
  ;;=> (clojure.core/let [a_152671 1]
  ;;     (fancy-add:val 3 a_152671))

  (!! (let [a 1]
        (fancy-add 1 a 2))) ;;=> 4
  )

 ;; substitutions --------------

 (_

  ;; 'substitutions' are another metaprograming device that deserve attention i think
  ;; it gives the user a way to replace a simple symbol with an arbitrary expression

  ;; trivial substitution
  (E+ macro-exemple.foo:sub ;; :sub attribute denotes a substitution function 
      (fn [e]
        ;; we return a simple keyword, but this could have been any expression
        ;; (built or not with the help of the environment. bound to 'e here)
        :foofoofoo))

  (exp @E 'macro-exemple.foo) ;;=> :foofoofoo

  ;; substitution are performed at expansion time, like macros

  ;; another simple substitution
  ;; a really dummy exemple just to demonstrate that you have access to the environment

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

  ;; so now it is substituted by what's in substitution-exemple.bar:val
  (exp @E 'substitution-exemple.bar)    ;=> 42

  (env-inspect 'substitution-exemple.bar)
  )

 ;; updates --------------------

 (_
  ;; extending E+ behavior with the :upd attribute

  ;; it can hold a function that creates an update datastructure (like the one E+ takes), hold on, see the exemple:

  ;; we declare an update called tagged
  (E+ tagged:upd ;; note the :upd suffix
      (f [e [tag data]]
         ;; an update function has to return any datastructure that is understood by E+
         ;; like expansions and substitutions it has access to the environment (not used in this case)
         [:tag tag :val data]))

  ;; we can use it in an E+ form like this
  (E+ upd-function-demo.foo (tagged :my-tag 42))

  ;; when E+ see an :upd call it will execute it and substitute it with its return value
  ;; in this case it will be equivalent to this
  (E+ upd-function-demo.foo [:tag :my-tag :val 42])

  (env-inspect 'upd-function-demo.foo)

  ;; this technique is used at several place in asparagus source (feel free to look at it)
  ;; contrary to macros and substitutions, updates are recursivelly executed,
  ;; so an update functions can return an expression which is another update call, which will be further processed
  )

 ;; effects -----------------------

 (_
  ;; sometimes you need to do dirty/real things,
  ;; :fx let you write an expression that will be executed at environment extension time
  ;; the expression being previously compiled with the current environment
  (E+ fx-demo.foo [:fx (println "defining fx-demo.foo") :val 42])

  ;; in practice it is used for things like, extending a protocol for exemple.
  ;; see the 'generic section (which wrap clojure's protocols)

  ;; all those special attributes may appears abstract at this time,
  ;; but looking at asparagus source (that is highly documented) will show them in practice, and it will become more clear I hope

  )
 )

;; ------------------------------------------------------------------------
;;                              generics
;; ------------------------------------------------------------------------

(_

 ;; generic functions are at heart of asparagus, every core operations are defined this way

 ;; mono arity

 (_
  ;; we will define a my-generic function and implement it for some built in types

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

 ;; poly arity

 (_

  ;; as clojure.core/fn generics a multi arity syntax
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
       ([a b . (& c [c1 . cs])] ;; you can put any asparagus binding pattern in arguments
        ;; default case
        {:my-generic2-variadic-arity
         {:a a :b b :c c :c1 c1 :cs cs}})
       ))

  (check
   (eq (my-generic2 [1 2 3]) {:coll [1 2 3]})
   (eq (my-generic2 "iop") {:something "iop"})
   (eq (my-generic2 (lst 1 2 3) 42) {:line (lst 1 2 3) :extra-arg 42})
   (eq (my-generic2 :iop 42) {:my-generic2-arity2-default-case [:iop 42]})
   (eq (!! (my-generic2 [1 2 3] 1 :iop {}))
       {:my-generic2-variadic-arity
        {:a [1 2 3] :b 1
         :c (lst :iop {})
         :c1 :iop
         :cs (lst {})}})
   )
  )

 ;; inspection, extension

 (_
  ;; we can inspect your generic like this
  (!! (my-generic2.inspect))

  ;; extension
  ;; here we will define an arity2 implementation for vectors
  ;; with clojure protocols, if we extend a protocol to our type, we have to implement all arities
  ;; in asparagus this is not nescessary

  (E+ (my-generic2.extend
       [a b]
       :vec {:vec a :extra-arg b}))

  (!! (my-generic2 [1 2 3] "iop"))
  ;; we still benefits from the others arity implementations
  (!! (my-generic2 [1 2 3]))
  (!! (my-generic2 [1 2 3] 1 2 3))

  ;; the extend form is letting you implement several things at once
  ;; it has the same syntax as the initial definition

  (E+ (my-generic2.extend
       ([a]
        :num {:num a}
        :str {:str a}
        :seq {:seq a})
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

 ;; a la carte polymorphism

 (_
  ;; one consideration that came to my mind and that is experimented at the end of asparagus.boot.generis
  ;; is that each implementation should be callable directly when there is no need for polymorphism
  ;; candidate syntaxes would be:
  ;; (my-generic2_vec my-vec arg1 arg2)
  ;; (my-generic2.case :vec my-vec arg1 arg2)
  ;; it seems reasonable to be able to disambiguate when we can do so.
  ;; and for critical code it can speed things a bit...
  ;; it looks like best of both world to me
  )
 )

;; ------------------------------------------------------------------------
;;                                types
;; ------------------------------------------------------------------------

(_

 ;; simple
 (E+ (type+ :split [left right]))

 ;; definition
 (E+ (type+ :mytyp ;;typetag

            [bar baz] ;; fields

            ;; generic implementations ...
            (+ [a b]
               (!let [(:mytyp b) b]
                     (mytyp (+ (:bar a) (:bar b))
                          (+ (:baz a) (:baz b)))))))

 (!! (+.inspect))

 ;; instantiation
 (!! (mytyp 1 2))
 (!! (mytyp? (mytyp 1 2)))
 (!! (map->mytyp {:bar 1 :baz 2}))

 ;; type
 (!! (type (mytyp 1 2))) ;;=> :mytyp

 ;; using generic implmentations
 (!! (+ (mytyp 1 2) (mytyp 1 2) ))

 )

;; ------------------------------------------------------------------------
;;                            object orientation
;; ------------------------------------------------------------------------

(_

 ;; a light way to mimic object oriented programming in clojure is to put methods inside a map

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

   )

 ;; here's an experimental way to create objects that shares prototypes

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

 (env-inspect 'named)

 (!! (let [o (named "Bob")] (:greet o "Joe")))

 (!! (let [o (person "Bob" "Wallace")]
       (lst (:greet o "Joe")
            (:walk o)
            person.proto)))



 )

;; ------------------------------------------------------------------------
;;                              dive and tack
;; ------------------------------------------------------------------------

;; dive

;; the dive generic function, let you get something inside something else
;; its first argument represent the address of what you want to get
;; the second is the thing in which you want to find it
;;
;; it is like core/get but with arguments reversed, and being a generic function, it can be extended.

;; for key and syms it does what core/get had done
(is 1 (dive :a {:a 1}))
(is 1 (dive 'a {'a 1}))

;; for nums it search an idx
(is :io (dive 2 [0 0 :io 0]))
;; negative idxs supported
(is :io (dive -1 [0 0 :io]))

;; for vector it goes deep
(is :io (dive [:a :b] {:a {:b :io}}))
;; but any valid diving-address can be used
(is :io (dive [:a -1] {:a [0 0 0 :io]}))

;; maps let you compose a return value
(is (dive {:one [:a :b] :two [:c 1 :d]}
          {:a {:b :io} :c [0 {:d 42} 0]})
    {:one :io
     :two 42})

;; it also accpets raw functions
(is 1 (dive inc 0))
;; which does not seems to really make sense at first but it can be handy in more complex dives
(is 1
    (dive [:a num? inc]
          {:a 0}))

;; Also, we can mention that it is a concrete exemple of something that is a function and a macro at the same time
;; here we use a technique that is analog to the one we used in bind (binding operators)
;; the dive module holds a map of operations implementations in dive.ops
;; At expansion time, if the first argument to dive is an sexpr, the verb will be searched in dive.ops
;; if an implementation is found, it will be executed (at expansion time) and the return value will take the place of the original expression
;;
;; as an exemple, we use the 'ks operation
(!! (dive (ks a b) {:a 1 :b 2 :c 2}))
;; ks is resolved in dive.ops and applied to the given args (here :a and :b), producing this form
(dive (fn [y] (select-keys y [:a :b]))
      {:a 1 :b 2 :c 2})

;; functions implement dive so the expansion time work is done, the form will now ready for runtime

;; As you may have deduced by yourself, dive.ops can be extended with new operations
;; Keep in mind that it will not alter all previous call to dive, which are already compiled. (this is a good thing :))

;; extension 

(_

 (E+ dive.ops:val
     {:wtf
      (f1 [x] '(f_ [:wtf ~x _]))})

 (is (dive (wtf 42) {:a 1 :b 2})
     [:wtf 42 {:a 1 :b 2}])

 (E+ (dive.op+ wtf [a]
               '(f_ [:wtf ~a _])))



 (exp @E '(dive (wtf 43) {:a 1 :b 2}))
 (!! (dive (wtf 43) {:a 1 :b 2}))

 (env-inspect 'dive.ops)
 (is (!! (dive (wtf3 43) {:a 1 :b 2}))
     [:wtf3 43 {:a 1 :b 2}])

 (E+ foo 42)
 (exp @E '(f1 [arg1] '(f1 sd [:wtf3 ~arg1 sd])))

 (bubfind @E (path 'f1)))

;; quoting -----------------------------------------

(_ :xp1

    (E+ foo 42)

    (exp @E 'foo)

    (E+ foo:sub (f_ 'foo:val))

    (defmacro expquote [x]
      `(exp @E ''~x))

    (exp @E ''(f [a b c] a))

    (expquote (f [a b c] a))

    (c/eval (exp @E (c/eval (expquote (f1 [arg1] '(f1 sd [:wtf3 arg1 sd]))))))

    (!! '(f1 [arg1] '(f1 sd [:wtf3 arg1 sd])))

    ;; lambda exp
    (exp @E '(f [a b c] a))
    (exp @E '(f1 a a))
    (exp @E '(f1 _ _))
    (exp @E '(f_ _))

    (!! ((lambda.compiler) @E '([_a] _a)))

    (!! (hygiene.shadow.gensym? '_))

    (!! (cadr 'abc))

    (E+ quote:mac
        (fn [e [x]]
          (pp "exp quot " x)
          (cp.expand (fun.alt e 0 x))))

    (E+ foo 42)

    (exp @E
         '(let [foot 12 b :sd c 1 d (quot yop)]
            #_(quote.fun:alt e 0 (lst ))
            '(foo ~b '(c ~'~d))))

    (let [foot 12 b :sd c 1 d (quot yop)]
      #_(quote.fun:alt e 0 (lst ))
      '(foo ~b '(c ~'~d)))

    (!! '(1 '~(add 1 ~(add 2 3)) 4))
    (!! (list 1 (list (sym "quote") (list `unquote (list 'add '1 (add 2 3))))))

    (expquote (1 '~(add 1 ~(add 2 3)) 4))

    (do '~a)

    (env-inspect 'b)

    (holycoll? '(a b c))

    (quote? (lst 'quote 1))

    '(a b c `(r t y))

    (!! (hygiene.shadow @E 'aze_123))

    (defmacro xp [x]
      `(exp @E '~x))

    (xp (f [z_123] z_123))

    (E+ q'
        [:links {cp composite}
         (fn [e form]
           #_(pp "io")
           (cp form
               ;; we do not touch dots
               ;; they will be handled via composite.expand after quoting
               cp.dot? cp.dot
               cp.dotdot? cp.dotdot
               ;; if unquote we perform expansion with e
               unquote? (exp e (second form))
               ;; handle collections
               seq? (cons `list ($ form (p rec e)))
               holycoll? ($ form (p rec e))
               ;; else we quote wathever it is
               (quote.fun.wrap form)))
         :mac
         (fn [e [x]]
           (cp.expand
            (q':val e x)))])

    (E+ quote:mac q':mac)

    (ppenv quote.fun.wrap)

    (!! (let [a 1] '(a b ~(add 1 2) ~{a a})))

    (first (first '(~r)))
    )

(_ :uncomplected-quotes

  ;; quoting, qualifying, splicing, unquoting

  'sq   ;; quasiquote
  'sqq  ;; quasiquote qualified
  'sqq! ;; quasiquote qualified strict

  (E+ quotes
      [:links {cp composite}

       wrap
       (fn [x] (list (symbol "quote") x))
       rootsym
       (fn [p] (path->sym (path (symbol "_") p)))

       mk
       (fn [{:keys [strict qualified]}]
         (fn [e form]
           #_(pp "io")
           (cp form
               ;; we do not touch dots
               ;; they will be handled via composite.expand after quoting
               cp.dot? cp.dot
               cp.dotdot? cp.dotdot
               ;; if unquote we perform expansion with e
               unquote? (cxp e (second form))
               ;; handle collections
               seq? (cons `list ($ form (p rec e)))
               holycoll? ($ form (p rec e))
               symbol?
               (cs strict
                   (let [[p _] (assert (bubfind e (path form))
                                       (str "unqualifiable symbol: " form " "))]
                     (quotes.wrap (quotes.rootsym (path form))))
                   qualified
                   (cs [p (path form)
                        [p v] (bubfind e p)]
                       (quotes.wrap (quotes.rootsym p))
                       (quotes.wrap form))
                   (quotes.wrap form))
               ;; else we quote wathever it is
               (quotes.wrap form))))

       sq:mac  (fn [e [x]] (let [f (quotes.mk {})] (cp.expand (f e x))))
       sqq:mac (fn [e [x]] (let [f (quotes.mk {:qualified true})] (cp.expand (f e x))))
       sq!:mac (fn [e [x]] (let [f (quotes.mk {:qualified true :strict true})] (cp.expand (f e x))))

       quote?
       (fn [e x]
         #_(pp "quote?" x
               (and (seq? x)
                    (#{(path 'quotes.sq:mac)
                       (path 'quotes.sqq:mac)
                       (path 'quotes.sq!:mac)}
                     (or (qualsym e (car x))
                         (qualsym e (sym (car x) :mac)))))
               "_")
         (and (seq? x)
              (#{(path 'quotes.sq:mac)
                 (path 'quotes.sqq:mac)
                 (path 'quotes.sq!:mac)}
               (or (qualsym e (car x))
                   (qualsym e (sym (car x) :mac))))))

       mk2
       (fn [{:keys [strict qualified]}]
         (fn [e lvl form]
           #_(pp "mk2 in" lvl form "_")
           (cp form
               ;; we do not touch dots
               ;; they will be handled via composite.expand after quoting
               cp.dot? cp.dot
               cp.dotdot? cp.dotdot
               ;; if unquote we perform expansion with e
               unquote?
               (cs (zero? lvl)
                   (cxp e (second form))
                   (list `list (quotes.wrap `unquote) (rec e (dec lvl) (second form))))

               ;; if nested quote 
               (p quotes.quote? e)
               (list `list (quotes.wrap (car form)) (rec e (inc lvl) (second form)))
               ;; handle collections
               seq? (cons `list ($ form (p rec e lvl)))
               holycoll? ($ form (p rec e lvl))

               symbol?
               (cs strict
                   (let [[p _] (assert (bubfind e (path form))
                                       (str "unqualifiable symbol: " form " "))]
                     (quotes.wrap (quotes.rootsym (path form))))
                   qualified
                   (cs [p (path form)
                        [p v] (bubfind e p)]
                       (quotes.wrap (quotes.rootsym p))
                       (quotes.wrap form))
                   (quotes.wrap form))
               ;; else we quote wathever it is
               (quotes.wrap form))))

       sq:mac  (fn [e [x]] (let [f (quotes.mk2 {})] (cp.expand (f e 0 x))))
       sqq:mac (fn [e [x]] (let [f (quotes.mk2 {:qualified true})] (cp.expand (f e 0 x))))
       sq!:mac (fn [e [x]] (let [f (quotes.mk2 {:qualified true :strict true})] (cp.expand (f e 0 x))))]

      (import quotes [sq sqq sq!])

      :fx
      (check
       (sq (add 1 2 ~(+ [] (lst 1 2))))
       (sqq (add 1 2 a ~(sip [] . (lst 1 2))))
       ;; (throws (sq! (add 1 2 a ~(+ [] (lst 1 2)))))
       (sqq (add 1 2 a ~(sip [] . (lst 1 2))
                 (sqq (a b c ~(add 1 2 ~(add 3 4)))))))

      ))


