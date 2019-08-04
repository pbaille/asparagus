(in-ns 'asparagus.core)

(init-top-forms
     let lut ?let !let !lut
     ?f !f f fu !fu
     f1 !f1 ?f1 !fu1 fu1
     f_ !f_ ?f_ !fu_ fu_
     cf ?cf !cf cfu !cfu 
     clet clut !clet !clut
     case casu !case !casu)

;; asparagus is my last experience in language design 
;; it is a clojure embedded language
;; it differs from clojure on several crucial points
;; for instance it does not use clojure namespaces nor clojure macro system

;; you are still reading? :)

;; ok let's go

;; ------------------------------------------------------------------------
;;                            the environment
;; ------------------------------------------------------------------------

;; you can define a variable like this (E+ stands for extend-environment)
;; its like a really (really!) fancy 'def

(E+ foo 1)

;; or several at once

(E+ bar \a
    baz 42)

;; for now we will use !! macro to evaluate forms, but later it will no longer be needed

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

;; dot notation can be used in definitions to

(E+ mymodule.c.f 2
    foo.bar 'foob)

;; this definition does not overide our previous ones

;; foo is still defined

(!! foo) ;=> 1
(!! foo.bar) ;=> 'foob

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
;; Certainly I can create a stats namespace holding those helpers, but... it seems heavy to me for sush a common/natural thing...

(!! (stats 1 2 3 4)) ;=> {:xs (1 2 3 4), :sum 10, :mean 5/2}

;; it could be defined with a map literal to

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
        :mean (.mean xs)})]})

;; strings literals represent documentation
;; (a bold choice maybe... but maybe not so much if we really want to make documentation a first class citizen)
;; and I've said to myself, maybe hardcoded string in code are not so common? far less than keywords for instance

(E+ myvar
    ["myvar doc" 42])

(!! (eq "myvar doc"
        myvar:doc))

;;  is equivalent to

(E+ myvar {:val 42 :doc "myvar doc"})

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
        :mean (.mean xs)})]})

;; so you may have a question now :)
;; If hashmaps, vectors and strings have special semantics in E+,
;; how can I use them as normal values for my variables?
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

;; bubbling resolution
;; use absolute and relative paths for all our vars is kind of painfull and ugly
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

;; links

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

(keys @E)

;; ------------------------------------------------------------------------
;;                           compilation steps
;; ------------------------------------------------------------------------

;; there is three fundamentals compilation steps in asparagus

;; qualify ;; will try to turn all symbols of the given expression into path objects
;; expand ;; macro and substitutions will be executed here
;; resolve ;; paths are turned to their corresponding var syms

;; ------------------------------------------------------------------------
;;                          data primitives
;; ------------------------------------------------------------------------

;; literals works the same way as clojure ones (except for some extensions that will be explained later)

{:a 1}
[1 2 3]
#{1 2}
"hello"
:iop

;; the quote is different from clojure's one,
;; so symbols and list litterals will not be written as in clojure

;; a quoted sym
(quot mysym) ;=> 'mysym
(sym "mysym") ;=> 'mysym

;; a list
(lst 1 2 3)

;; collections --------------------------------------

;; constructor functions
;; compared to clojure, the API have been uniformized

(vec 1 2 3) ;=> [1 2 3]
(lst 1 2 3) ;=> '(1 2 3)
(set 1 2 3) ;=> #{1 2 3}
(map [:a 1] [:b 2]) ;=> {:a 1 :b 2}

;; with sequential last argument (like core/list*)

(vec* (lst 1 2 3 4)) ;; with one argument it behaves like core.vec
(vec* 1 2 [3 4])

(lst* [1 2 3 4])
(lst* 1 2 [3 4])

(set* 1 2 [3 4])

(map* [:a 1] [:b 2] {:c 2 :d 5}) ;; same as (map* [:a 1] [:b 2] [[:c 2] [:d 5]])

;; preds
;; each collection have its pred, that returns the given collection on success or nil otherwise

(vec? [1 2 3]) ;=> [1 2 3]
(lst? (lst 1 2 3)) ;=> '(1 2 3)
(set? #{1 2 3}) ;=> #{1 2 3}
(map? {:a 1}) ;=> {:a 1}

;; we will see that in asparagus we avoid predicates (functions that returns booleans)
;; in favor of guards (functions that can return nil indicating failure, or data)
;; for instance (pos? 1) will be, i think, more useful if it returns 1 in case of success and nil otherwise
;; this way it can be composed more easily i think.
;; more on control flow, shortcircuiting and stuff later...

;; words -------------------------------------------

;; constructors
;; symbols and keywords have their core/str(ish) construtors

(sym "foo") ;=> 'foo
(key "foo") ;=> :foo

(sym :foo "bar") ;=> 'foobar
(key "foo" :bar "baz") ;=> :foobarbaz

;; star variants

(sym* "ab" (lst "cd" "ef" "gh")) ;=> 'abcdefgh
(key* "my" :keyword "_" [:foo :bar "baz"]) ;=> :mykeyword_foobarbaz
(str* "mystr_" ["a" "b"]) ;=> "mystr_ab"

;; guards
;; as for collections, we use guards instead of preds

(key? :iop) ;=> :iop
(sym? (quot bob)) ;=> 'bob
(str? "hi") ;=> "hi"

;; ------------------------------------------------------------------------
;;                          joining things
;; ------------------------------------------------------------------------

;; joining things together with +
;; ------------------------------

(is (+ [1 2] '(3 4))
    [1 2 3 4])

(is (+ '(1 2) [3 4])
    '(1 2 3 4))

(is (+ {:a 1 :b 0} {:b 2})
    {:a 1 :b 2})

;; + is variadic
(is (+ #{} '(1 2) [3 4] #{3 5})
    #{1 2 3 4 5})

;; as you have seen, the return type is determined by the first argument

;; strs syms and keywords

(is (+ 'foo "bar") 'foobar)
(is (+ :foo 'bar) :foobar)
(is (+ "foo" 'bar :baz) "foobar:baz")

;; on function it do composition
;(left to right, not like core.comp do)
(is ((+ inc inc (p mul 2)) 0)
    4)

;; unlike clojure.core.concat, + is an extensible operation that can be implementated by user types


;; sip add one or several element into something
;; ---------------------------------------------

(is (sip [] 1 2)
    [1 2])

;; for lists it adds at the end (not like conj do)
;; it is a choice that can be disctable, in my own pratice i'm not realying often on way that clojure lists implements conj
;; sip being a generic operation (extendable by user types) we could add a datatype that conj elements at its head like clojure lists...
(is (sip '(1 2) 3)
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

;; 'pure returns the empty version of the given argument
;; ---------------------------------------------------

(is (pure "foobar") "")

(is (pure {:a 1 :b 2}) {})

(is (pure inc) id)

;; like sip and +, pure is a generic operation that can be implemented by user types

;; pure? test for purity
;; ---------------------

(is {} (pure? {}))

(nil? (!! (pure? {:a 1})))

;; composite collections litterals
;; -------------------------------

;; vectors ----

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
  )

;; maps ---------

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
  )

;; lists ----------

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
  )

;; binding forms ---------------------------------------------------------------------------

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

;; let variants
;; ------------

;; ?let (shortcircuiting let)
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

;; !let (strict let)
;; is like ?let but with implicit prefix !, it support ? and _ prefixes
(eq :catched
    (try (!let [a nil] :never)
         (catch Exception _ :catched)))

;; lut (unified let)
;; in a unified let, all symbols that appears several times have to bind to the same value (equal values)

(lut [a 1 a (dec 2)] :success)

(lut [a 1
      a 2] ;; this will shorts because a is already bound to 1
     (error "never thrown"))

;; !lut (unified strict let)

(eq :catched
    (try (!lut [a 1
                a 2] ;; this will throw because a is already bound to 1
               :never)
         (catch Exception _ :catched)))

;; destructuration -------------------

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

;; binding operators -------------------

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

;; special bindings ---------------------

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

;; value patterns -----------------------

;; any value can be used in pattern position,

(eq :ok (let [a (inc 2)
              3 a] ;; 3 is in binding position, therefore the seed (a) is tested for equality against it, and it shorts if it fails
          :ok))

(let [a (inc 2)
      4 a]
  (error "never"))

;; user type patterns -------------------

;; any type can implement the 'bind operation and join the party
;; TODO

;; cased let ----------------------------

;; cased let is like a cascade of shortcircuiting let forms
;; it can be be compared to cond-let but is more powerful

;;simple
(eq (clet [x (pos? -1)] {:pos x} ;first case
          [x (neg? -1)] {:neg x} ;second case
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

;; lambdas --------------------------------------------------------

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
 1 2 3 4) ;=> 10

;; the same can be acheive with 'rec
((f [x . xs]
    (if-not (c/seq xs) x
            (add x (rec . xs))))
 1 2 3 4) ;=> 10

;; like in scheme, binding pattern can be a simple symbol
;; this is the reason why we need keyword litteral to name lambdas (to disambiguate)
((f xs (add . xs))
 1 2 3 4) ;=> 10

;; shortcircuiting variant
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
       :yeah))

;; lambda syntactic sugar -----------------

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

;; case lambda ------------------------------

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

;; case ----------------------------------------

(let [x (range 12)]
  ;; try those values too:  42 "iop" :pouet
  (case x
    (num? x) {:num x} ;; first clause, x is a number
    (str? x) {:str x} ;; second clause, x is a string
    [x . xs] {:car x :cdr xs} ;; third clause, x is sequential
    :nomatch)) ;; the default value

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
         (vec? x) :vec)))

;; application and invocation --------------------------------------------------------

;; application and invocation are generic function that can be implemented for any type
;; those operations are so central in functional programming that i've decided to give them really short symbols
;; * for application 
;; § for invocation

;; a little help for better readibility
(defmacro is [& xs]
  `(!! (check (~'eq ~@xs))))

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

;; environment (continued) ----------------------------

;; it's time to talk about metaprograming
;; as any Lisp, asparagus has metaprograming facilities (e.g macros but not only)
;; asparagus macro system is a bit different than regulars Lisps

;; it use a technique introduced by Daniel Friedman called "expansion passing style"
;; in regular Lisps, macro calls are recursively expanded until no more macro calls appears in the resulting expression
;; it results in more concise macro definition, but is less powerfull and prevents you to do more advanced things

;; a macros in asparagus is a function that takes 2 arguments:
;; the current environment
;; the list of operands of the macro call

;; since macros have access to the environment, and are responsable to thread expansion further
;; they can do all sort of transformations/updates on the environment before passing it to further expansions
;; therefore a macro has full control over its operands, which tends to make more sense to me.

;; another interesting thing about asparagus macros, occuring from the fact that there is no automatic recursive expansions
;; is that functions and macros can share the same name. you may ask yourself what is the point :)
;; in fact in clojurescript, this kind of technique is used for compile time optimizations for exemple.

;; macros ------------------

;; a simple macro definition

(E+ postfix:mac
    (fn [e args]
      (reverse args)))

;; let's expand this form in the current global environment
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

;; substitutions --------------

;; 'substitutions' are another metaprograming device that deserve attention i think
;; it gives the user a way to replace a simple symbol with an arbitrary expression

;; trivial substitution
(E+ macro-exemple.foo:sub ;; :sub attribute denotes a substitution function 
    (fn [e] :foofoofoo))

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

(exp @E 'substitution-exemple.bar) ;=> (some expression)

(E+ substitution-exemple.bar 42) ;; not we define a :val

;; so now it is substituted by what's in substitution-exemple.bar:val
(exp @E 'substitution-exemple.bar) ;=> 42

(env-inspect 'substitution-exemple.bar)

;; updates --------------------

;; extending E+ behavior with the :upd attribute

;; it can hold a function that creates an update datastructure , hold on, see the exemple:

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
;; contrary to macros and substitutions, updates are recursivelly executed, so an update function can return an expression which is another update call

;; effects -----------------------

;; sometimes you need to do dirty (real) things,
;; :fx let you write an expression that will be executed at environment extension time
(E+ fx-demo.foo [:fx (println "defining fx-demo.foo") :val 42])

;; in practice it is used for things like, extending a protocol for exemple.
;; see the 'generic section (which wrap clojure's protocols)

;; all those special attributes may appears abstract at this time,
;; but looking at asparagus source will show them in practice, and it will become more clear I hope




;; iterables ------------------------------------------------------------------------



























(defmacro is [& xs]
  `(!! (~'eq ~@xs)))

;; application (*) and invocation (§) ----------------------------

;; functions -----

;; invocation
(is (§ add 1 2 3)
    6)

;; application
(is (* add 1 2 [3 4])
    10)

;; curried arity 1
(is ((§ add) 1 2 3)
    6)

(is ((* add) 1 2 [3 4])
    10)
