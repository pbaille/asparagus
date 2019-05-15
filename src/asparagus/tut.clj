(in-ns 'asparagus.core)

(init-top-forms
 let lut ?let !let !lut
 ?f !f f fu !fu
 f1 !f1 ?f1 !fu1 fu1
 f_ !f_ ?f_ !fu_ fu_)

;; asparagus is my last experience in language design
;; it is a clojure embedded language
;; it differs from clojure on several crucial points
;; for instance it does not use clojure namespaces nor clojure macro system


;; ------------------------------------------------------------------------
;;                            the environment
;; ------------------------------------------------------------------------


;; you can define a variable like this

(E+ foo 1)

;; or several at once

(E+ bar "iop"
    baz 42)

;; for now we will use !! macro to evaluate forms, but later it will no longer be needed

(!! foo)
(!! baz)

;; you can modularize definitions
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

(!! (add mymodule.a mymodule.c.d))

;; dot notation can be used in definitions to

(E+ mymodule.c.f 2
    foo.bar 'foob)

;; this definition does not overide our previous ones

;; foo is still defined

(!! foo) ;=> 1
(!! foo.bar) ;=> 'foob

;; one handy usage of this behavior is scoped helpers definition

(E+ stats.sum
    (fn [xs] (apl add xs))
    stats.mean
    (fn [xs] (div (stats.sum xs) (count xs)))
    stats
    (fn [& xs]
      {:xs xs
       :sum (stats.sum xs)
       :mean (stats.mean xs)}))

(!! (stats 1 2 3 4))

;; it could be defined with a map literal to

(E+ stats
    {;; the :val field hold the main value of the module
     ;; if 'stats' appears in the code this is the value we are refering to

     ;; note that the sum and mean helpers function (defined after) are available
     ;; when using map literal for definition, all members are available to each others
     :val
     (fn [& xs]
       {:xs xs
        :sum (.sum xs)
        :mean (.mean xs)})
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

;; any environment variable can have any number of those keyword fields

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
     (fn [xs] (div (..sum xs) (count xs)))

     ;; here the :val of stats (the :val keyword can be omitted)
     (fn [& xs]
       {:xs xs
        :sum (.sum xs)
        :mean (.mean xs)})]})

;; strings literals represent documentation

(E+ myvar
    ["myvar doc" 42])

(!! (eq "myvar doc"
        myvar:doc))

;;  is equivalent to

(E+ myvar {:val 42 :doc "myvar doc"})

;; finally we can redefine stats with that in mind

(E+ stats
    [sum
     (fn [xs] (apl add xs))

     mean
     ["given a seq of numbers, return the mean of it"
      (fn [xs] (div (..sum xs) (count xs)))]

     "returns a map of staistics concerning given numbers"
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

;; you may wonder about interop... we do not support it for now, have to think about it

;; bubbling resolution
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

;; the :links field let you define shorter accesses to other modules or members
;; when a non relative symbol is resolved its first segment will be searched in the current module links
;; if there is an existant links it will be substituted by it

(E+ links.demo
    {mod1 {a 1 b 2 c {d 3 e 4}}
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

;; there is more details concerning bubling and linking but we will skip for now

;; ------------------------------------------------------------------------
;;                           compilation steps
;; ------------------------------------------------------------------------

;; the asparagus environment is holded by the asparagus.core/E atom

(keys @E)

;; there is three fundamentals compilation steps in asparagus

;; qualify
;; expand
;; resolve

;; ------------------------------------------------------------------------
;;                          data primitives
;; ------------------------------------------------------------------------

;; disclaimer
;; in this section, 'map and 'key will be redefined, for some people it is far too much, for this I apologize...

;; literals works the same way as clojure ones (except for some extensions that will be explained later)

{:a 1}
[1 2 3]
#{1 2}
"hello"
:iop

;; the quote is different from clojure's one,
;; so symbols and list litterals will not be used as in clojure

;; a quoted sym
(quot mysym) ;=> 'mysym
(sym "mysym") ;=> 'mysym

;; a list
(lst 1 2 3)

;; collections --------------------------------------

;; constructor functions
;; compared to clojure the API have been uniformized

(vec 1 2 3) ;=> [1 2 3]
(lst 1 2 3) ;=> '(1 2 3)
(set 1 2 3) ;=> #{1 2 3}
(map [:a 1] [:b 2]) ;=> {:a 1 :b 2}

;; with sequential last argument (like core/list*)
(vec* 1 2 [3 4])
(lst* 1 2 [3 4])
(set* 1 2 [3 4])
(map* [:a 1] [:b 2] {:c 2 :d 5})

;; preds
;; each collection have its pred, that returns the given collection on success or nil otherwise

(vec? [1 2 3]) ;=> [1 2 3]
(lst? (lst 1 2 3)) ;=> '(1 2 3)
(set? #{1 2 3}) ;=> #{1 2 3}
(map? {:a 1}) ;=> {:a 1}

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

;; preds
;; as for collections, preds return given value for success or nil

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

;; strs syms and keywords

(is (+ 'foo "bar") 'foobar)
(is (+ :foo 'bar) :foobar)
(is (+ "foo" 'bar :baz) "foobar:baz")

;; on function it do composition
;(left to right, not like core.comp do)
(is ((+ inc inc (p mul 2)) 0)
    4)

;; sip add one or several element into something
;; ---------------------------------------------

(is (sip [] 1 2)
    [1 2])

;; for lists it adds at the end (not like conj do)
(is (sip '(1 2) 3)
    '(1 2 3))

(is (sip #{3 4} 1 2)
    #{1 2 3 4})

;; for maps it works on entries
(is (sip {:a 1} [:b 2] [:c 3])
    {:a 1 :b 2 :c 3})

;; for function it partially apply given args
(is ((sip add 1 2) 3)
    6)

;; pure return the empty version of the given argument
;; ---------------------------------------------------

(is (pure "foobar") "")

(is (pure {:a 1 :b 2}) {})

(is (pure inc) id)

;; pure? test for purity
;; ---------------------

(is {} (pure? {}))

(nil? (!! (pure? {:a 1})))

















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
