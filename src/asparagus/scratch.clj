(in-ns 'asparagus.core)

(E+ env
    {
     get
     [
      "getting things inside an environment"
      (fn [e s]
        (second (env-absfind e (path s))))

      val (fn [e s] (env.get e (sym s :val)))
      mac (fn [e s] (env.get e (sym s :mac)))
      sub (fn [e s] (env.get e (sym s :sub)))]

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

      strict
      (fn [e x]
        (qualify
         (env.put.val
          e 'env.qualify.fail
          #(error "not resolvable: " %))
         x))]

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
                  id))]
          (c/comp
           (get-expanding-step :composite)
           (get-expanding-step :method-calls)
           (p env.expand.raw e)
           (get-expanding-step :top-lvl-unquotes))))

      raw
      [
       "takes an environment e and an expression x
        will handle substitutions and macro calls"
       (fn [e x]
         (cp x
             ;; subsitution
             subpath? (expand-subpath e x)
             ;; macro call
             mcall? (expand-mcall e x)
             ;; clojure collection
             holycoll? ($ x (p expand e))
             ;; anything else
             x))]

      :flags
      {:composite true
       :top-lvl-unquotes true
       :method-calls true}

      ;; impls

      composite
      (fn [_ x] (composite.expand x))

      top-lvl-unquotes
      (fn

        ([e x] (rec e 0 x))

        ([e lvl x]
         (cp x

             (p quotes.quote? e)
             (rec e (inc lvl) (cadr x))

             unquote?
             (cs (zero? lvl)
                 (c/eval (res e (exp e (cadr x))))
                 (rec e (dec lvl) (cadr x)))

             holycoll?
             ($ x (p rec e lvl))

             x)))

      method-calls
      [
       "this compilation step will insert § in front of sexpr starting with a litteral vec or map
        and will handle object oriented syntax (sexpr starting with a keyword) like the janet language do"

       (fn [e x]
          (cp x
              seq?
              (cs [x1 (car x)
                   ? (key? x1)
                   [o . _xs] (cdr x)
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
     [
      "qualify and expand x with e"
      (fn [e x & [opts]]
        (let [e
              (env.upd e
                       'env.expand:flags (fn [v] (c/merge v opts))
                       'env.qualify:strict #(c/get opts :strict %))]
          (env.expand e (env.qualify e x))))]})

(!! (env.exp @E '(let [x 1] (lst (add . (range 10)) (:yop x)))))
(!! (env.exp @E
             '(lst (add . (range 10)) (:yop x . op ~(add 1 2)))
             {:composite false
              ;;:method-calls false
              ;;:strict false
              }))

