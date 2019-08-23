(in-ns 'asparagus.core)

(!! (env.exp @E '(let [x 1] (lst (add . (range 10)) (:yop x)))))
(!! (env.exp @E
             '(lst (add . (range 10)) (:yop x . op ~(add 1 2)))
             {:composite false
              ;;:method-calls false
              ;;:strict false
              }))

(!! (env.expand.top-lvl-unquotes @E (qualify @E '(_.primitives.fn [_] nil))))
