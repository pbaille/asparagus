(ns asparagus.boot.state
  (:refer-clojure :exclude [reset!]))

(def state0 {:fns {} :types {} :guards {}})

(def state (atom state0))

(defn reset! [] (clojure.core/reset! state state0))
