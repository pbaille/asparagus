(spit "tutorial.org"
      (-> (slurp "tutorial.org")
          (clojure.string/replace #"\[=" "[ =")
          (clojure.string/replace #"=\]" "= ]")))
