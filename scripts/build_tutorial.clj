(spit "tutorial.org"
      (-> (slurp "src/asparagus/tutorial.org")
          (clojure.string/replace #"\[=" "[ =")
          (clojure.string/replace #"=\]" "= ]")))
