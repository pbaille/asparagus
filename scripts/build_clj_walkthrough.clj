(require '[asparagus.boot.prelude :as p])

(defn str-seq [x]
  (cond
    (nil? x) ()
    (string? x) (list x)
    (sequential? x) (mapcat str-seq x)))

(defn str+ [& xs]
  (apply str (str-seq xs)))

(defn indent [depth & xs]
  (str+ (repeat (* 2 depth) " ") xs))

(defn cat-line [{:as state :keys [code? depth result]} & xs]
  (assoc state
         :result
         (str+ result (indent depth (if code? "" "; ") xs))))

(defn cat-section [state line]
  (let [[_ stars title] (re-find #"^(\*+) *(.*)\n" line)
        depth (count stars)
        sstack (:section-stack state)
        taken (take-while (partial <= depth) sstack)
        closing  (repeat (count taken)  ")")
        closing (when (seq closing) (str+ closing "\n\n"))
        sstack-next (cons depth (drop-while (partial <= depth) sstack))]
    (assoc state
           :depth depth
           :section-stack sstack-next
           :result (str+ (:result state)
                         closing
                         (indent (dec depth) "(do \"" title "\"\n")))))

(defn close-all [state]
  (update state
          :result str+
          (repeat (count (:section-stack state)) ")")))

(defn remove-toc [state]
  (update state
          :result
          #(->> % (re-seq #".*\n") (next)
                (drop-while (fn [l] (not (re-find #"^\(do" l))))
                str+)))

(->> (slurp "tutorial.org")
     (re-seq #".*\n")
     (reduce (fn [{:as state :keys [code? depth result]} line]
               (cond
                 (re-find #"^\*+" line) (cat-section state line)
                 (re-find #"#\+begin_src clojure*" line) (assoc state :code? true)
                 (re-find #"#\+end_src" line) (assoc state :code? false)
                 (re-find #"^ *\n" line) (assoc state :result (str result "\n"))
                 :else (cat-line state line)))
             {:code? false
              :depth 1
              :result ""})
     close-all
     remove-toc
     :result
     (spit "src/asparagus/tutorial.clj"))

