(ns clojure-miniproject.core
  (:gen-class))

(def p1 '(and x (or x (and y (not z)))))
(def b1 '{x false, z true})


(defn simplify-and
  "Simplifies a given and statement."
  [s]
  (let [filtered (distinct (filter #(not= % true) s))] ;; (and x x true y) -> (and x y)
    (cond (some #(= % false) filtered)
          'false ;; (and x false) -> false
          (= (count filtered) 2)
          (second filtered) ;; (and x) -> (x)
          (= (count filtered) 1)
          'true ;; (and true true) -> true 
          :else filtered) ;; (and x y) -> (and x y)
    )
)

(defn simplify-or
  "Simplifies a given or statement."
  [s]
  (let [filtered (distinct (filter #(not= % false) s))] ;; (or x x false y) -> (or x y)
    (cond (some #(= % true) filtered)
        'true ;; (or x true) -> true
      (= (count filtered) 2)
        (second filtered) ;; (or x) -> (x)
      (= (count filtered) 1) 
        'false ;; (or false false) -> false 
      :else filtered) ;; (or x y) -> (or x y)
  )
)

(defn simplify-not
  "Simplifies a given not statement."
  [s]
    (cond (= (second s) true) 'false ;; (not true) -> false
    (= (second s) false) 'true ;; (not false) -> true
    
    (not (coll? (second s))) 
         s ;; (not x) -> (not x)
    
    (= (first (second s)) 'not)
         (second (second s)) ;; (not (not x)) -> x
          
    (= (first (second s)) 'and) ;; demorgan's
         (cons 'or (map #(cons 'not (list %)) (drop 1 (second s))))
    (= (first (second s)) 'or) ;; demorgan's
         (cons 'and (map #(cons 'not (list %)) (drop 1 (second s))))
    
    :else (cons 'not (second s)) ;; (not (x)) -> (not x)
    ) 
)

(defn simplify
  "simplifies the given logical statement recursively."
  [exp]
  
  (let [todo (map #(if (coll? %) (simplify %) %) exp)] 
    
    (cond (= (first todo) 'and)
        (simplify-and todo)
      (= (first todo) 'or)
        (simplify-or todo)
      (= (first todo) 'not)
        (simplify-not todo)
      :else (first todo) 
    )
  )
)

(defn bind-values
  "Replaces values in list l with bindings in map m."
  [m l]
  (map #(if (coll? %)
          (bind-values m %)
          (get m % %)) l)
)

(defn evalexp
  "Binds, simplifies, and evaluates a logical expression."
  [bindings exp]
  (simplify (bind-values bindings exp))
)

(defn -main
  [& args] args 
  (evalexp b1 p1) ;; example call using example logic and bindings above 
)
