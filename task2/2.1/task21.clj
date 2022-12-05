(defn nextIntegralSumElement [f, a, step] 
  (* (/ (+ (f a) (f (+ a step))) 2) step)
)

(defn integrate [f, a, b, step]
  (reduce + 
    (map (fn [a_i] (nextIntegralSumElement f a_i step))
      (range a b step)
    )
  )
)

(def integrateWithMemoize
  (memoize 
   (fn [f, a, b, step] 
     (if (>= a b)
      0
      (+ (nextIntegralSumElement f (- b step) step) (integrateWithMemoize f a (- b step) step))
     )
   )
  )
)

(defn sqr [x]
  (* x x)
)

(time (integrate sqr 0 1000 6))
(time (integrate sqr 0 1000 6))
(time (integrateWithMemoize sqr 0 1000 6))
(time (integrateWithMemoize sqr 0 1000 6))