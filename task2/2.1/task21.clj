(defn nextIntegralSumElement [f, a, step] 
  (* (/ (+ (f a) (f (+ a step))) 2) step)
)

(defn integral [f]
  (fn [b]
    (reduce + 
      (map (fn [a_i] (nextIntegralSumElement f a_i 0.1))
        (range 0 b 0.1)
      )
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

(defn integralWithMemoize [f]  
  (fn [b]
    (integrateWithMemoize f 0 b 0.1)
  )
)


(defn sqr [x]
  (* x x)
)

(let [
      simpleSqrIntegral (integral sqr),
      memoizeSqrIntegral (integralWithMemoize sqr)
]
  (time (simpleSqrIntegral 10))
  (time (simpleSqrIntegral 10))
  (time (memoizeSqrIntegral 10))
  (time (memoizeSqrIntegral 10))
)
