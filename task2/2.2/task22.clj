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

(defn partialSolutionsSequence [f a step]
  (lazy-seq (cons (nextIntegralSumElement f a step) (partialSolutionsSequence f (+ a step) step)))
)

(defn integralWithParialSolutionSequence [f]
  (fn [b]
    (reduce + (take (/ b 0.1) (partialSolutionsSequence f 0 0.1)))
  )
)

(defn sqr [x]
  (* x x)
)

(let [
      simpleSqrIntegral (integral sqr),
      partialSolutionSqrIntegral (integralWithParialSolutionSequence sqr)
]
  (time (simpleSqrIntegral 10))
  (time (simpleSqrIntegral 10))
  (time (partialSolutionSqrIntegral 10))
  (time (partialSolutionSqrIntegral 10))
)
