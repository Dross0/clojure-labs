(defn next-integral-sum-element [f, a, step] 
  (* (/ (+ (f a) (f (+ a step))) 2) step)
)

(defn integral [f step]
  (fn [b]
    (reduce + 
      (map (fn [a_i] (next-integral-sum-element f a_i step))
        (range 0 b step)
      )
    )
  )
)

(defn partial-solutions-sequence [f a step]
  (lazy-seq (cons (next-integral-sum-element f a step) (partial-solutions-sequence f (+ a step) step)))
)

(defn integral-with-partial-solution-sequence [f step]
  (fn [b]
    (reduce + (take (/ b step) (partial-solutions-sequence f 0 step)))
  )
)

(defn sqr [x]
  (* x x)
)

(let [
      simpleSqrIntegral (integral sqr 0.1),
      partialSolutionSqrIntegral (integral-with-partial-solution-sequence sqr 0.1)
]
  (time (simpleSqrIntegral 1000))
  (time (simpleSqrIntegral 1000))
  (time (partialSolutionSqrIntegral 1000))
  (time (partialSolutionSqrIntegral 1000))
)
