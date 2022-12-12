(defn next-integral-sum-element [f, a, step] 
  (* (/ (+ (f a) (f (+ a step))) 2) step)
)

(defn integrate [f, b, step]
  (if (<= b 0)
      0
      (+ (next-integral-sum-element f (- b step) step) (integrate f (- b step) step))
  )
)

(defn integral [f, step]
  (fn [b]
    (integrate f b step)
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
      simpleSqrIntegral (integral sqr 1),
      partialSolutionSqrIntegral (integral-with-partial-solution-sequence sqr 1)
]
  (print "Simple integral [FIRST RUN]: ")
  (time (simpleSqrIntegral 100))
  (print "Simple integral [SECOND RUN]: ")
  (time (simpleSqrIntegral 99))
  (print "Patrial solution integral [FIRST RUN]: ")
  (time (partialSolutionSqrIntegral 100))
  (print "Patrial solution integral [SECOND RUN]: ")
  (time (partialSolutionSqrIntegral 99))
)
