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

(defn integral-with-memoize [f, step]
  (let [integrateFunc (fn [selfFunc b]
                 (if (<= b 0)
                   0
                   (+ (next-integral-sum-element f (- b step) step) (selfFunc selfFunc (- b step)))
                ))
        ]
    (partial integrateFunc (memoize integrateFunc))
  )
)

(defn sqr [x]
  (* x x)
)

(let [
      simpleSqrIntegral (integral sqr 1),
      memoizeSqrIntegral (integral-with-memoize sqr 1)
]
  (print "Simple integral [FIRST RUN]: ")
  (time (simpleSqrIntegral 100))
  (print "Simple integral [SECOND RUN]: ")
  (time (simpleSqrIntegral 101))
  (print "Simple integral [THIRD RUN]: ")
  (time (simpleSqrIntegral 99))
  (print "Memoize integral [FIRST RUN]: ")
  (time (memoizeSqrIntegral 100))
  (print "Memoize integral [SECOND RUN]: ")
  (time (memoizeSqrIntegral 101))
  (print "Memoize integral [THIRD RUN]: ")
  (time (memoizeSqrIntegral 99))
)
