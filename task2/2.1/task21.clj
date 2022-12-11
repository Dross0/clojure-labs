(defn next-integral-sum-element [f, a, step] 
  (* (/ (+ (f a) (f (+ a step))) 2) step)
)

(defn integral [f, step]
  (fn [b]
    (reduce + 
      (map (fn [a_i] (next-integral-sum-element f a_i step))
        (range 0 b step)
      )
    )
  )
)

(defn integral-with-memoize [f, step]
  (with-local-vars
      [integrate-with-memoize (memoize 
               (fn [b] 
                 (reduce + 
                    (map (fn [a_i] (next-integral-sum-element f a_i step))
                      (range 0 b step)
                    )
                 )
              )
          )
      ]
    (.bindRoot integrate-with-memoize @integrate-with-memoize)
    @integrate-with-memoize)
)


(defn sqr [x]
  (* x x)
)

(let [
      simpleSqrIntegral (integral sqr 0.1),
      memoizeSqrIntegral (integral-with-memoize sqr 0.1)
]
  (print "Simple integral [FIRST RUN]: ")
  (time (simpleSqrIntegral 100000))
  (print "Simple integral [SECOND RUN]: ")
  (time (simpleSqrIntegral 100000))
  (print "Memoize integral [FIRST RUN]: ")
  (time (memoizeSqrIntegral 100000))
  (print "Memoize integral [SECOND RUN]: ")
  (time (memoizeSqrIntegral 100000))
)
