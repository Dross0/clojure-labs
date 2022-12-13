(defn next-integral-sum-element [f, a, step] 
  (* (/ (+ (f a) (f (+ a step))) 2) step)
)

(defn sum-elements [f step stepsSeq]
  (map 
     (fn [a_i] (next-integral-sum-element f a_i step)) 
     stepsSeq
  )
)

(defn integral [f step]
  (fn [b]
    (reduce + (sum-elements f step (range 0 b step)))
  )
)


(defn integral-with-partial-solution-sequence [f step]
  (let [
        solutions (reductions + 0 (sum-elements f step (iterate (partial + step) 0)))
      ]
    (fn [b]
      (nth solutions (int (/ b step)))
    )
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
  (time (simpleSqrIntegral 100000))
  (print "Simple integral [SECOND RUN]: ")
  (time (simpleSqrIntegral 99999))
  (print "Simple integral [THIRD RUN]: ")
  (time (simpleSqrIntegral 100001))
  (print "Patrial solution integral [FIRST RUN]: ")
  (time (partialSolutionSqrIntegral 100000))
  (print "Patrial solution integral [SECOND RUN]: ")
  (time (partialSolutionSqrIntegral 99999))
  (print "Patrial solution integral [THIRD RUN]: ")
  (time (partialSolutionSqrIntegral 100001))
)
